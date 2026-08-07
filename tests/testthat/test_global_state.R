# Package-wide guard, not a per-script test file: CRAN Repository Policy
# requires a function to restore any global state it touches -- the search
# path, the working directory, `options()`, the RNG seed, the locale and
# environment variables. The checks below walk every function in whep's
# namespace, so they hold under `R CMD check` (installed package) exactly as
# under `devtools::test()`, and no source file needs to be read.

# Calls that mutate state the caller cannot get back, and for which this
# package has no legitimate use. `library()`/`require()`/`attach()` pollute the
# search path (a Depends/Imports entry plus a `pkg::` prefix is the fix) and
# `detach()` only ever runs when nothing upstream of it failed.
.banned_global_state_calls <- function() {
  c(
    "library",
    "require",
    "attach",
    "detach",
    "setwd",
    "Sys.setenv",
    "Sys.setlocale",
    "install.packages",
    "sink",
    "par"
  )
}

# Calls that make a `set.seed()` local to its own frame instead of leaving the
# caller's stream reseeded.
.seed_restoring_calls <- function() {
  c("with_preserve_seed", "local_preserve_seed", "with_seed")
}

# Calls that put `options()` back the way they were found.
.option_restoring_calls <- function() {
  c("on.exit", "local_options", "with_options")
}

# Names of the functions called anywhere inside `expr`. A `pkg::fun()` call
# contributes `fun`, so namespace-prefixing a banned call cannot hide it.
.called_fun_names <- function(expr) {
  if (!is.call(expr)) {
    return(character())
  }
  if (rlang::is_call(expr, c("::", ":::"))) {
    return(as.character(expr[[3]]))
  }
  head_name <- if (is.name(expr[[1]])) as.character(expr[[1]]) else character()
  nested <- purrr::map(as.list(expr), .called_fun_names)
  c(head_name, unlist(nested, use.names = FALSE))
}

.fun_call_names <- function(fun) {
  defaults <- purrr::map(as.list(formals(fun)), .called_fun_names)
  unique(c(
    .called_fun_names(body(fun)),
    unlist(defaults, use.names = FALSE)
  ))
}

# Every function whep defines, mapped to the names it calls.
.whep_fun_calls <- function() {
  namespace <- asNamespace("whep")
  object_names <- ls(namespace, all.names = TRUE)
  object_names |>
    purrr::set_names() |>
    purrr::map(function(nm) get(nm, envir = namespace)) |>
    purrr::keep(is.function) |>
    purrr::map(.fun_call_names)
}

# Renders offenders as "fun() calls x, y" so a failure names them.
.describe_offenders <- function(offenders) {
  offenders |>
    purrr::imap_chr(function(hits, nm) {
      paste0(nm, "() calls ", paste(sort(hits), collapse = ", "))
    }) |>
    as.character()
}

testthat::test_that("no function mutates unrestorable global state", {
  offenders <- .whep_fun_calls() |>
    purrr::map(function(calls) {
      intersect(calls, .banned_global_state_calls())
    }) |>
    purrr::keep(function(hits) length(hits) > 0)

  testthat::expect_equal(.describe_offenders(offenders), character())
})

testthat::test_that("set.seed() is scoped so the caller's RNG survives", {
  # Deliberately per-function: a helper that seeds while its caller does the
  # restoring is flagged, because the helper is then unsafe to reuse.
  unscoped <- .whep_fun_calls() |>
    purrr::keep(function(calls) "set.seed" %in% calls) |>
    purrr::keep(function(calls) !any(.seed_restoring_calls() %in% calls))

  testthat::expect_equal(as.character(names(unscoped)), character())
})

testthat::test_that("options() are put back by whoever changed them", {
  unrestored <- .whep_fun_calls() |>
    purrr::keep(function(calls) "options" %in% calls) |>
    purrr::keep(function(calls) !any(.option_restoring_calls() %in% calls))

  testthat::expect_equal(as.character(names(unrestored)), character())
})

testthat::test_that("the scanner sees through namespace prefixes", {
  # Guards the guard: if `.called_fun_names()` stopped unwrapping `::`, every
  # check above would pass vacuously on namespaced offenders.
  probe <- function() {
    base::setwd("/tmp")
    utils::install.packages("x")
  }

  testthat::expect_setequal(
    intersect(.fun_call_names(probe), .banned_global_state_calls()),
    c("setwd", "install.packages")
  )
})
