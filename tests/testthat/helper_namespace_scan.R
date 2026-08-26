# Walks the call graph of every function whep defines, from the namespace
# rather than from source, so the scanners built on it hold under `R CMD check`
# (installed package, no `R/` directory) exactly as under `devtools::test()`.
# Shared by test_global_state.R and test_dplyr_deprecations.R.

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

# Functions calling any of `banned`, described.
.whep_callers_of <- function(banned) {
  .whep_fun_calls() |>
    purrr::map(function(calls) intersect(calls, banned)) |>
    purrr::keep(function(hits) length(hits) > 0) |>
    .describe_offenders()
}
