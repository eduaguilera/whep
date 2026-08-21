# Gate against a stale data/*.rda (#384).
#
# The tables under data/ are committed build products of the data-raw/ scripts,
# and nothing used to assert that they still match what their builder emits
# from the current inputs. An edited inst/extdata CSV that was never followed by
# `Rscript data-raw/<builder>.R` therefore shipped a table disagreeing with its
# own source, indistinguishable from a fresh one until someone happened to
# re-run the builder. That is how regions_full and polities_cats came to resolve
# eight areas to polities upstream had retired or superseded (#382).
#
# This gate re-runs every builder whose inputs live inside the repo, in a
# private environment with the `usethis::use_data()` calls removed so nothing is
# written, and compares each rebuilt object with the committed .rda by content.
# The comparison has to be on content: `save()` output is not byte-reproducible,
# and re-running one builder rewrites several unrelated .rda files with
# identical content, so a byte or file-hash comparison would be pure noise.
#
# Coverage is partial by construction, and the partition is asserted below so a
# new dataset cannot arrive both unchecked and unexcluded:
#
#   * checked  -- the 49 datasets written by the seven builders in
#     `.offline_data_builders()`, which read only inst/extdata/, data-raw/ and
#     committed data/*.rda.
#   * excluded -- the 7 datasets in `.externally_built_datasets()`. Five come
#     from table_mappings.R, which cannot run past `sf::st_read()` on the
#     whep-polities GeoPackage (`WHEP_POLITIES_GPKG`) -- those five get the
#     opportunistic check at the end of this file instead; it does read
#     items_cbs and items_prod verbatim from CSVs before that point, but only
#     the whole script is a builder. coello_synthetic_n.R reads an off-repo
#     >50 MB CSV (`WHEP_COELLO_DIR`). livestock_coefficients.R needs
#     `openxlsx`, which the package does not even declare, and writes with
#     `save()` from inside `main()` rather than through `use_data()`, so this
#     mechanism does not reach it at all.
#
# R/sysdata.rda (one constant, from data-raw/constants.R) is internal data
# rather than a data/*.rda, and is out of scope here.
#
# What the gate proves is that each table matches its builder's output from the
# inputs currently in the repo. It does not prove those inputs are current
# against their upstream source. Two things narrow that hole:
# test_polity_output_coverage.R fails if regions_full or polities_cats stops
# resolving an area to a reporting polity with a polygon, and the last block of
# this file re-runs table_mappings.R itself wherever the whep-polities checkout
# happens to be present, skipping where it is not (#835).
#
# The gate skips where data-raw/ is absent, which is every check of a built
# tarball -- `^data-raw$` is in .Rbuildignore. The `offline-tests` job runs
# `devtools::test()` on the checkout, where data-raw/ is present, so the gate
# runs there on every push and pull request.

# Registry ---------------------------------------------------------------

# Builders whose every input lives inside the repo, so re-running them needs no
# network, no pin and no WHEP_* path.
.offline_data_builders <- function() {
  c(
    "balance_coefficients.R",
    "cft_mapping.R",
    "feed_coefficients.R",
    "harmonization_tables.R",
    "nitrogen_refs.R",
    "sjos_n_coefficients.R",
    "whep_inputs.R"
  )
}

# The data/*.rda this gate cannot rebuild, with what blocks each one. Asserted
# below to be exactly the complement of what the offline builders write, so
# adding a dataset -- or dropping an external dependency -- forces an update
# here instead of silently shrinking the gate.
.externally_built_datasets <- function() {
  tibble::tribble(
    ~dataset,                ~builder,                   ~blocked_by,
    "items_cbs",             "table_mappings.R",         "WHEP_POLITIES_GPKG",
    "items_prod",            "table_mappings.R",         "WHEP_POLITIES_GPKG",
    "polities",              "table_mappings.R",         "WHEP_POLITIES_GPKG",
    "polity_area_crosswalk", "table_mappings.R",         "WHEP_POLITIES_GPKG",
    "polity_label_aliases",  "table_mappings.R",         "WHEP_POLITIES_GPKG",
    "coello_synthetic_n",    "coello_synthetic_n.R",     "WHEP_COELLO_DIR",
    "livestock_coefs",       "livestock_coefficients.R", "openxlsx"
  )
}

# Mechanics --------------------------------------------------------------

# The builders resolve their inputs with here::here(), so the gate resolves the
# repo root the same way rather than from the test path: two different answers
# would compare one checkout's .rda against another's CSVs. here::here() aborts
# when no project root sits above the working directory, which is the normal
# case when the tests run from a built tarball.
.data_gate_root <- function() {
  root <- tryCatch(here::here(), error = function(cnd) NA_character_)
  if (is.na(root) || !dir.exists(file.path(root, "data-raw"))) {
    return(NA_character_)
  }
  root
}

.skip_without_data_raw <- function() {
  testthat::skip_if_not_installed("here")
  root <- .data_gate_root()
  if (is.na(root)) {
    testthat::skip("data-raw/ is not in the built package (.Rbuildignore)")
  }
  root
}

.is_use_data_call <- function(expr) {
  is.call(expr) && identical(rlang::expr_text(expr[[1]]), "usethis::use_data")
}

# The saved objects are the unnamed arguments; `overwrite`, `compress` and
# `internal` are named.
.use_data_object_names <- function(expr) {
  args <- rlang::call_args(expr)
  purrr::map_chr(args[rlang::names2(args) == ""], rlang::as_name) |>
    unname()
}

.builder_object_names <- function(builder, root) {
  exprs <- as.list(parse(file.path(root, "data-raw", builder)))
  exprs[purrr::map_lgl(exprs, .is_use_data_call)] |>
    purrr::map(.use_data_object_names) |>
    purrr::list_c()
}

# Runs the builder with its `use_data()` calls dropped, so the committed .rda
# files are read but never rewritten, and returns what it would have saved.
.rebuild_data_objects <- function(builder, root) {
  exprs <- as.list(parse(file.path(root, "data-raw", builder)))
  code <- exprs[!purrr::map_lgl(exprs, .is_use_data_call)]
  env <- new.env(parent = globalenv())
  withr::local_dir(root)
  suppressMessages(suppressWarnings(purrr::walk(code, eval, envir = env)))
  rlang::env_get_list(env, .builder_object_names(builder, root))
}

.committed_data_object <- function(dataset, root) {
  env <- new.env()
  load(file.path(root, "data", paste0(dataset, ".rda")), envir = env)
  rlang::env_get(env, dataset)
}

# readr tags a freshly read tibble with a `spec` attribute, a `problems`
# external pointer and the spec_tbl_df class. None of that is data -- the
# pointer does not survive save()/load() and the spec's shape is a readr
# implementation detail -- so comparing it would make the gate fail on a readr
# upgrade instead of on stale data.
.drop_readr_bookkeeping <- function(x) {
  if (!inherits(x, "data.frame")) {
    return(x)
  }
  attr(x, "spec") <- NULL
  attr(x, "problems") <- NULL
  class(x) <- setdiff(class(x), "spec_tbl_df")
  x
}

.shipped_datasets <- function(root) {
  list.files(file.path(root, "data"), pattern = "\\.rda$") |>
    stringr::str_remove("\\.rda$")
}

# A throwaway repo -- one builder, one input CSV, one committed .rda -- standing
# in for the real thing so the gate's own mechanics can be tested both ways.
# With `stale = TRUE` the committed copy holds a value the CSV no longer
# produces, which is exactly the drift #384 is about. It lives in a temp dir
# tied to the calling test's frame, so nothing here touches this repo.
.toy_builder_repo <- function(stale) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "data-raw"))
  dir.create(file.path(root, "data"))
  writeLines(
    c(
      'toy_table <- readr::read_csv("toy.csv", show_col_types = FALSE)',
      "usethis::use_data(toy_table, overwrite = TRUE)"
    ),
    file.path(root, "data-raw", "toy.R")
  )
  readr::write_csv(tibble::tibble(value = c(1, 2)), file.path(root, "toy.csv"))
  toy_table <- tibble::tibble(value = if (stale) c(1, 99) else c(1, 2))
  save(toy_table, file = file.path(root, "data", "toy_table.rda"))
  root
}

# Tests ------------------------------------------------------------------

testthat::test_that("every data/*.rda is either rebuilt here or excluded", {
  root <- .skip_without_data_raw()
  rebuilt <- .offline_data_builders() |>
    purrr::map(\(builder) .builder_object_names(builder, root)) |>
    purrr::list_c()
  excluded <- .externally_built_datasets()$dataset

  testthat::expect_length(intersect(rebuilt, excluded), 0)
  testthat::expect_equal(
    sort(c(rebuilt, excluded)),
    sort(.shipped_datasets(root))
  )
})

purrr::walk(.offline_data_builders(), function(builder) {
  testthat::test_that(paste(builder, "reproduces the .rda it writes"), {
    root <- .skip_without_data_raw()
    rebuilt <- .rebuild_data_objects(builder, root)

    testthat::expect_gt(length(rebuilt), 0)
    purrr::iwalk(rebuilt, function(object, dataset) {
      testthat::expect_equal(
        .drop_readr_bookkeeping(object),
        .drop_readr_bookkeeping(.committed_data_object(dataset, root)),
        label = paste0("data-raw/", builder, " rebuild of ", dataset),
        expected.label = paste0("committed data/", dataset, ".rda")
      )
    })
  })
})

testthat::test_that("a data/*.rda lagging its inputs is reported", {
  root <- .toy_builder_repo(stale = TRUE)
  rebuilt <- .rebuild_data_objects("toy.R", root)

  testthat::expect_named(rebuilt, "toy_table")
  testthat::expect_failure(testthat::expect_equal(
    .drop_readr_bookkeeping(rebuilt$toy_table),
    .drop_readr_bookkeeping(.committed_data_object("toy_table", root))
  ))
  # Dropping the use_data() calls is what keeps the gate read-only: the stale
  # copy must still be stale after the rebuild.
  testthat::expect_equal(
    .committed_data_object("toy_table", root)$value,
    c(1, 99)
  )
})

testthat::test_that("a data/*.rda built from its inputs passes", {
  root <- .toy_builder_repo(stale = FALSE)
  rebuilt <- .rebuild_data_objects("toy.R", root)

  testthat::expect_equal(
    .drop_readr_bookkeeping(rebuilt$toy_table),
    .drop_readr_bookkeeping(.committed_data_object("toy_table", root))
  )
})

# Upstream-input builder -------------------------------------------------

# `table_mappings.R` is excluded from the gate above because its three inputs
# are published by `whep-polities`, not by this repo. That exclusion is what
# made #835 invisible: `polity_area_crosswalk` had been built from a superseded
# upstream revision and shipped looking exactly like a fresh one, and nothing
# ever compared the builder's output with its `.rda`.
#
# The builder IS runnable wherever that checkout exists -- which is every
# machine that can regenerate the tables in the first place -- so it is run
# there. This is deliberately a WEAKER check than the gate above: it proves
# nothing on CI, where the inputs are absent and it skips. What it buys is that
# the person who *can* re-sync finds out, on their next `devtools::test()`,
# that the shipped tables no longer match upstream.
#
# It reads no `WHEP_*` path that is not already set: `Sys.getenv()` with the
# builder's own default resolves to `~/whep-polities`, the check is
# file-existence, and absence skips. No network is involved -- the GeoPackage
# and the two CSVs are local files.
.whep_polities_input_files <- function() {
  c(
    WHEP_POLITIES_GPKG = "polities_database.gpkg",
    WHEP_POLITIES_FAOSTAT_MAP = "faostat_area_polity_map.csv",
    WHEP_POLITIES_LABEL_ALIAS_MAP = "label_alias_map.csv"
  )
}

# The same resolution `data-raw/table_mappings.R` performs: the env var if set,
# the checkout's published path otherwise.
.upstream_polity_input_paths <- function() {
  files <- .whep_polities_input_files()
  purrr::imap_chr(files, function(file, envvar) {
    Sys.getenv(
      envvar,
      unset = path.expand(
        file.path("~", "whep-polities", "data", "final", file)
      )
    )
  })
}

.skip_without_upstream <- function() {
  testthat::skip_if_not_installed("sf")
  paths <- .upstream_polity_input_paths()
  absent <- names(paths)[!file.exists(paths)]
  if (length(absent) > 0) {
    testthat::skip(paste(
      "whep-polities inputs are not on this machine:",
      toString(absent)
    ))
  }
  invisible(paths)
}

testthat::test_that("table_mappings.R matches upstream where it can be run", {
  root <- .skip_without_data_raw()
  .skip_without_upstream()
  rebuilt <- .rebuild_data_objects("table_mappings.R", root)

  # All five, not just the crosswalk: #835 was filed about
  # `polity_area_crosswalk`, and the rebuild moved `polities` and
  # `polity_label_aliases` too. One builder, one revision, one comparison.
  excluded <- .externally_built_datasets()
  testthat::expect_setequal(
    names(rebuilt),
    excluded$dataset[excluded$builder == "table_mappings.R"]
  )
  purrr::iwalk(rebuilt, function(object, dataset) {
    testthat::expect_equal(
      .drop_readr_bookkeeping(object),
      .drop_readr_bookkeeping(.committed_data_object(dataset, root)),
      label = paste0("data-raw/table_mappings.R rebuild of ", dataset),
      expected.label = paste0("committed data/", dataset, ".rda")
    )
  })
})
