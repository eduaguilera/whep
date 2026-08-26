# Gate on the provenance of the GLEAM coefficient tables (#607).
#
# `R CMD check --as-cran` reported a persistent NOTE because the four
# `gleam_*` topics carrying a `\doi{}` cited 10.1088/1748-9326/aad4d8, a DOI
# that is not registered at all (doi.org 404, Crossref "Resource not found").
# The tables are in fact parsed from the FAO GLEAM 3.0 Supplement S1 workbook
# committed under data-raw/, which carries no DOI, so the citation is by title
# and URL instead.
#
# These checks read the checkout, not the installed package: man/ and data-raw/
# are absent from an installed package (and data-raw/ from a built tarball, it
# being .Rbuildignored), so they skip there. The `offline-tests` job runs
# `devtools::test()` on the checkout, where both are present, so the gate runs
# on every push and pull request -- the same arrangement as
# test_data_raw_freshness.R.

.checkout_root <- function() {
  root <- testthat::test_path("..", "..")
  if (!dir.exists(file.path(root, "man"))) {
    return(NULL)
  }
  root
}

# The DOI that was never registered, in the two forms that make it a citation
# rather than a mention: `\doi{}` markup, which is what `--as-cran` extracts
# from an .Rd file, and a doi.org URL. The provenance comment at the top of
# R/livestock_coefs.R names the string deliberately, and must not trip this.
.unregistered_doi_citations <- function() {
  c(
    "\\doi{10.1088/1748-9326/aad4d8}",
    "doi.org/10.1088/1748-9326/aad4d8"
  )
}

.gleam_workbook_url <- function() {
  paste0(
    "https://www.fao.org/fileadmin/user_upload/gleam/docs/",
    "GLEAM_3.0_Supplement_S1.xlsx"
  )
}

test_that("no documentation cites the unregistered GLEAM DOI", {
  root <- .checkout_root()
  testthat::skip_if(is.null(root), "man/ absent (not a source checkout)")

  files <- c(
    list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
    list.files(file.path(root, "man"), pattern = "\\.Rd$", full.names = TRUE)
  )
  cited <- function(f) {
    lines <- readLines(f, warn = FALSE)
    .unregistered_doi_citations() |>
      purrr::some(\(doi) any(stringr::str_detect(lines, stringr::fixed(doi))))
  }
  offenders <- files |>
    purrr::keep(cited) |>
    basename()

  expect_equal(offenders, character(0))
})

test_that("the workbook-derived GLEAM topics cite the FAO workbook", {
  root <- .checkout_root()
  testthat::skip_if(is.null(root), "man/ absent (not a source checkout)")

  # Every table that the builder script parses out of the Supplement S1
  # workbook.
  topics <- c(
    "gleam_crop_residue_params",
    "gleam_crop_residue_nitrogen",
    "gleam_dressing_percentages",
    "gleam_energy_use_ef",
    "gleam_feed_composition",
    "gleam_feed_conversion_ratios",
    "gleam_feed_digestibility",
    "gleam_field_operation_ef",
    "gleam_fracremove",
    "gleam_geographic_hierarchy",
    "gleam_mechanization_levels",
    "gleam_processing_transport_ef"
  )

  cites <- topics |>
    purrr::set_names() |>
    purrr::map_lgl(\(topic) {
      rd <- file.path(root, "man", paste0(topic, ".Rd"))
      if (!file.exists(rd)) {
        return(FALSE)
      }
      any(stringr::str_detect(
        readLines(rd, warn = FALSE),
        stringr::fixed(.gleam_workbook_url())
      ))
    })

  expect_true(
    all(cites),
    info = paste(
      "topics not citing the workbook:",
      paste(names(cites)[!cites], collapse = ", ")
    )
  )
})

test_that("the committed GLEAM workbook is the FAO original", {
  root <- .checkout_root()
  testthat::skip_if(is.null(root), "man/ absent (not a source checkout)")
  workbook <- file.path(root, "data-raw", "GLEAM_3.0_Supplement_S1.xlsx")
  testthat::skip_if_not(file.exists(workbook), "data-raw/ absent")

  # md5 of the file downloaded from .gleam_workbook_url() on 2026-08-25; the
  # citation added in #607 is only true of this exact workbook.
  expect_equal(
    unname(tools::md5sum(workbook)),
    "207e3e928c176b2189e520bddcb0c5f6"
  )
})
