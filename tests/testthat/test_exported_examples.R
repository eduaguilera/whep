# Every exported function must carry an \examples block (whep#189).
#
# R CMD check only runs the examples that exist, so an export documented
# without \examples is executed by no gate at all -- which is how the
# always-erroring build_supply_use() of #176 stayed green through five
# platforms. This file is that missing gate: it audits the Rd of every export
# rather than any one function, so a new export cannot arrive unexercised.
#
# The Rd comes from whichever of the two trees this runs in. testthat runs
# every test file with the working directory set to tests/testthat, so
# ../../man is the source package's man/ under devtools::test() and covr, and
# does not exist under R CMD check, which runs the tests inside
# <pkg>.Rcheck/tests against an installed copy -- there the installed Rd
# database is what there is. Both parse into the same Rd structure, so one
# audit covers both. (A plain relative path rather than testthat::test_path(),
# which aborts instead of resolving when it cannot find a tests/testthat above
# the working directory.)

.examples_audit_rd_list <- function() {
  man_dir <- file.path("..", "..", "man")
  if (dir.exists(man_dir)) {
    files <- list.files(man_dir, pattern = "[.]Rd$", full.names = TRUE)
    return(lapply(files, tools::parse_Rd))
  }
  unname(as.list(tools::Rd_db("whep")))
}

.examples_audit_rd_tags <- function(rd) {
  vapply(
    rd,
    function(section) as.character(attr(section, "Rd_tag"))[1],
    character(1)
  )
}

.examples_audit_rd_aliases <- function(rd) {
  aliases <- rd[.examples_audit_rd_tags(rd) == "\\alias"]
  unname(vapply(
    aliases,
    function(alias) trimws(paste(unlist(alias), collapse = "")),
    character(1)
  ))
}

.examples_audit_rd_entry <- function(rd) {
  aliases <- .examples_audit_rd_aliases(rd)
  has_examples <- "\\examples" %in% .examples_audit_rd_tags(rd)
  stats::setNames(rep(has_examples, length(aliases)), aliases)
}

# Named logical, one entry per documented alias: does its topic have examples?
.examples_audit_index <- function() {
  unlist(lapply(.examples_audit_rd_list(), .examples_audit_rd_entry))
}

.examples_audit_exports <- function() {
  exports <- sort(getNamespaceExports("whep"))
  is_function <- vapply(
    exports,
    function(name) is.function(getExportedValue("whep", name)),
    logical(1)
  )
  exports[is_function]
}

.examples_audit_parse_rd <- function(lines, dir) {
  path <- tempfile(tmpdir = dir, fileext = ".Rd")
  writeLines(lines, path)
  tools::parse_Rd(path)
}

testthat::test_that("the examples audit reads a populated Rd index", {
  # A silent failure to find any Rd would make the audit below pass
  # vacuously, so assert it saw the package before trusting what it says.
  index <- .examples_audit_index()

  testthat::expect_gt(length(index), 200L)
  testthat::expect_true(any(index))
  testthat::expect_gt(length(.examples_audit_exports()), 150L)
})

testthat::test_that("the examples detector separates present from absent", {
  dir <- withr::local_tempdir()
  base <- c("\\name{toy}", "\\alias{toy}", "\\title{Toy}")

  with_examples <- .examples_audit_parse_rd(
    c(base, "\\examples{1 + 1}"),
    dir
  )
  without_examples <- .examples_audit_parse_rd(base, dir)

  testthat::expect_true(
    "\\examples" %in% .examples_audit_rd_tags(with_examples)
  )
  testthat::expect_false(
    "\\examples" %in% .examples_audit_rd_tags(without_examples)
  )
  testthat::expect_equal(.examples_audit_rd_aliases(with_examples), "toy")
  testthat::expect_equal(
    .examples_audit_rd_entry(without_examples),
    c(toy = FALSE)
  )
})

testthat::test_that("every exported function is documented", {
  undocumented <- setdiff(
    .examples_audit_exports(),
    names(.examples_audit_index())
  )

  testthat::expect_equal(undocumented, character(0))
})

testthat::test_that("every exported function has an @examples block", {
  index <- .examples_audit_index()
  exports <- intersect(.examples_audit_exports(), names(index))

  without_examples <- exports[!index[exports]]

  testthat::expect_equal(without_examples, character(0))
})
