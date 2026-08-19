.lazydata_fixture <- function() {
  lazydata <- new.env(parent = emptyenv())
  delayedAssign(
    "polities_stub",
    tibble::tibble(polity_code = 1L),
    assign.env = lazydata
  )
  assign("shadowing_stub", "from_lazydata", envir = lazydata)
  lazydata
}

test_that(".bind_lazydata makes datasets visible by bare name", {
  target <- new.env(parent = emptyenv())
  bound <- whep:::.bind_lazydata(.lazydata_fixture(), target)

  testthat::expect_setequal(bound, c("polities_stub", "shadowing_stub"))
  testthat::expect_true(exists(
    "polities_stub",
    envir = target,
    inherits = FALSE
  ))
  get("polities_stub", envir = target) |>
    nrow() |>
    testthat::expect_equal(1L)
})

test_that(".bind_lazydata keeps the new bindings lazy", {
  target <- new.env(parent = emptyenv())
  whep:::.bind_lazydata(.lazydata_fixture(), target)

  rlang::env_binding_are_lazy(target, "polities_stub") |>
    unname() |>
    testthat::expect_true()
})

test_that(".bind_lazydata never shadows objects of the target", {
  target <- new.env(parent = emptyenv())
  assign("shadowing_stub", "from_target", envir = target)
  bound <- whep:::.bind_lazydata(.lazydata_fixture(), target)

  testthat::expect_equal(bound, "polities_stub")
  get("shadowing_stub", envir = target) |>
    testthat::expect_equal("from_target")
})

test_that(".bind_lazydata is a no-op without a lazydata environment", {
  target <- new.env(parent = emptyenv())

  whep:::.bind_lazydata(NULL, target) |>
    testthat::expect_equal(character())
  testthat::expect_equal(ls(target), character())
})

test_that("every shipped dataset is reachable from the namespace", {
  ns <- asNamespace("whep")
  dataset_names <- ls(ns$.__NAMESPACE__.$lazydata, all.names = TRUE)
  testthat::skip_if(length(dataset_names) == 0)

  missing <- dataset_names[
    !purrr::map_lgl(dataset_names, \(name) {
      exists(name, envir = ns, inherits = FALSE)
    })
  ]
  testthat::expect_equal(missing, character())
})
