# Tests for the per-family admin-statistics pin loader (#1000, T24).
#
# Nothing here reaches the network or the pins board. Two bindings are
# stubbed, because the loader deliberately has two seams: it asks
# `whep_inputs` whether an alias is registered at all, and only then reads
# it. Stubbing only the reader would make every family report as absent,
# since the five aliases are registered by a later, deliberate step.
#
# The package's NAMESPACE is not regenerated in this wave, so the exported
# loader is reached as `whep:::read_admin_family()` rather than `whep::`.

# The reader contract these pins share with `read_admin_stats_nass()`,
# written out rather than derived from the code under test.
admin_family_contract <- function(measure = "value") {
  c(
    "source",
    "source_native_unit_id",
    "source_native_unit_name",
    "source_native_item_code",
    "source_native_item_name",
    "indicator_used",
    "quantity",
    "year",
    measure,
    "value_unit",
    "value_flag",
    "grain",
    "nuts_version",
    "source_version",
    "recorded_at"
  )
}

# Three units of one item-year, in the shape a tier-2 family ships.
admin_value_pin <- function(alias, grain = "admin1") {
  tibble::tibble(
    source = alias,
    source_native_unit_id = c("U1", "U2", "U3"),
    source_native_unit_name = c("One", "Two", "Three"),
    source_native_item_code = "27",
    source_native_item_name = "Rice",
    indicator_used = "area_harvested",
    quantity = "area",
    year = 2000L,
    value = c(500, 300, 200),
    value_unit = "ha",
    value_flag = c(NA, "zero_observed", NA),
    grain = grain,
    nuts_version = NA_character_,
    source_version = "NATIONAL_OFFICIAL:TST",
    recorded_at = "2026-09-03T00:00:00Z"
  )
}

# Two complete groups of the tier-3 family: shares, never values.
admin_share_pin <- function() {
  tibble::tibble(
    source = "admin-stats-latam",
    source_native_unit_id = c("A1", "A2", "A3", "A1", "A2", "A3"),
    source_native_unit_name = c("One", "Two", "Three", "One", "Two", "Three"),
    source_native_item_code = "661",
    source_native_item_name = "Cocoa beans",
    indicator_used = c(rep("area_harvested", 3), rep("production", 3)),
    quantity = c(rep("area", 3), rep("production", 3)),
    year = 2000L,
    share = c(0.5, 0.3, 0.2, 0.25, 0.65, 0.1),
    value_unit = c(rep("ha", 3), rep("tonnes", 3)),
    value_flag = NA_character_,
    grain = "admin1",
    nuts_version = NA_character_,
    source_version = "2026-05-21",
    recorded_at = "2026-09-03T00:00:00Z"
  )
}

admin_all_pins <- function() {
  list(
    "admin-stats-japan" = admin_value_pin("admin-stats-japan"),
    "admin-stats-spain-provinces" = admin_value_pin(
      "admin-stats-spain-provinces",
      grain = "admin2"
    ),
    "admin-stats-australia" = admin_value_pin("admin-stats-australia"),
    "admin-stats-france-livestock" = admin_value_pin(
      "admin-stats-france-livestock"
    ),
    "admin-stats-latam" = admin_share_pin()
  )
}

# Stub the board: `registered` is what `whep_inputs` would know, `pins` is
# what the board would return for each alias.
local_admin_board <- function(
  pins = admin_all_pins(),
  registered = names(pins),
  env = parent.frame()
) {
  testthat::local_mocked_bindings(
    .admin_family_registered = function(alias) alias %in% registered,
    whep_read_file = function(file_alias, type = "parquet", version = NULL) {
      pins[[file_alias]]
    },
    .package = "whep",
    .env = env
  )
}

test_that("every shipped family loads and nothing is reported absent", {
  local_admin_board()
  families <- whep:::read_admin_family()

  expect_named(
    families,
    c(whep:::.admin_family_aliases(), "not_shipped")
  )
  expect_equal(families$not_shipped, character(0))
  expect_equal(nrow(families[["admin-stats-japan"]]), 3L)
})

test_that("an omitted family is named while the others still load", {
  pins <- admin_all_pins()
  pins[["admin-stats-spain-provinces"]] <- NULL
  local_admin_board(pins)

  families <- whep:::read_admin_family()

  expect_equal(families$not_shipped, "admin-stats-spain-provinces")
  expect_false("admin-stats-spain-provinces" %in% names(families))
  expect_equal(
    setdiff(names(families), "not_shipped"),
    setdiff(whep:::.admin_family_aliases(), "admin-stats-spain-provinces")
  )
})

test_that("a registered family whose pin is empty is reported absent", {
  pins <- admin_all_pins()
  pins[["admin-stats-australia"]] <- pins[["admin-stats-australia"]][0, ]
  local_admin_board(pins)

  families <- whep:::read_admin_family()

  expect_equal(families$not_shipped, "admin-stats-australia")
})

test_that("an unregistered alias is reported without reading the board", {
  local_admin_board(
    pins = list(),
    registered = character(0)
  )
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, type = "parquet", version = NULL) {
      cli::cli_abort("The board must not be read for an unregistered alias.")
    },
    .package = "whep"
  )

  families <- whep:::read_admin_family()

  expect_equal(families$not_shipped, whep:::.admin_family_aliases())
  expect_named(families, "not_shipped")
})

test_that("a board failure propagates instead of reading as an embargo", {
  local_admin_board()
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, type = "parquet", version = NULL) {
      cli::cli_abort("The board is unreachable.")
    },
    .package = "whep"
  )

  expect_error(whep:::read_admin_family(), "board is unreachable")
})

test_that("an unknown alias aborts and names the known families", {
  local_admin_board()

  expect_error(
    whep:::read_admin_family("admin-stats-jrc"),
    "unknown admin-statistics famil"
  )
  expect_error(
    whep:::read_admin_family(c("admin-stats-japan", "admin-stats-peru")),
    "admin-stats-peru"
  )
  expect_error(whep:::read_admin_family(character(0)), "character vector")
  expect_error(whep:::read_admin_family(1L), "character vector")
})

test_that("only the requested families are read", {
  local_admin_board()

  families <- whep:::read_admin_family(
    c("admin-stats-japan", "admin-stats-japan")
  )

  expect_named(families, c("admin-stats-japan", "not_shipped"))
  expect_equal(families$not_shipped, character(0))
})

test_that("every family keeps the shared reader contract", {
  local_admin_board()
  families <- whep:::read_admin_family()

  value_families <- setdiff(
    whep:::.admin_family_aliases(),
    c("admin-stats-latam", "not_shipped")
  )
  purrr::walk(value_families, \(alias) {
    expect_named(families[[alias]], admin_family_contract("value"))
  })
  expect_named(
    families[["admin-stats-latam"]],
    admin_family_contract("share")
  )
  expect_equal(families[["admin-stats-spain-provinces"]]$grain[[1L]], "admin2")
})

test_that("a pin missing a contract column aborts naming it", {
  pins <- admin_all_pins()
  pins[["admin-stats-japan"]]$grain <- NULL
  local_admin_board(pins)

  expect_error(whep:::read_admin_family("admin-stats-japan"), "grain")
})

test_that("the LatAm family ships shares and no source values", {
  local_admin_board()
  latam <- whep:::read_admin_family("admin-stats-latam")[[
    "admin-stats-latam"
  ]]

  expect_true("share" %in% names(latam))
  expect_false("value" %in% names(latam))

  totals <- latam |>
    dplyr::summarise(
      total = sum(.data$share),
      .by = c("source_native_item_name", "indicator_used", "year")
    )
  expect_equal(totals$total, rep(1, nrow(totals)), tolerance = 1e-12)
})

test_that("a LatAm pin carrying source values aborts", {
  pins <- admin_all_pins()
  pins[["admin-stats-latam"]]$value <- c(50, 30, 20, 25, 65, 10)
  local_admin_board(pins)

  expect_error(
    whep:::read_admin_family("admin-stats-latam"),
    "derived shares only"
  )
})

test_that("a LatAm pin without its share column aborts", {
  pins <- admin_all_pins()
  pins[["admin-stats-latam"]]$share <- NULL
  local_admin_board(pins)

  expect_error(whep:::read_admin_family("admin-stats-latam"), "share")
})

test_that("the example is the documented shape and needs no board", {
  example <- whep:::read_admin_family(example = TRUE)

  expect_named(
    example,
    c("admin-stats-japan", "admin-stats-latam", "not_shipped")
  )
  expect_named(
    example[["admin-stats-japan"]],
    admin_family_contract("value")
  )
  expect_named(
    example[["admin-stats-latam"]],
    admin_family_contract("share")
  )
  expect_length(example$not_shipped, 3L)
  expect_equal(sum(example[["admin-stats-latam"]]$share), 1, tolerance = 1e-9)
  expect_equal(nrow(example[["admin-stats-japan"]]), 5L)
})
