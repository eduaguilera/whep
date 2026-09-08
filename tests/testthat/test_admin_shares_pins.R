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

test_that("the documented composition needs the documented rename", {
  # `read_admin_shares()` prescribes `resolve_admin_units()` as the way to
  # get polities, and the two ends key the identifier column under
  # different names. The rename is now stated at both ends; this pins the
  # asymmetry itself, so renaming either column re-opens the question.
  shares <- whep:::read_admin_shares(example = TRUE)$shares

  expect_true("source_native_id" %in% names(shares))
  expect_false("source_native_unit_id" %in% names(shares))
  expect_error(
    whep:::resolve_admin_units(shares, "whep-lab-japan"),
    "source_native_unit_id"
  )

  renamed <- dplyr::rename(shares, source_native_unit_id = source_native_id)
  expect_s3_class(
    whep:::resolve_admin_units(renamed, "whep-lab-japan")$rows,
    "tbl_df"
  )
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

# --- T38: the assembled admin-shares pin -------------------------------------
#
# `read_admin_shares()` reads one pin, not five, and it must report the
# families that are NOT in it rather than leaving a caller to notice the
# absence. Nothing here reaches the board: the registration seam and
# `whep_read_file()` are both stubbed, exactly as the family loader's tests
# above stub them.

# Rows in the assembled pin's own shape: the admin-shares contract, keyed
# source-natively, with `level_polity_code` still unresolved. Japan ships
# values, the Latin American family ships shares and no value.
admin_shares_pin_rows <- function() {
  tibble::tibble(
    area_code = c(110L, 110L, 19L, 19L),
    level_polity_code = NA_character_,
    level = 1L,
    item_prod_code = c(27L, 27L, 661L, 661L),
    indicator_used = "area_harvested",
    year = 2000L,
    value = c(134900, 120700, NA, NA),
    share = c(NA, NA, 0.79, 0.21),
    source = c(
      "admin-stats-japan",
      "admin-stats-japan",
      "admin-stats-latam",
      "admin-stats-latam"
    ),
    tier = c(2L, 2L, 3L, 3L),
    grain = "admin1",
    concept_break = FALSE,
    nuts_version = NA_character_,
    source_native_id = c(
      "JPN-HOKKAIDO",
      "JPN-NIIGATA",
      "BOL-LAPAZ",
      "BOL-SANTACRUZ"
    ),
    source_native_name = c("Hokkaido", "Niigata", "La Paz", "Santa Cruz"),
    source_id = c(
      "admin-stats-japan",
      "admin-stats-japan",
      "admin-stats-latam",
      "admin-stats-latam"
    ),
    source_version = c(
      "NATIONAL_OFFICIAL:JPN:MAFF",
      "NATIONAL_OFFICIAL:JPN:MAFF",
      "2026-05-21",
      "2026-05-21"
    ),
    recorded_at = "2026-09-03T06:21:54Z",
    treatment_year = "observed",
    value_flag = NA_character_
  )
}

local_admin_shares_board <- function(
  rows = admin_shares_pin_rows(),
  registered = TRUE,
  env = parent.frame()
) {
  testthat::local_mocked_bindings(
    .admin_shares_registered = function() registered,
    whep_read_file = function(file_alias, type = "parquet", version = NULL) {
      rows
    },
    .package = "whep",
    .env = env
  )
}

test_that("the assembled pin loads with its contract columns", {
  local_admin_shares_board()
  out <- whep:::read_admin_shares()

  expect_named(out, c("shares", "excluded", "not_shipped"))
  expect_named(out$shares, names(whep:::admin_shares_prototype()))
  expect_equal(nrow(out$shares), 4L)
  expect_equal(out$not_shipped, character(0))
})

test_that("the pin carries no resolved polity code", {
  # Source-native pinning is what makes a staleness warning unnecessary
  # (`R/admin_shares_polities.R`); a frozen polity code inside the pin
  # would silently reintroduce the problem, so it is refused here.
  rows <- admin_shares_pin_rows()
  rows$level_polity_code <- c("JPN-HOKKAIDO-1869-2025", NA, NA, NA)
  local_admin_shares_board(rows)

  expect_error(
    whep:::read_admin_shares(),
    class = "whep_error_admin_pin_resolved"
  )
})

test_that("a shares-only family survives the read", {
  local_admin_shares_board()
  out <- whep:::read_admin_shares()

  latam <- dplyr::filter(out$shares, source == "admin-stats-latam")
  expect_equal(nrow(latam), 2L)
  expect_true(all(is.na(latam$value)))
  expect_equal(sum(latam$share), 1, tolerance = 1e-12)
})

test_that("a pin row with neither measurement is refused", {
  rows <- admin_shares_pin_rows()
  rows$share[3] <- NA_real_
  local_admin_shares_board(rows)

  expect_error(
    whep:::read_admin_shares(),
    class = "whep_error_admin_no_measure"
  )
})

test_that("a duplicated source-native key is refused", {
  rows <- dplyr::bind_rows(
    admin_shares_pin_rows(),
    admin_shares_pin_rows()[1L, ]
  )
  local_admin_shares_board(rows)

  err <- expect_error(
    whep:::read_admin_shares(),
    class = "whep_error_schema_violation"
  )
  expect_match(conditionMessage(err), "duplicate")
})

test_that("families absent from the pin are named, not left to be noticed", {
  local_admin_shares_board()
  excluded <- whep:::read_admin_shares()$excluded

  expect_named(excluded, c("source", "reason", "detail"))
  expect_setequal(
    excluded$source,
    setdiff(
      whep:::.admin_family_aliases(),
      c("admin-stats-japan", "admin-stats-latam")
    )
  )
  expect_true(all(
    excluded$reason %in%
      c(
        "no_consent_manifest_row",
        "no_rows_in_pin"
      )
  ))
  # Every one of the five families has a consent row today, so the
  # families missing from this pin are missing for the other reason.
  expect_equal(unique(excluded$reason), "no_rows_in_pin")
})

test_that("an unregistered pin is reported, and the board is not read", {
  local_admin_shares_board(registered = FALSE)
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, type = "parquet", version = NULL) {
      cli::cli_abort("The board must not be read for an unregistered alias.")
    },
    .package = "whep"
  )

  out <- whep:::read_admin_shares()

  expect_equal(nrow(out$shares), 0L)
  expect_equal(out$not_shipped, "admin-shares")
  expect_equal(nrow(out$excluded), 5L)
  # Not "no_rows_in_pin": nothing was read, so no family was filtered and
  # the per-source drop counts that reason points at were never computed.
  expect_equal(unique(out$excluded$reason), "pin_not_read")
  expect_match(unique(out$excluded$detail), "no pin was read")
})

test_that("a registered alias that reads nothing is also pin_not_read", {
  # The live shape of the same state: the alias is on the board, the read
  # comes back empty. `not_shipped` and `excluded` have to agree.
  local_admin_shares_board(rows = whep:::admin_shares_prototype())

  out <- whep:::read_admin_shares()

  expect_equal(out$not_shipped, "admin-shares")
  expect_equal(unique(out$excluded$reason), "pin_not_read")
})

test_that("a pin that was read reports the families it dropped", {
  # The control for the two above: with rows on the contract the report is
  # about filtering, and `pin_not_read` must not appear.
  local_admin_shares_board()

  out <- whep:::read_admin_shares()

  expect_equal(out$not_shipped, character(0))
  expect_false("pin_not_read" %in% out$excluded$reason)
})

test_that("a board failure propagates instead of reading as an absence", {
  local_admin_shares_board()
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, type = "parquet", version = NULL) {
      cli::cli_abort("The board is unreachable.")
    },
    .package = "whep"
  )

  expect_error(whep:::read_admin_shares(), "board is unreachable")
})

test_that("the example is the documented shape and needs no board", {
  example <- whep:::read_admin_shares(example = TRUE)

  expect_named(example, c("shares", "excluded", "not_shipped"))
  expect_named(example$shares, names(whep:::admin_shares_prototype()))
  expect_true(any(is.na(example$shares$value)))
  expect_true(all(is.na(example$shares$level_polity_code)))
  expect_named(example$excluded, c("source", "reason", "detail"))
})

# --- T38 repair: the consent boundary, proved on the assembled pin -----------
#
# The family read path aborts when the shares-only family's pin carries a
# `value` column (`.admin_family_check_consent()`); the assembled pin
# supersedes that path and must enforce the same per-source rule, or the
# withheld Latin American source values travel with nothing to stop them.
# The gate that says which in-house sources may travel at all, and the
# report that names the families absent from the pin, are exercised here
# too: both are new abort/report branches, and neither had a test.

# The pin as the Latin American family's consent forbids it: the withheld
# source values present beside the derived shares.
admin_shares_pin_with_values <- function() {
  rows <- admin_shares_pin_rows()
  rows$value[3:4] <- c(79000, 21000)
  rows
}

test_that("the assembled pin refuses the withheld source values", {
  local_admin_shares_board(admin_shares_pin_with_values())

  err <- expect_error(
    whep:::read_admin_shares(),
    class = "whep_error_admin_pin_consent_value"
  )
  expect_match(conditionMessage(err), "admin-stats-latam")
  expect_match(conditionMessage(err), "2")
})

test_that("the same pin without those values is accepted", {
  # The other direction of the rule: a shares-only source is a first-class
  # state, not a tolerated one, so the value-free pin must still load.
  local_admin_shares_board()
  out <- whep:::read_admin_shares()

  latam <- dplyr::filter(out$shares, source == "admin-stats-latam")
  expect_equal(nrow(latam), 2L)
  expect_true(all(is.na(latam$value)))
})

test_that("a declared value family shipping no value is refused", {
  # The mirror failure, and the concrete harm in relaxing `value` for
  # every source rather than for the consented one: a family whose
  # manifest row declares `value` may not quietly become shares-only.
  rows <- admin_shares_pin_rows()
  rows$value[1] <- NA_real_
  rows$share[1] <- 0.53
  local_admin_shares_board(rows)

  err <- expect_error(
    whep:::read_admin_shares(),
    class = "whep_error_admin_pin_measure"
  )
  expect_match(conditionMessage(err), "admin-stats-japan")
})

test_that("a NaN value is refused where the pin is read", {
  # `is.na(NaN)` is TRUE, so a 0/0 artefact would otherwise pass the
  # shares-only rule above and travel as a consented shares-only row.
  rows <- admin_shares_pin_rows()
  rows$value[3] <- NaN
  local_admin_shares_board(rows)

  expect_error(
    whep:::read_admin_shares(),
    class = "whep_error_admin_nonfinite"
  )
})

test_that("an in-house tier-2 source the package cannot place is refused", {
  # An unrecognised in-house label is refused AS UNKNOWN rather than found
  # absent from an allow-list: the two are the same state today, but only
  # the first survives that list being widened, and a typo reaches the
  # second by accident.
  rows <- admin_shares_pin_rows()
  rows$source[1:2] <- "admin-stats-nowhere"
  rows$source_id[1:2] <- "admin-stats-nowhere"

  err <- expect_error(
    whep:::.admin_shares_check_pin(rows),
    class = "whep_error_admin_unknown_source"
  )
  expect_match(conditionMessage(err), "admin-stats-nowhere")
})

test_that("an in-house tier-3 source the package cannot place is refused", {
  rows <- admin_shares_pin_rows()
  rows$source[3:4] <- "panel-in-progress"
  rows$source_id[3:4] <- "panel-in-progress"

  expect_error(
    whep:::.admin_shares_check_pin(rows),
    class = "whep_error_admin_unknown_source"
  )
})

test_that("a recognised family with no consent row is refused", {
  # The consent branch itself, reached the way it happens in the field: a
  # manifest row lost to a rewrite, with the family still in the pin.
  manifest <- whep:::.admin_shares_manifest()
  short <- manifest[manifest$alias != "admin-stats-japan", ]

  err <- expect_error(
    whep:::.admin_shares_check_pin(admin_shares_pin_rows(), manifest = short),
    class = "whep_error_admin_pin_consent"
  )
  expect_match(conditionMessage(err), "admin-stats-japan")
})

test_that("a tier-1 source needs no consent row", {
  # A public product ships under its own licence and has no row in the
  # consent manifest; gating it against one would report the wrong reason.
  # It must still be DECLARED -- its `consent` is `"public_licence"`, not
  # the absence of a declaration.
  rows <- admin_shares_pin_rows()
  rows$source[1:2] <- "JRC_subnational_crops"
  rows$source_id[1:2] <- "JRC_subnational_crops"
  rows$tier[1:2] <- 1L

  expect_equal(nrow(whep:::.admin_shares_check_pin(rows)), 4L)
})

test_that("the assembled pin's own alias does not consent to itself", {
  # The pin-of-pins bookkeeping row lives in the same file the gate reads
  # as its allow-list, so the gate must count only the consent-bearing
  # families -- otherwise a source calling itself `admin-shares` is
  # granted permission by its own manifest row. It names no family, so it
  # is refused as an unrecognised in-house source and never reaches a
  # consent decision at all.
  rows <- admin_shares_pin_rows()
  rows$source[1:2] <- whep:::.admin_shares_alias()
  rows$source_id[1:2] <- whep:::.admin_shares_alias()

  err <- expect_error(
    whep:::.admin_shares_check_pin(rows),
    class = "whep_error_admin_unknown_source"
  )
  expect_match(conditionMessage(err), whep:::.admin_shares_alias())
  expect_false(
    whep:::.admin_shares_alias() %in%
      whep:::.admin_shares_consented(whep:::.admin_shares_manifest())
  )
})

test_that("a family with no consent row is reported as such", {
  # The second member of the excluded report's closed vocabulary. Today
  # every family has a consent row, so the branch is reached by taking one
  # away -- which is exactly how it happens in the field, a manifest row
  # lost to a rewrite.
  manifest <- whep:::.admin_shares_manifest()
  short <- manifest[manifest$alias != "admin-stats-australia", ]
  testthat::local_mocked_bindings(
    .admin_shares_manifest = function() short,
    .package = "whep"
  )
  local_admin_shares_board()

  excluded <- whep:::read_admin_shares()$excluded
  australia <- excluded[excluded$source == "admin-stats-australia", ]

  expect_equal(australia$reason, "no_consent_manifest_row")
  expect_match(australia$detail, "admin_stats_pins_manifest")
})

test_that("the excluded report says why a family contributes nothing", {
  # `detail` carried the family's attribution string, which explains the
  # permission the family ships under and nothing about its absence.
  local_admin_shares_board()
  excluded <- whep:::read_admin_shares()$excluded
  france <- excluded[excluded$source == "admin-stats-france-livestock", ]

  expect_equal(france$reason, "no_rows_in_pin")
  expect_match(france$detail, "head count")
  expect_no_match(france$detail, "attribution:")
})

test_that("a pin missing a measurement column aborts without warnings", {
  # The cross-column rule reached `x$value` directly, so a pin without
  # that column emitted tibble's "Unknown or uninitialised column"
  # warnings before the correct classed error.
  rows <- admin_shares_pin_rows()
  rows$value <- NULL
  local_admin_shares_board(rows)

  expect_no_warning(
    expect_error(
      whep:::read_admin_shares(),
      class = "whep_error_schema_violation"
    )
  )
})

# --- T38 repair: two writers, one shared manifest ---------------------------
#
# `inst/extdata/admin_stats_pins_manifest.csv` is written by two scripts:
# `prepare_admin_stats_pins.R` rebuilds the five family rows, and
# `prepare_admin_shares_pin.R` records the assembled pin's row. Neither may
# delete the other's, and the loss is silent when one does: the assembled
# row is the only tracked record of that artifact's version, md5, byte
# count and composed attribution, and the consent gate only asks whether
# the five FAMILIES are present.
#
# `inst/scripts` is `.Rbuildignore`d, so these run from a source checkout
# and skip on the tarball, where the scripts are absent.

admin_pins_script_env <- function(name) {
  path <- testthat::test_path("..", "..", "inst", "scripts", name)
  testthat::skip_if_not(file.exists(path), "inst/scripts is not in this build")
  env <- new.env(parent = globalenv())
  source(path, local = env, echo = FALSE)
  env
}

# The five family rows as the sibling writer produces them.
admin_pins_manifest_fixture <- function() {
  aliases <- whep:::.admin_family_aliases()
  tibble::tibble(
    alias = aliases,
    tier = c(2L, 2L, 2L, 2L, 3L),
    measure = c("value", "value", "value", "value", "share"),
    rows = c(32095L, 323469L, 6597L, 58740L, 875514L),
    units = c(46L, 50L, 8L, 89L, 142L),
    year_min = 1961L,
    year_max = c(2022L, 2021L, 2022L, 2020L, 2023L),
    attribution = paste0("recorded permission for ", aliases),
    pin_version = "20260903T062154Z",
    parquet_file = paste0(aliases, ".parquet"),
    bytes = c(133439, 1772902, 41155, 480912, 7214414),
    md5 = paste0("md5-", seq_along(aliases)),
    retrieved_at = "2026-09-03T06:21:54Z"
  )
}

# One family rebuilt, which is what `prepare_admin_stats_pins(aliases =)`
# does and the shape its writer receives.
admin_pins_rebuilt_family <- function() {
  list(
    "admin-stats-japan" = list(
      report = tibble::tibble(
        alias = "admin-stats-japan",
        tier = 2L,
        measure = "value",
        rows_out = 32095L,
        units_out = 46L,
        year_min = 1961L,
        year_max = 2022L
      ),
      attribution = "rebuilt permission for admin-stats-japan"
    )
  )
}

admin_pins_rebuilt_staged <- function() {
  tibble::tibble(
    alias = "admin-stats-japan",
    pin_version = "20260905T000000Z",
    parquet_file = "admin-stats-japan.parquet",
    bytes = 133439,
    md5 = "md5-rebuilt",
    csv_bytes = 1
  )
}

admin_shares_staged_fixture <- function() {
  tibble::tibble(
    alias = "admin-shares",
    pin_version = "20260904T135207Z",
    parquet_file = "admin-shares.parquet",
    bytes = 7694161,
    md5 = "90139195b50b3f898c62e6e4169dceb9",
    csv_bytes = 1
  )
}

write_shares_row <- function(env, path) {
  env$write_admin_shares_manifest(
    rows = admin_shares_pin_rows(),
    staged = admin_shares_staged_fixture(),
    manifest = tibble::as_tibble(
      utils::read.csv(path, stringsAsFactors = FALSE)
    ),
    path = path,
    retrieved_at = "2026-09-04T13:52:10Z"
  )
}

write_family_rows <- function(env, path) {
  env$write_admin_pins_manifest(
    families = admin_pins_rebuilt_family(),
    staged = admin_pins_rebuilt_staged(),
    path = path,
    retrieved_at = "2026-09-05T00:00:00Z"
  )
}

test_that("rebuilding the families keeps the assembled pin's row", {
  stats <- admin_pins_script_env("prepare_admin_stats_pins.R")
  shares <- admin_pins_script_env("prepare_admin_shares_pin.R")
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(admin_pins_manifest_fixture(), path)

  suppressMessages(write_shares_row(shares, path))
  suppressMessages(write_family_rows(stats, path))
  out <- utils::read.csv(path, stringsAsFactors = FALSE)

  expect_setequal(
    out$alias,
    c(whep:::.admin_family_aliases(), "admin-shares")
  )
  assembled <- out[out$alias == "admin-shares", ]
  expect_equal(assembled$md5, "90139195b50b3f898c62e6e4169dceb9")
  expect_equal(assembled$pin_version, "20260904T135207Z")
  # A partial rebuild replaces only the family it rebuilt.
  expect_equal(out$md5[out$alias == "admin-stats-japan"], "md5-rebuilt")
  expect_equal(out$md5[out$alias == "admin-stats-australia"], "md5-3")
})

test_that("recording the assembled pin keeps the family rows", {
  stats <- admin_pins_script_env("prepare_admin_stats_pins.R")
  shares <- admin_pins_script_env("prepare_admin_shares_pin.R")
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(admin_pins_manifest_fixture(), path)

  suppressMessages(write_family_rows(stats, path))
  suppressMessages(write_shares_row(shares, path))
  out <- utils::read.csv(path, stringsAsFactors = FALSE)

  expect_setequal(
    out$alias,
    c(whep:::.admin_family_aliases(), "admin-shares")
  )
  expect_equal(out$md5[out$alias == "admin-stats-japan"], "md5-rebuilt")
  expect_equal(
    out$attribution[out$alias == "admin-stats-latam"],
    "recorded permission for admin-stats-latam"
  )
})

test_that("a re-run replaces its own row rather than adding a second", {
  stats <- admin_pins_script_env("prepare_admin_stats_pins.R")
  shares <- admin_pins_script_env("prepare_admin_shares_pin.R")
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(admin_pins_manifest_fixture(), path)

  suppressMessages(write_shares_row(shares, path))
  suppressMessages(write_shares_row(shares, path))
  suppressMessages(write_family_rows(stats, path))
  suppressMessages(write_family_rows(stats, path))
  out <- utils::read.csv(path, stringsAsFactors = FALSE)

  expect_equal(nrow(out), 6L)
  expect_equal(anyDuplicated(out$alias), 0L)
})

test_that("the example's excluded report is the real reporter's", {
  # The fixture transcribed `detail` by hand and so advertised the old
  # contract -- the family's attribution string -- while every test on it
  # passed. It is built from the reporter now, and this pins that.
  example <- whep:::read_admin_shares(example = TRUE)

  expect_equal(
    example$excluded,
    whep:::.admin_shares_excluded(example$shares)
  )
  france <- example$excluded[
    example$excluded$source == "admin-stats-france-livestock",
  ]
  expect_match(france$detail, "head count")
})

test_that("an excluded report with nothing to say is still typed", {
  # With every family in the pin, `absent` is `character(0)` and a bare
  # `ifelse()` would hand back two logical columns.
  rows <- admin_shares_pin_rows()
  present <- purrr::map(whep:::.admin_family_aliases(), function(alias) {
    one <- rows[1L, ]
    one$source <- alias
    one$source_id <- alias
    one$source_native_id <- alias
    one
  })
  excluded <- whep:::.admin_shares_excluded(purrr::list_rbind(present))

  expect_equal(nrow(excluded), 0L)
  expect_true(is.character(excluded$reason))
  expect_true(is.character(excluded$detail))
})

# --- T38 repair, round 3: identity decides the family, never spelling -------
#
# The gate matched the shares-only family by an exact string on `source`
# alone, never read `source_id`, and let tier 1 out of the rule entirely.
# Every route below carried the withheld Latin American source values
# through `read_admin_shares()` with nothing raised. They are pinned here
# together with the routes that already refused, so the whole matrix is
# visible in one place rather than one passing test per fix.

# The pin as the consent forbids it -- the withheld values beside the
# derived shares -- relabelled, re-identified and re-tiered at will.
admin_shares_relabelled <- function(label, tier = 3L, source_id = label) {
  rows <- admin_shares_pin_with_values()
  rows$source[3:4] <- label
  rows$source_id[3:4] <- source_id
  rows$tier[3:4] <- as.integer(tier)
  rows
}

# Spellings of one label. Trimming and case-folding are the minimum: a
# gate a trailing space defeats is not a gate.
admin_shares_spellings <- function(label = "admin-stats-latam") {
  c(
    label,
    paste0(label, " "),
    paste0(" ", label),
    paste0("\t", label, "\n"),
    toupper(label),
    "Admin-Stats-Latam"
  )
}

test_that("the shares-only rule sees through the label's spelling", {
  grid <- tidyr::expand_grid(
    label = admin_shares_spellings(),
    tier = c(1L, 2L, 3L)
  )

  purrr::pwalk(grid, function(label, tier) {
    expect_error(
      whep:::.admin_shares_check_pin(admin_shares_relabelled(label, tier)),
      class = "whep_error_admin_pin_consent_value"
    )
  })
})

test_that("tier is a precedence rank, never a permission", {
  # The exact label at tier 1 was refused; every perturbation of it at
  # tier 1 was not, because the consent gate caught the perturbations at
  # tier 2/3 alone and the measurement rule never saw them as the family.
  rows <- admin_shares_relabelled("admin-stats-latam ", tier = 1L)

  err <- expect_error(
    whep:::.admin_shares_check_pin(rows),
    class = "whep_error_admin_pin_consent_value"
  )
  expect_match(conditionMessage(err), "admin-stats-latam")
  expect_match(conditionMessage(err), "2 rows")
})

test_that("a perturbed label still loads where it ships shares alone", {
  # The other direction, and a deliberate one: canonicalisation decides
  # which family a row belongs to, and belonging is not by itself an
  # offence. A shares-only pin stays first-class however its label is
  # cased, and the excluded report agrees with the gate about it.
  purrr::walk(admin_shares_spellings(), function(label) {
    rows <- admin_shares_pin_rows()
    rows$source[3:4] <- label
    rows$source_id[3:4] <- label

    expect_equal(nrow(whep:::.admin_shares_check_pin(rows)), 4L)
    expect_false(
      "admin-stats-latam" %in% whep:::.admin_shares_excluded(rows)$source
    )
  })
})

test_that("a source_id naming the shares-only family is refused", {
  # `source_id` is the producer's immutable identifier in the same value
  # space as `source`, and nothing read the two together: a row naming a
  # consented family in one column and the withheld panel in the other
  # passed with its values attached.
  rows <- admin_shares_relabelled(
    "admin-stats-japan",
    tier = 2L,
    source_id = "admin-stats-latam"
  )

  err <- expect_error(
    whep:::.admin_shares_check_pin(rows),
    class = "whep_error_admin_pin_identity"
  )
  expect_match(conditionMessage(err), "admin-stats-latam")
  # And the measurement rule refuses it on its own, so the two identifiers
  # agreeing is not the only thing standing between the panel's values and
  # a caller.
  expect_error(
    whep:::.admin_shares_check_measure(rows),
    class = "whep_error_admin_pin_consent_value"
  )
})

test_that("a label the package does not recognise is refused", {
  # A near-miss spelling that canonicalises to nothing declared is not the
  # consented family, and it is not a declared public product either: it
  # has no family to check and no permitted measurement to check against.
  # Tier does not excuse it, in either direction.
  grid <- tibble::tribble(
    ~label, ~tier,
    "admin-stats-latam-2", 1L,
    "admin-stats-latam-2", 3L,
    "admin-stats-nowhere", 1L,
    "panel-in-progress", 2L,
    "admin-shares", 1L
  )

  purrr::pwalk(grid, function(label, tier) {
    err <- expect_error(
      whep:::.admin_shares_check_pin(
        admin_shares_relabelled(label, tier)
      ),
      class = "whep_error_admin_unknown_source"
    )
    expect_match(conditionMessage(err), label, fixed = TRUE)
  })
})

test_that("a declared public source is accepted", {
  # The rule above must not swallow the tier-1 sources it is not about: a
  # public product ships under its own licence and has no consent row, and
  # its declaration is what says so.
  declared <- c(
    "USDA_NASS",
    "IBGE_PAM",
    "Eurostat_apro_cpshr",
    "JRC_subnational_crops"
  )
  purrr::walk(declared, \(label) {
    rows <- admin_shares_pin_rows()
    rows$source[1:2] <- label
    rows$source_id[1:2] <- label
    rows$tier[1:2] <- 1L

    expect_equal(nrow(whep:::.admin_shares_check_pin(rows)), 4L)
  })
})

test_that("the withheld values do not survive read_admin_shares()", {
  # End to end, by the route the brief named: a different spelling of the
  # source label at tier 1. It returned two rows carrying 79000 and 21000.
  local_admin_shares_board(
    admin_shares_relabelled("admin-stats-latam ", tier = 1L)
  )

  expect_error(
    whep:::read_admin_shares(),
    class = "whep_error_admin_pin_consent_value"
  )
})

# --- T38 repair, round 3: the family path's column set is closed ------------
#
# Pre-existing at HEAD, not introduced by T38: `.admin_family_check()`
# REQUIRED the declared columns without CLOSING the set, and the consent
# rule beside it tests one column name. The shares-only family's withheld
# values therefore travelled untouched in any column not literally called
# `value`, and `read_admin_family()`'s caller received them.

test_that("a shares-only family may not ship values under an alias", {
  latam <- whep:::.example_admin_family()[["admin-stats-latam"]]
  latam$raw_value_ha <- c(79000, 9100, 8900, 1800, 900)[seq_len(nrow(latam))]

  err <- expect_error(
    whep:::.admin_family_check(latam, "admin-stats-latam"),
    class = "whep_error_admin_pin_columns"
  )
  expect_match(conditionMessage(err), "raw_value_ha")
})

test_that("a value family may not ship an undeclared column either", {
  # The set is closed for every family, not only for the consented one: an
  # undeclared column is a measurement no rule on this path can see.
  japan <- whep:::.example_admin_family()[["admin-stats-japan"]]
  japan$raw_value_ha <- 1

  expect_error(
    whep:::.admin_family_check(japan, "admin-stats-japan"),
    class = "whep_error_admin_pin_columns"
  )
})

test_that("a `value` column still reports as the consent breach it is", {
  # Ordering: the closure would catch a `value` column too, since it is
  # not in the shares-only family's shape. The consent rule runs first so
  # that the T23 breach is named as one.
  latam <- whep:::.example_admin_family()[["admin-stats-latam"]]
  latam$value <- c(79000, 9100, 8900, 1800, 900)[seq_len(nrow(latam))]

  expect_error(
    whep:::.admin_family_check(latam, "admin-stats-latam"),
    class = "whep_error_admin_pin_consent_value"
  )
})

test_that("the declared shape itself still loads on both families", {
  example <- whep:::.example_admin_family()

  purrr::walk(c("admin-stats-japan", "admin-stats-latam"), function(alias) {
    expect_equal(
      nrow(whep:::.admin_family_check(example[[alias]], alias)),
      nrow(example[[alias]])
    )
  })
})

# --- T38 repair, round 3: the builder's own defects -------------------------
#
# `inst/scripts` is `.Rbuildignore`d, so these run from a source checkout
# and skip on the tarball, where the script is absent.

# A family pin whose unit ids carry the ISO3 prefix the assembly resolves
# containers from, so its rows survive the common filters.
admin_placed_family_pin <- function(alias = "admin-stats-japan") {
  rows <- admin_value_pin(alias)
  rows$source_native_unit_id <- c("JPN-U1", "JPN-U2", "JPN-U3")
  rows
}

test_that("the builder ships the measurement the family is consented for", {
  # The measure was picked by presence -- `share` if the column was there,
  # `value` otherwise -- so the builder's notion of what a family ships
  # could differ from the gate's. It is read from the same constant now.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  japan <- admin_placed_family_pin()
  japan$share <- c(0.5, 0.3, 0.2)

  built <- env$.shares_from_family(japan, "admin-stats-japan", 2L)

  expect_equal(nrow(built$rows), 3L)
  expect_equal(built$rows$value, c(500, 300, 200))
  expect_true(all(is.na(built$rows$share)))
})

test_that("the builder refuses a family pin without its declared measure", {
  # The same defect in the other direction: a shares-only pin that had
  # lost its `share` column and grown a `value` one was staged as the
  # source values the consent withholds.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  latam <- admin_share_pin()
  latam$share <- NULL
  latam$value <- c(50, 30, 20, 25, 65, 10)

  expect_error(
    env$.shares_from_family(latam, "admin-stats-latam", 3L),
    "carries no"
  )
})

test_that("an unknown tier-1 label names the ones the script knows", {
  # `{.val {.shares_tier1_spec()$source}}` starts with a dot inside the
  # braces, so cli parsed it as a STYLE and replaced the whole message
  # with "Invalid cli literal". Fail-closed survived; the diagnosis did
  # not.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")

  err <- expect_error(
    env$.shares_from_tier1(tibble::tibble(x = 1), "not-a-tier1-source")
  )
  expect_match(conditionMessage(err), "No tier-1 definition")
  expect_match(conditionMessage(err), "USDA_NASS")
  expect_no_match(conditionMessage(err), "Invalid cli literal")
})

test_that("an all-dropped tier-1 source warns only about what it dropped", {
  # A bare `$` on the vocabulary join's output emitted tibble's "Unknown
  # or uninitialised column" warning on a legitimate path, and `R CMD
  # check` surfaces it.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  injected <- tibble::tibble(
    source = "IBGE_PAM",
    source_native_unit_id = c("BR-SP", "BR-RJ"),
    source_native_unit_name = c("Sao Paulo", "Rio de Janeiro"),
    source_native_item_code = c("99991", "99992"),
    source_native_item_name = c("Nothing", "Nothing else"),
    indicator_used = "area_harvested",
    year = 2000L,
    value = c(10, 20),
    grain = "admin1"
  )

  seen <- character()
  built <- withCallingHandlers(
    env$.shares_from_tier1(injected, "IBGE_PAM"),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(nrow(built$rows), 0L)
  expect_true(any(grepl("does not know", seen)))
  expect_false(any(grepl("uninitialised", seen)))
})

test_that("a mapped tier-1 row keeps the columns it did carry", {
  # The guard above must stay a guard: where the injected source carries a
  # column, the value in it is what reaches the contract.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  injected <- tibble::tibble(
    source = "IBGE_PAM",
    source_native_unit_id = c("BR-SP", "BR-RJ"),
    source_native_unit_name = c("Sao Paulo", "Rio de Janeiro"),
    # `40102` is an `"exact"` class of `admin_items_sidra`, mapping to
    # `item_prod_code` 27.
    source_native_item_code = "40102",
    source_native_item_name = "Arroz",
    indicator_used = "area_harvested",
    year = 2000L,
    value = c(10, 20),
    grain = "admin1",
    nuts_version = NA_character_,
    source_version = "2026-05-21",
    recorded_at = "2026-09-03T00:00:00Z",
    value_flag = c(NA, "zero_observed")
  )

  built <- env$.shares_from_tier1(injected, "IBGE_PAM")

  expect_equal(nrow(built$rows), 2L)
  expect_equal(built$rows$value, c(10, 20))
  expect_equal(built$rows$item_prod_code, c(27L, 27L))
  expect_equal(built$rows$source_version, c("2026-05-21", "2026-05-21"))
  expect_equal(built$rows$value_flag, c(NA, "zero_observed"))
  expect_true(all(is.na(built$rows$share)))
})

test_that("the attribution names a consented family that shipped no row", {
  # Naming only the contributing families left a manifest reader unable to
  # tell a fifth consented family that was read and dropped from one that
  # was never consented at all.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")

  attribution <- env$.shares_attribution(
    admin_shares_pin_rows(),
    admin_pins_manifest_fixture()
  )

  expect_match(attribution, "admin-stats-france-livestock")
  expect_match(attribution, "contributing no row")
  expect_match(attribution, "recorded permission for admin-stats-japan")
})

# --- T38 repair, round 4: a closed world at the pin boundary ----------------
#
# Rounds 2 and 3 tried to make the gate REJECT bad spellings, and each
# round the next spelling arrived: a leading non-breaking space, a
# zero-width space, a Cyrillic `a`, a fullwidth `a`, an underscore
# variant. A deny-list on labels cannot work, and the decisive case shows
# why: a label in no recognised namespace at tier 1, carrying the withheld
# values, is indistinguishable from a genuine new public product, because
# `admin_shares_schema()` deliberately leaves `source` open and `tier` is a
# precedence rank, never a permission.
#
# So the design changed rather than the deny-list growing again. Every
# source in the assembled pin must be DECLARED in
# `.admin_source_registry()`, and a label resolving to no declaration is
# refused whatever its tier and however it is spelled. Canonicalisation
# survives as a convenience; it is no longer what stands between the
# withheld values and a caller.

# Spellings canonicalisation does NOT fold. Each is a different label
# under the closed world, so none resolves and every one is refused.
# Written as `\u` escapes so this file stays ASCII.
admin_shares_perturbations <- function(label = "admin-stats-latam") {
  c(
    paste0("\u00a0", label),
    paste0("\u200b", label),
    sub("a", "\u0430", label),
    sub("a", "\uff41", label),
    gsub("-", "_", label),
    gsub("-", "\u2013", label)
  )
}

test_that("the source registry is one well-formed authority", {
  registry <- whep:::.admin_source_registry()

  expect_named(registry, c("source", "is_family", "measure", "consent"))
  expect_equal(anyDuplicated(registry$source), 0L)
  expect_true(all(registry$measure %in% c("value", "share")))
  expect_true(all(registry$consent %in% c("manifest", "public_licence")))
  # A family's identity IS its own source label; a public product names
  # no family at all.
  expect_equal(
    whep:::.admin_family_of(c("admin-stats-latam", "USDA_NASS")),
    c("admin-stats-latam", NA)
  )
  expect_equal(
    registry$source[registry$is_family],
    whep:::.admin_family_aliases()
  )
  # Only a family's permission is recorded in the consent manifest.
  expect_equal(
    registry$source[registry$consent == "manifest"],
    whep:::.admin_family_aliases()
  )
  # The assembled pin's own alias is not a source, so it declares nothing
  # and cannot consent to itself.
  expect_false(whep:::.admin_shares_alias() %in% registry$source)
})

test_that("the family vocabulary is read off the registry", {
  expect_equal(
    whep:::.admin_family_aliases(),
    c(
      "admin-stats-japan",
      "admin-stats-spain-provinces",
      "admin-stats-australia",
      "admin-stats-france-livestock",
      "admin-stats-latam"
    )
  )
  expect_equal(whep:::.admin_family_shares_only(), "admin-stats-latam")
  expect_equal(
    whep:::.admin_family_measure(whep:::.admin_family_aliases()),
    c("value", "value", "value", "value", "share")
  )
  # An undeclared alias has no permitted measurement, and is refused
  # rather than defaulted to `value`.
  expect_error(
    whep:::.admin_family_measure("admin-stats-nowhere"),
    class = "whep_error_admin_unknown_source"
  )
})

test_that("no perturbed spelling resolves, at any tier", {
  grid <- tidyr::expand_grid(
    label = admin_shares_perturbations(),
    tier = c(1L, 2L, 3L)
  )

  purrr::pwalk(grid, function(label, tier) {
    err <- expect_error(
      whep:::.admin_shares_check_pin(admin_shares_relabelled(label, tier)),
      class = "whep_error_admin_unknown_source"
    )
    expect_match(conditionMessage(err), "not declared")
  })
})

test_that("no perturbed spelling survives read_admin_shares()", {
  purrr::walk(admin_shares_perturbations(), function(label) {
    local_admin_shares_board(admin_shares_relabelled(label, tier = 1L))

    expect_error(
      whep:::read_admin_shares(),
      class = "whep_error_admin_unknown_source"
    )
  })
})

test_that("an undeclared label at tier 1 is refused, not read as new", {
  # The decisive case. `JRC_1975_2020` is the one to read twice: it looks
  # exactly like a public product, and this package emits no such label.
  labels <- c(
    "INE_panel_2026",
    "JRC_1975_2020",
    "Eurostat_apro_cpshr_v2",
    "subnational-panel"
  )

  purrr::walk(labels, function(label) {
    err <- expect_error(
      whep:::.admin_shares_check_pin(admin_shares_relabelled(label, 1L)),
      class = "whep_error_admin_unknown_source"
    )
    expect_match(conditionMessage(err), label, fixed = TRUE)
    expect_match(conditionMessage(err), "declaration")
  })
})

test_that("the two identifiers are refused disagreeing in either order", {
  pairs <- list(
    c("admin-stats-japan", "admin-stats-latam"),
    c("admin-stats-latam", "admin-stats-japan")
  )

  purrr::walk(pairs, function(pair) {
    expect_error(
      whep:::.admin_shares_check_pin(
        admin_shares_relabelled(pair[[1L]], 2L, source_id = pair[[2L]])
      ),
      class = "whep_error_admin_pin_identity"
    )
  })
  # And an undeclared spelling is refused from either column on its own,
  # so the identity rule is not the only thing reading `source_id`.
  purrr::walk(c("source", "source_id"), function(column) {
    rows <- admin_shares_pin_with_values()
    rows[[column]][3:4] <- "\u00a0admin-stats-latam"

    expect_error(
      whep:::.admin_shares_check_declared(rows),
      class = "whep_error_admin_unknown_source"
    )
  })
})

test_that("a family pin cannot smuggle another family's rows", {
  # The second blocking finding of round 3: `.admin_family_check()` gated
  # on the alias the CALLER asked for and never read the rows' own
  # `source`, so the Latin American panel's withheld values were delivered
  # intact from inside any value family's pin -- three rows each under
  # japan, australia, spain-provinces and france-livestock.
  smuggled <- admin_value_pin("admin-stats-latam")
  smuggled$value <- c(790001.5, 210002.5, 89000)
  value_families <- setdiff(
    whep:::.admin_family_aliases(),
    "admin-stats-latam"
  )

  purrr::walk(value_families, function(alias) {
    pins <- admin_all_pins()
    pins[[alias]] <- smuggled
    local_admin_board(pins)

    err <- expect_error(
      whep:::read_admin_family(alias),
      class = "whep_error_admin_family_source"
    )
    expect_match(conditionMessage(err), "admin-stats-latam")
  })
})

test_that("a family pin naming an undeclared source is refused", {
  pins <- admin_all_pins()
  pins[["admin-stats-japan"]]$source <- "\u00a0admin-stats-japan"
  local_admin_board(pins)

  expect_error(
    whep:::read_admin_family("admin-stats-japan"),
    class = "whep_error_admin_family_source"
  )
})

test_that("a family pin holding its own rows still loads", {
  # The rule must not refuse the ordinary case it exists to protect.
  local_admin_board()
  families <- whep:::read_admin_family()

  expect_equal(families$not_shipped, character(0))
  expect_equal(nrow(families[["admin-stats-latam"]]), 6L)
})

# --- T38 repair, round 4: the declared measure column is a measurement ------
#
# Closing the column set stops a value travelling in a column the family
# does not declare. It did not stop one travelling in the column it DOES
# declare: a shares-only pin whose `share` column held the withheld source
# values passed every rule on the family path, because nothing there asked
# whether a share was a share. The assembled contract bounds `share` to
# [0, 1] (`admin_shares_schema()`); the family path now holds the same
# bound, and a finite non-negative `value` for a value family.

test_that("a share column carrying source values is refused", {
  pins <- admin_all_pins()
  pins[["admin-stats-latam"]]$share <- c(
    790001.5,
    210002.5,
    89000,
    .25,
    .65,
    .1
  )
  local_admin_board(pins)

  err <- expect_error(
    whep:::read_admin_family("admin-stats-latam"),
    class = "whep_error_admin_pin_measure_bounds"
  )
  expect_match(conditionMessage(err), "790001.5", fixed = TRUE)
})

test_that("an out-of-range or non-finite measurement is refused", {
  cases <- tibble::tribble(
    ~alias,              ~column, ~bad,
    "admin-stats-latam", "share", NaN,
    "admin-stats-latam", "share", -0.1,
    "admin-stats-latam", "share", 1.5,
    "admin-stats-japan", "value", Inf,
    "admin-stats-japan", "value", -1
  )

  purrr::pwalk(cases, function(alias, column, bad) {
    pins <- admin_all_pins()
    pins[[alias]][[column]][[1L]] <- bad
    local_admin_board(pins)

    expect_error(
      whep:::read_admin_family(alias),
      class = "whep_error_admin_pin_measure_bounds"
    )
  })
})

test_that("a non-numeric measure column is refused on the family path", {
  pins <- admin_all_pins()
  pins[["admin-stats-latam"]]$share <- as.character(
    pins[["admin-stats-latam"]]$share
  )
  local_admin_board(pins)

  expect_error(
    whep:::read_admin_family("admin-stats-latam"),
    class = "whep_error_admin_pin_measure_bounds"
  )
})

test_that("a missing measurement is not a breach on the family path", {
  # `NA` constrains nothing and is refused by the assembled contract when
  # it reaches a row with no other measurement; it is not a smuggled value.
  pins <- admin_all_pins()
  pins[["admin-stats-latam"]]$share[[1L]] <- NA_real_
  local_admin_board(pins)

  expect_equal(
    nrow(whep:::read_admin_family("admin-stats-latam")[["admin-stats-latam"]]),
    6L
  )
})

test_that("the family builder takes each measure from the one authority", {
  # Round 2's verifier: the builder's notion of what a family ships
  # differed from the gate's. Both read the registry now, so they cannot.
  env <- admin_pins_script_env("prepare_admin_stats_pins.R")
  families <- env$.admin_pins_families()

  expect_equal(families$alias, whep:::.admin_family_aliases())
  expect_equal(
    families$measure,
    whep:::.admin_family_measure(families$alias)
  )
  expect_equal(
    families$measure[families$alias == "admin-stats-latam"],
    "share"
  )
})

test_that("every tier-1 source the assembly can place is declared", {
  # A tier-1 label the assembly knows how to place but the registry does
  # not declare would stage rows the reader's own gate then refuses.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  labels <- env$.shares_tier1_spec()$source

  expect_equal(whep:::.admin_source_of(labels), labels)
})

# --- T38 repair, round 4: the preparer's routes ask the rows what they are --
#
# Both routes into the assembled pin REWRITE `source` to the name the rows
# arrived under -- a staged family folder to its alias, an injected tier-1
# tibble to its list name -- and neither read the column first. The Latin
# American panel's rows, values attached, were therefore relabelled
# `admin-stats-japan` from a folder of that name and `IBGE_PAM` from an
# injection of that name, and `read_admin_shares()`'s gate then saw only
# the new label. Each route now resolves the rows' own `source` through
# the registry before it rewrites anything.

# The panel's rows with the withheld values, placed so that they survive
# the common filters, and labelled as what they are.
admin_smuggled_family_rows <- function(label = "admin-stats-latam") {
  rows <- admin_placed_family_pin("admin-stats-japan")
  rows$source <- label
  rows$value <- c(790001.5, 210002.5, 89000)
  rows
}

test_that("the staged route refuses another family's rows", {
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  value_families <- setdiff(
    whep:::.admin_family_aliases(),
    "admin-stats-latam"
  )

  purrr::walk(value_families, function(alias) {
    err <- expect_error(
      env$.shares_from_family(admin_smuggled_family_rows(), alias, 2L),
      class = "whep_error_admin_family_source"
    )
    expect_match(conditionMessage(err), "admin-stats-latam")
  })
})

test_that("the staged route refuses an undeclared label too", {
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")

  purrr::walk(admin_shares_perturbations(), function(label) {
    expect_error(
      env$.shares_from_family(
        admin_smuggled_family_rows(label),
        "admin-stats-japan",
        2L
      ),
      class = "whep_error_admin_family_source"
    )
  })
})

test_that("the staged route refuses a value column in the shares-only family", {
  # The same T23 breach `read_admin_family()` refuses on the pin route. It
  # used to be left behind silently here: the measure selection took
  # `share` and dropped `value` on the floor, which hid that a staged
  # folder was carrying the withheld values at all.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  latam <- admin_share_pin()
  latam$value <- c(50, 30, 20, 25, 65, 10)

  expect_error(
    env$.shares_from_family(latam, "admin-stats-latam", 3L),
    class = "whep_error_admin_pin_consent_value"
  )
})

test_that("the staged route still takes a family's own rows", {
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")

  built <- env$.shares_from_family(
    admin_placed_family_pin(),
    "admin-stats-japan",
    2L
  )

  expect_equal(nrow(built$rows), 3L)
  expect_equal(built$rows$value, c(500, 300, 200))
})

test_that("the assembly refuses a staged folder holding another family", {
  # End to end through `assemble_admin_shares()`: a folder named for a
  # value family, holding the panel's rows, with the board holding
  # nothing so that the staged route is the one taken.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  staging <- withr::local_tempdir()
  folder <- file.path(staging, "admin-stats-japan", "20260905T000000Z")
  dir.create(folder, recursive = TRUE)
  nanoparquet::write_parquet(
    admin_smuggled_family_rows(),
    file.path(folder, "admin-stats-japan.parquet")
  )
  manifest <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(admin_pins_manifest_fixture(), manifest)
  testthat::local_mocked_bindings(
    .admin_family_registered = function(alias) FALSE,
    .package = "whep"
  )

  err <- expect_error(
    env$assemble_admin_shares(staging_dir = staging, manifest_path = manifest),
    class = "whep_error_admin_family_source"
  )
  expect_match(conditionMessage(err), "admin-stats-latam")
})

# Injected rows in the reader shape, carrying a `source` of their own.
admin_injected_tier1_rows <- function(source = "IBGE_PAM") {
  tibble::tibble(
    source = source,
    source_native_unit_id = c("BR-SP", "BR-RJ"),
    source_native_unit_name = c("Sao Paulo", "Rio de Janeiro"),
    source_native_item_code = "40102",
    source_native_item_name = "Arroz",
    indicator_used = "area_harvested",
    year = 2000L,
    value = c(790001.5, 210002.5),
    grain = "admin1"
  )
}

test_that("an injected tier-1 source cannot relabel another source's rows", {
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  foreign <- c(
    "admin-stats-latam",
    "00a0admin-stats-latam",
    "USDA_NASS",
    "IBGE_PAM_v2"
  )

  purrr::walk(foreign, function(label) {
    err <- expect_error(
      env$.shares_from_tier1(admin_injected_tier1_rows(label), "IBGE_PAM"),
      class = "whep_error_admin_family_source"
    )
    expect_match(conditionMessage(err), label, fixed = TRUE)
  })
})

test_that("an injected tier-1 source without a source column is refused", {
  # Rows naming no producer cannot be resolved against the registry, and
  # every tier-1 reader emits `source`, so an injection without it is a
  # laundering route rather than a convenience: the label alone would
  # decide what the rows are, which is the alias-only gate all over again.
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")
  rows <- admin_injected_tier1_rows()
  rows$source <- NULL

  err <- expect_error(
    env$.shares_from_tier1(rows, "IBGE_PAM"),
    class = "whep_error_admin_family_source"
  )
  expect_match(conditionMessage(err), "no `source` column", fixed = TRUE)
})

test_that("an injected tier-1 source naming itself still loads", {
  env <- admin_pins_script_env("prepare_admin_shares_pin.R")

  built <- env$.shares_from_tier1(admin_injected_tier1_rows(), "IBGE_PAM")

  expect_equal(nrow(built$rows), 2L)
  expect_equal(built$rows$value, c(790001.5, 210002.5))
  expect_equal(unique(built$rows$source), "IBGE_PAM")
})
