# The reader is exercised through `whep:::` rather than `whep::` because
# NAMESPACE is not regenerated in this wave, so the new export is not yet
# registered. Nothing here reaches the network or a WHEP_* path: every
# read goes through a fixture holding a real SDMX-CSV response, and the
# two live legs (`.fetch_eurostat()` and `.eurostat_code_exists()`) are
# stubbed with `local_mocked_bindings()`.

eurostat_fixture <- function(table) {
  test_path("fixtures", paste0("eurostat_", table, ".csv"))
}

read_eurostat_fixture <- function(table, ...) {
  suppressMessages(
    whep:::read_admin_stats_eurostat(
      table = table,
      file = eurostat_fixture(table),
      ...
    )
  )
}

# Write a fixture back out with one textual substitution, so a case the
# live tables do not yet contain (a 2025 reference year) or cannot contain
# (two current codes sharing a label) can still be exercised on real rows.
patch_eurostat_fixture <- function(table, pattern, replacement) {
  text <- readr::read_file(eurostat_fixture(table))
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  readr::write_file(
    stringr::str_replace_all(text, stringr::fixed(pattern), replacement),
    path
  )
  path
}

eurostat_output_columns <- c(
  "source",
  "source_native_unit_id",
  "source_native_unit_name",
  "source_native_item_code",
  "source_native_item_name",
  "quantity",
  "indicator_used",
  "year",
  "value",
  "value_unit",
  "value_flag",
  "concept_break",
  "grain",
  "nuts_level",
  "nuts_version",
  "source_version",
  "recorded_at"
)

# ---- Parsing ----------------------------------------------------------

test_that("a real SDMX-CSV response parses into the reader's columns", {
  out <- read_eurostat_fixture("apro_cpnhr_h")

  expect_s3_class(out, "tbl_df")
  expect_named(out, eurostat_output_columns)
  expect_gt(nrow(out), 0L)
  expect_true(is.character(out$source_native_unit_id))
  expect_true(is.character(out$source_native_item_code))
  expect_true(is.integer(out$year))
  expect_true(is.double(out$value))
  expect_true(is.logical(out$concept_break))
  expect_true(is.integer(out$nuts_level))
  expect_true(all(out$source == "Eurostat_apro_cpnhr_h"))
  expect_true(all(out$source_version == "01/08/23 23:00:00"))
  expect_match(out$recorded_at[[1L]], "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:")
})

test_that("labels with commas survive parsing intact", {
  out <- read_eurostat_fixture("apro_mt_ls_r")
  swine <- dplyr::filter(out, source_native_item_code == "A3100")

  expect_gt(nrow(swine), 0L)
  expect_true(
    all(swine$source_native_item_name == "Live swine, domestic species")
  )
})

test_that("only the two admin-shares grains are emitted by default", {
  out <- read_eurostat_fixture("apro_cpshr")

  expect_setequal(out$grain, "admin1")
  expect_setequal(out$nuts_level, 2L)
  expect_setequal(out$quantity, c("area", "production"))
  expect_setequal(
    out$indicator_used,
    c("area_harvested", "production")
  )
})

test_that("head counts carry a quantity but no indicator", {
  out <- read_eurostat_fixture("apro_mt_ls_r")

  expect_setequal(out$quantity, "heads")
  expect_true(all(is.na(out$indicator_used)))
  expect_setequal(out$value_unit, "heads")
})

# ---- Unit conversion --------------------------------------------------

test_that("thousand hectares and thousand tonnes convert to ha and t", {
  out <- read_eurostat_fixture("apro_cpnhr_h")
  wheat <- out |>
    dplyr::filter(
      source_native_unit_id == "FRF2",
      source_native_item_code == "C1110",
      year == 1995L
    )

  # Eurostat serves FRF2/C1110/1995 as 405.10 thousand hectares and
  # 2906.40 thousand tonnes.
  expect_equal(
    wheat$value[wheat$quantity == "area"],
    405100
  )
  expect_equal(wheat$value_unit[wheat$quantity == "area"], "ha")
  expect_equal(
    wheat$value[wheat$quantity == "production"],
    2906400
  )
  expect_equal(wheat$value_unit[wheat$quantity == "production"], "tonnes")
})

test_that("thousand head converts to head and head stays head", {
  cattle <- read_eurostat_fixture("apro_mt_ls_r") |>
    dplyr::filter(
      source_native_unit_id == "FRF2",
      source_native_item_code == "A2000",
      year == 2020L
    )
  poultry <- read_eurostat_fixture("ef_lsk_poultry") |>
    dplyr::filter(
      source_native_unit_id == "FRF2",
      source_native_item_code == "A5000",
      year == 2020L
    )

  # 547.52 thousand head of bovines; 6,600,200 head of poultry.
  expect_equal(cattle$value, 547520)
  expect_equal(poultry$value, 6600200)
})

test_that("a changed unit label aborts instead of rescaling silently", {
  wrong_unit <- dplyr::mutate(
    whep:::.eurostat_measure_map(),
    label_check = "million hectares"
  )
  local_mocked_bindings(.eurostat_measure_map = function() wrong_unit)

  expect_error(
    read_eurostat_fixture("apro_cpnhr_h"),
    "changed the unit"
  )
})

test_that("measures with no admin-shares quantity are dropped and counted", {
  msgs <- capture_messages(
    out <- whep:::read_admin_stats_eurostat(
      table = "apro_cpshr",
      file = eurostat_fixture("apro_cpshr")
    )
  )

  expect_true(any(grepl("HUMD_EU_PC", msgs)))
  expect_true(any(grepl("YLD_HUMD_EU_T_HA", msgs)))
  expect_false(any(out$value_unit == "t/ha"))
  expect_false("yield" %in% out$quantity)
})

test_that("suppressed observations keep the flag and lose only the value", {
  swine <- read_eurostat_fixture("apro_mt_ls_r") |>
    dplyr::filter(
      source_native_unit_id == "FI20",
      source_native_item_code == "A3100",
      year == 2020L
    )

  expect_equal(nrow(swine), 1L)
  expect_true(is.na(swine$value))
  expect_equal(swine$value_flag, "C")
})

test_that("a clean observation carries no flag", {
  clean <- read_eurostat_fixture("apro_cpnhr_h") |>
    dplyr::filter(
      source_native_unit_id == "FRF2",
      source_native_item_code == "C1110",
      quantity == "area",
      year == 1995L
    )

  expect_true(is.na(clean$value_flag))
})

# ---- The 2025 concept break -------------------------------------------

test_that("pre-2025 crop rows carry no concept break", {
  out <- read_eurostat_fixture("apro_cpshr")

  expect_true(max(out$year) <= 2024L)
  expect_false(any(out$concept_break))
})

test_that("2025 crop rows are flagged as a concept break", {
  patched <- patch_eurostat_fixture("apro_cpshr", ",2021,", ",2025,")
  out <- suppressMessages(
    whep:::read_admin_stats_eurostat(table = "apro_cpshr", file = patched)
  )

  expect_true(any(out$year == 2025L))
  expect_true(all(out$concept_break[out$year == 2025L]))
  expect_false(any(out$concept_break[out$year < 2025L]))
})

test_that("the concept break is a crop-table rule only", {
  patched <- patch_eurostat_fixture("apro_mt_ls_r", ",2020,", ",2025,")
  out <- suppressMessages(
    whep:::read_admin_stats_eurostat(table = "apro_mt_ls_r", file = patched)
  )

  expect_true(any(out$year == 2025L))
  expect_false(any(out$concept_break))
})

# ---- NUTS versions and the vintage dedupe -----------------------------

test_that("the version comes from the label suffix, else the current one", {
  expect_equal(
    whep:::.eurostat_nuts_version("Champagne-Ardenne (NUTS 2013)"),
    "2013"
  )
  expect_equal(
    whep:::.eurostat_nuts_version("Champagne-Ardenne"),
    whep:::.eurostat_current_nuts()
  )
  expect_equal(whep:::.eurostat_current_nuts(), "2024")
})

test_that("a recoded territory keeps its newest vintage, and says so", {
  msgs <- capture_messages(
    out <- whep:::read_admin_stats_eurostat(
      table = "apro_cpnhr_h",
      file = eurostat_fixture("apro_cpnhr_h")
    )
  )
  overlap <- out |>
    dplyr::filter(
      stringr::str_starts(source_native_unit_name, "Champagne-Ardenne"),
      source_native_item_code == "C1110",
      quantity == "area",
      year %in% 1990:1999
    )

  # FR21 covers 1989-1999 and FRF2, the same polygon recoded, 1990-1999.
  expect_true(any(grepl("Dropped 40 older-vintage rows", msgs)))
  expect_true(any(grepl("FR21 \\(NUTS 2013\\)", msgs)))
  expect_true(any(grepl("0 dropped rows disagreed", msgs)))
  expect_setequal(overlap$source_native_unit_id, "FRF2")
  expect_equal(nrow(overlap), 10L)
})

test_that("a year only the older code covers is kept", {
  out <- read_eurostat_fixture("apro_cpnhr_h")
  earliest <- out |>
    dplyr::filter(
      stringr::str_starts(source_native_unit_name, "Champagne-Ardenne"),
      source_native_item_code == "C1110",
      quantity == "area",
      year == 1989L
    )

  expect_equal(earliest$source_native_unit_id, "FR21")
  expect_equal(earliest$value, 413100)
  expect_equal(earliest$nuts_version, "2013")
})

test_that("the dedupe leaves one row per territory, item and year", {
  out <- read_eurostat_fixture("apro_cpnhr_h")
  keys <- out |>
    dplyr::mutate(
      label_key = stringr::str_remove(
        source_native_unit_name,
        "\\s*\\(NUTS\\s+\\d{4}\\)$"
      )
    ) |>
    dplyr::count(label_key, source_native_item_code, quantity, year)

  expect_true(all(keys$n == 1L))
})

test_that("a shared label at different NUTS levels is never merged", {
  out <- read_eurostat_fixture("apro_mt_ls_r", nuts_level = c(1L, 2L))
  aland <- dplyr::filter(out, source_native_unit_name == "Åland")

  expect_setequal(aland$source_native_unit_id, c("FI2", "FI20"))
  expect_setequal(aland$nuts_level, c(1L, 2L))
})

test_that("a tie at the newest vintage keeps both rows and warns", {
  # Two current codes for one NUTS 2 label is a collision the vintage
  # preference cannot break, so nothing may be dropped on it.
  patched <- patch_eurostat_fixture(
    "apro_cpnhr_h",
    "FR22:Picardie (NUTS 2013)",
    "FR22:Alsace"
  )

  expect_warning(
    out <- suppressMessages(
      whep:::read_admin_stats_eurostat(
        table = "apro_cpnhr_h",
        file = patched
      )
    ),
    "share a label at the newest NUTS version"
  )
  expect_true(all(c("FR22", "FRF1") %in% out$source_native_unit_id))
})

# ---- Grain, levels and the national check -----------------------------

test_that("NUTS 1 is opt-in and carries no grain", {
  default <- read_eurostat_fixture("apro_cpshr")
  with_nuts1 <- read_eurostat_fixture("apro_cpshr", nuts_level = c(1L, 2L))
  nuts1 <- dplyr::filter(with_nuts1, nuts_level == 1L)

  expect_false("DE1" %in% default$source_native_unit_id)
  expect_true("DE1" %in% nuts1$source_native_unit_id)
  expect_true(all(is.na(nuts1$grain)))
})

test_that("Germany's crop area stops at NUTS 2 and continues at NUTS 1", {
  out <- read_eurostat_fixture("apro_cpshr", nuts_level = c(1L, 2L))
  german_area <- out |>
    dplyr::filter(
      stringr::str_starts(source_native_unit_id, "DE"),
      quantity == "area"
    )
  nuts2_years <- german_area$year[german_area$nuts_level == 2L]
  nuts1_years <- german_area$year[german_area$nuts_level == 1L]

  # The fixture spans 2003-2004, 2008-2009 and 2020-2021: DE11 reports
  # crop area only in the first block, DE1 throughout.
  expect_true(max(nuts2_years) <= 2004L)
  expect_true(max(nuts1_years) >= 2021L)
})

test_that("national rows leave the units and arrive as the check", {
  out <- read_eurostat_fixture("apro_cpshr")
  national <- attr(out, "national_check")

  expect_false(any(nchar(out$source_native_unit_id) == 2L))
  expect_named(national, eurostat_output_columns)
  expect_setequal(national$source_native_unit_id, c("DE", "ES", "FR"))
  expect_setequal(national$nuts_level, 0L)
  expect_true(all(is.na(national$grain)))
})

test_that("the national check totals more than the units it contains", {
  out <- read_eurostat_fixture("apro_cpshr")
  national <- attr(out, "national_check")
  spain_national <- national$value[
    national$source_native_unit_id == "ES" &
      national$quantity == "area" &
      national$year == 2020L
  ]
  catalonia <- out$value[
    out$source_native_unit_id == "ES51" &
      out$quantity == "area" &
      out$year == 2020L
  ]

  expect_length(spain_national, 1L)
  expect_length(catalonia, 1L)
  expect_gt(spain_national, catalonia)
})

test_that("aggregates that would double-count members are dropped", {
  msgs <- capture_messages(
    out <- whep:::read_admin_stats_eurostat(
      table = "apro_cpshr",
      file = eurostat_fixture("apro_cpshr")
    )
  )

  expect_true(any(grepl("EL41_42", msgs)))
  expect_true(any(grepl("EU27_2020", msgs)))
  expect_false(any(out$source_native_unit_id == "EL41_42"))
  expect_false(any(attr(out, "national_check")$source_native_unit_id == "EU"))
  expect_true(all(c("EL41", "EL42") %in% out$source_native_unit_id))
})

test_that("geography classes and levels follow the code shape", {
  codes <- c("FR", "FRF", "FRF2", "FRF21", "EU", "EU27_2020", "EL41_42")
  classes <- whep:::.eurostat_geo_class(codes)

  expect_equal(
    classes,
    c(
      "nuts",
      "nuts",
      "nuts",
      "nuts",
      "supranational",
      "supranational",
      "composite"
    )
  )
  expect_equal(
    whep:::.eurostat_nuts_level(codes, classes),
    c(0L, 1L, 2L, 3L, NA, NA, NA)
  )
  expect_equal(
    whep:::.eurostat_grain(c(0L, 1L, 2L, 3L)),
    c(NA, NA, "admin1", "admin2")
  )
})

# ---- Requests, and the runtime code check -----------------------------

test_that("the SDMX key and period range follow the filters", {
  expect_equal(
    whep:::.eurostat_build_key(
      "apro_cpshr",
      list(items = c("C1110", "C1300"), geo = c("FR", "FRF2"))
    ),
    "A.C1110+C1300..FR+FRF2"
  )
  expect_equal(
    whep:::.eurostat_build_key("apro_cpshr", list()),
    "A..."
  )
  expect_equal(
    whep:::.eurostat_build_key("ef_lsk_poultry", list(items = "A5000")),
    "A.TOTAL.TOTAL.TOTAL.A5000.HD."
  )
  expect_equal(
    whep:::.eurostat_build_query(list(years = c(1990L, 1995L, 1999L))),
    list(startPeriod = 1990L, endPeriod = 1999L)
  )
  expect_equal(whep:::.eurostat_build_query(list()), list())
})

test_that("years are trimmed to the exact set requested", {
  out <- read_eurostat_fixture(
    "apro_cpnhr_h",
    filters = list(years = c(1995L, 1997L))
  )

  expect_setequal(out$year, c(1995L, 1997L))
})

test_that("a live read checks the code, then fetches with that key", {
  requested <- NULL
  local_mocked_bindings(
    .eurostat_code_exists = function(code) TRUE,
    .fetch_eurostat = function(code, key, query = list()) {
      requested <<- list(code = code, key = key, query = query)
      whep:::.eurostat_parse_sdmx(
        readr::read_file(eurostat_fixture("apro_cpnhr_h"))
      )
    }
  )

  out <- suppressMessages(
    whep:::read_admin_stats_eurostat(
      table = "apro_cpnhr_h",
      filters = list(items = "C1110", geo = "FRF2", years = 1995:1996)
    )
  )

  expect_equal(requested$code, "apro_cpnhr_h")
  expect_equal(requested$key, "A.C1110..FRF2")
  expect_equal(requested$query, list(startPeriod = 1995L, endPeriod = 1996L))
  expect_setequal(out$year, c(1995L, 1996L))
})

test_that("a renamed dataflow aborts naming the tables this reader knows", {
  local_mocked_bindings(.eurostat_code_exists = function(code) FALSE)

  expect_error(
    whep:::read_admin_stats_eurostat(table = "apro_cpshr"),
    "APRO_CPSHR"
  )
  expect_error(
    whep:::read_admin_stats_eurostat(table = "apro_cpshr"),
    "EF_LSK_POULTRY"
  )
})

test_that("the registry answer maps to exists, renamed, or an abort", {
  local_mocked_bindings(GET = function(...) NULL, .package = "httr")

  local_mocked_bindings(
    status_code = function(...) 200L,
    .package = "httr"
  )
  expect_true(whep:::.eurostat_code_exists("apro_cpshr"))

  local_mocked_bindings(
    status_code = function(...) 404L,
    .package = "httr"
  )
  expect_false(whep:::.eurostat_code_exists("apro_cpshr"))

  local_mocked_bindings(
    status_code = function(...) 500L,
    .package = "httr"
  )
  expect_error(
    whep:::.eurostat_code_exists("apro_cpshr"),
    "dataflow registry answered"
  )
})

test_that("an unknown table is rejected before anything is fetched", {
  local_mocked_bindings(
    .eurostat_code_exists = function(code) stop("must not be reached")
  )

  expect_error(
    whep:::read_admin_stats_eurostat(table = "apro_not_a_table"),
    "apro_not_a_table"
  )
})

test_that("a misspelt filter name aborts instead of widening the read", {
  expect_error(
    read_eurostat_fixture("apro_cpshr", filters = list(geos = "FR")),
    "filters"
  )
  expect_error(
    read_eurostat_fixture("apro_cpshr", filters = c(geo = "FR")),
    "filters"
  )
  expect_silent(whep:::.eurostat_check_filters(list(geo = "FR", years = 2000)))
})

test_that("nuts_level must be a subset of the three unit levels", {
  expect_error(
    read_eurostat_fixture("apro_cpshr", nuts_level = 0L),
    "nuts_level"
  )
  expect_error(
    read_eurostat_fixture("apro_cpshr", nuts_level = integer()),
    "nuts_level"
  )
  expect_equal(whep:::.eurostat_check_levels(c(2, 2, 3)), c(2L, 3L))
})

test_that("a response without labels aborts rather than guessing versions", {
  text <- readr::read_file(eurostat_fixture("apro_cpnhr_h"))
  stripped <- stringr::str_replace_all(text, "(?m)([A-Z0-9_]+):[^,\n]+", "\\1")
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_file(stripped, path)

  expect_error(
    suppressMessages(
      whep:::read_admin_stats_eurostat(
        table = "apro_cpnhr_h",
        file = path
      )
    ),
    "no geography labels"
  )
})

test_that("a response with no observations aborts naming the request", {
  header <- stringr::str_c(
    readr::read_lines(eurostat_fixture("apro_cpnhr_h"), n_max = 1L),
    "
"
  )
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_file(header, path)

  expect_error(
    whep:::read_admin_stats_eurostat(
      table = "apro_cpnhr_h",
      file = path,
      filters = list(geo = "FR21", years = 2015L)
    ),
    "served no observations"
  )
})

test_that("a non-CSV response aborts", {
  expect_error(
    whep:::.eurostat_parse_sdmx("<S:Fault>ERR_UNSUPPORTED_FORMAT</S:Fault>"),
    "empty or not a CSV"
  )
})

# ---- The documented example -------------------------------------------

test_that("read_admin_stats_eurostat(example = TRUE) matches the contract", {
  out <- whep:::read_admin_stats_eurostat(example = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_named(out, eurostat_output_columns)
  expect_equal(nrow(out), 10L)
  expect_true(all(out$value_unit %in% c("ha", "tonnes", "heads")))
  expect_true(all(out$quantity %in% c("area", "production", "heads")))
  expect_true(
    all(
      is.na(out$indicator_used) |
        out$indicator_used %in%
          c(
            "area_harvested",
            "area_planted_or_sown",
            "area_main",
            "area_cultivated",
            "production",
            "yield"
          )
    )
  )
  expect_true(all(is.na(out$indicator_used[out$quantity == "heads"])))
  expect_true(all(out$grain %in% "admin1"))
})
