# -----------------------------------------------------------------------
# prepare_admin_shares_pin.R
#
# Assembles the single `admin-shares` pin (#1000, T38): the tier-1, tier-2
# and tier-3 subnational administrative statistics, put onto the one
# contract `admin_shares_schema()` declares, so that
# `resolve_admin_shares()` and the level allocation read one table instead
# of six shapes. It stages the pin folder and appends its manifest row.
# It UPLOADS NOTHING and does not run `data-raw/whep_inputs.R`:
# registering the alias is a separate, deliberate step the user
# authorises.
#
# Inputs
# ------
# TIER 2 / TIER 3 -- the five families of `read_admin_family()`. Each is
# taken from the pins board where its alias is registered (route `"pin"`)
# and otherwise from its staged folder under `inst/scripts/pin_upload/`
# (route `"staged"`), which is where `prepare_admin_stats_pins.R` leaves
# it. The route is recorded and printed per family. There is no silent
# fallback: a family that is neither registered nor staged is reported as
# `"absent"` and contributes nothing.
#
# TIER 1 -- the public readers (`read_admin_stats_nass()`,
# `read_admin_stats_eurostat()`, `read_admin_stats_sidra()`, and the JRC
# product prepared by `prepare_jrc_subnational.R`). They need the network,
# so they are INJECTED through `sources`: a named list of reader-shape
# tibbles, the name being the source label. `sources = NULL`, the default,
# assembles the family half alone and says so.
#
# The consent gate
# ----------------
# `inst/extdata/admin_stats_pins_manifest.csv` is the record of what each
# in-house family may be redistributed as (T23, decided 2026-09-02). A
# family with no row there is EXCLUDED AND NAMED in the report -- never
# quietly skipped -- because an in-house compilation without a recorded
# permission is exactly what this design exists to stop. Tier-1 sources
# are public products with their own licences and carry no row here, so
# the manifest gate does not apply to them.
#
# What DOES apply to every source, tier 1 included, is the closed world of
# `whep:::.admin_source_registry()`: a label resolving to no declaration
# there is refused by `read_admin_shares()`, and this script runs that
# same gate before it stages anything. Every label in
# `.shares_tier1_spec()` below is therefore declared, and an injected
# `sources` name outside that spec aborts here rather than being staged
# into a pin the reader would then refuse.
#
# Which source a row IS is decided by its own `source` column resolved
# through that registry, never by the name it arrived under. Both routes
# into this script rewrite `source` to that name -- a staged family folder
# to its alias, an injected tier-1 tibble to its list name -- and a
# rewrite that never asked the rows what they were is a laundering step:
# the reader's gate then sees only the new name. So `.shares_from_family()`
# and `.shares_from_tier1()` both run `whep:::.admin_family_check_source()`
# on the rows first, and rows naming another declared source, or none --
# a missing `source` column included -- are refused rather than relabelled.
#
# A staged folder whose alias is not one of the five families is neither
# read nor gated: it is named in the report as `staged_not_a_family`. On
# 2026-09-03 that is `admin-stats-jrc`, the public tier-1 JRC product,
# whose route into this pin is `sources` and whose licence is recorded in
# `inst/extdata/jrc_subnational_source_manifest.csv`.
#
# Shares-only rows are first class
# --------------------------------
# The Latin American family is consented as DERIVED SHARES ONLY, so its
# rows carry `share` and no `value`. Since T38 the contract admits that
# (`admin_shares_schema()`, section "Shares-only rows"), and NOTHING here
# invents a value: not `0`, which would allocate as a reported area of
# zero, and not `share * national`, which would reconstruct the very
# quantity the consent withheld. Equally, no share is derived here for the
# families that ship values -- a share computed from the values in the
# same table makes the seam gate's tier-A identity true by construction.
#
# What is dropped, and why each drop is counted
# ---------------------------------------------
# Per source, in this order, every step reporting its own count:
#
#   1. head counts (`indicator_used` missing). Head counts are outside
#      this schema BY DESIGN: `indicator_used` closes over area,
#      production and yield (`R/admin_stats_sidra.R:76-77, 222-223`), and
#      the livestock constraint travels the reader/family path instead. So
#      `admin-stats-france-livestock` -- 58,740 rows, all head counts --
#      contributes NO ROW to this pin, and that is the design, not a loss.
#   2. an indicator outside the contract's closed vocabulary.
#   3. an item the WHEP production vocabulary does not carry. Tier-2/3
#      families need no T11 crosswalk -- their `source_native_item_code`
#      IS the WHEP `item_prod_code` -- so what they need instead is a
#      VALIDITY CHECK against `items_prod_full`. Tier-1 sources go through
#      their `admin_items_*` vocabulary first (below) and are then checked
#      the same way.
#   4. a grain outside `c("admin1", "admin2", "admin3")`. A national row
#      (NUTS 0) is the container, not a unit, and has no grain.
#   5. a unit whose container cannot be resolved. Family unit ids carry an
#      ISO3 prefix (`JPN-HOKKAIDO`, `ESP-ES111`, `BOL-LAPAZ`) resolved
#      through `regions_full`; NUTS ids carry the two-letter country code.
#      JRC's three Excel-mangled region codes (`2-D`, `4-D` and `5-D`,
#      served as `2-Dec`, `4-Dec`, `5-Dec`) carry neither, and are counted
#      and dropped here with the offending prefixes named.
#   6. a row carrying neither `value` nor `share`.
#
# `treatment_year` and the estimation lane: OPEN, and not decided here
# ------------------------------------------------------------------
# Every assembled row is written `treatment_year = "observed"`. That
# column says HOW THE YEAR WAS OBTAINED -- observed, interpolated or
# carried, the gap rule's vocabulary (`admin_shares_schema()`) -- and no
# assembled row is gap-filled, so the value is right on its own terms.
#
# It says nothing about how the VALUE was estimated, and the contract has
# no column that does. Measured on the harmonized panel
# (`C:/XL_files/whep/input/subnational/whep_production_subnational.parquet`,
# the 2026-08-31 snapshot, on 2026-09-05):
#
#   - Japan, Australia and France are 100% `lane == "observational"`
#     (32,273 + 18,843 + 21,917 / 13,725 / 157,708 rows), so T24 selecting
#     them without a lane filter costs nothing.
#   - Spain WAS filtered to `lane == "observational"` by T24 (717,861 of
#     1,373,508 rows available).
#   - The Latin American panel was NOT, and every one of its in-scope crop
#     rows is `lane == "legacy_balanced"`: 667,142 area and 676,623
#     production rows against ZERO observational ones in all six
#     countries. Its only observational rows are 9,807 head counts, which
#     this schema does not carry.
#
# So the 820,214 Latin American rows in this pin are a balanced
# reconstruction, not directly transcribed statistics, and filtering that
# family to `"observational"` would remove it entirely rather than trim
# it. Whether a `legacy_balanced` series may set the within-country shape
# is an inclusion decision for the maintainer, not one this script may
# make; it is recorded here so that it cannot be lost, and the guard below
# refuses a non-observational lane the moment a family pin starts
# carrying its `lane` column.
#
# The tier-1 vocabulary join
# --------------------------
# A tier-1 source's classes are mapped onto WHEP items by its
# `admin_items_*` table (T11), joined on `class_key` against the reader's
# `source_native_item_code` falling back to `source_native_item_name`,
# which is how those tables key a publisher that issues no class code.
# `mapping_kind` then decides:
#
#   - `"exact"` and `"aggregate"` are kept. The publisher's aggregate
#     binds (T10 crop rule 1).
#   - `"member"` and `"dropped"` are dropped and counted: a member summed
#     alongside its own published aggregate would double-count it, and a
#     dropped class has no WHEP counterpart.
#   - `"sum_member"` classes DO sum to their target, the publisher
#     shipping no aggregate. They are summed here, per unit, indicator and
#     year -- but only where EVERY declared member of that target is
#     present, because a partial sum understates without showing it. An
#     incomplete group is dropped and counted. A `"yield"` sum_member row
#     is dropped and counted too: a sum of yields is not a yield.
#   - a class the vocabulary does not know at all is dropped, counted and
#     WARNED about; that is a vocabulary gap, not an ordinary drop.
#
# The `level` convention
# ----------------------
# Every row is written at `level = 1L`. `level` is the administrative
# depth GRANTED for the row, and depth 1 is the only depth the polities
# side has granted (the plan's "Spain depth convention" is still open).
# It is not the reporting fineness: that is `grain`, and Spain's provinces
# stay `"admin2"` while France's departments and Japan's prefectures stay
# `"admin1"`. Keeping every candidate at one `level` is also what lets
# `resolve_admin_shares()` compare them at all -- `level` is part of its
# candidate group, so putting Spain's provinces at level 2 would take them
# out of the group Eurostat's NUTS-2 rows are in, and the grain rule its
# own documentation illustrates with exactly that pair could never fire.
#
# `concept_break` is written `FALSE` on every row. The contract forbids
# `NA` there, and no family declares a break, so `FALSE` records the
# ABSENCE OF A DECLARATION rather than a verified absence of one. Breaks
# are detected downstream, in the seam list `resolve_admin_shares()`
# builds.
#
# Usage
# -----
#   source("inst/scripts/prepare_admin_shares_pin.R")
#   result <- prepare_admin_shares_pin()                 # families only
#   result <- prepare_admin_shares_pin(sources = list(
#     JRC_subnational_crops = jrc_rows
#   ))
# -----------------------------------------------------------------------

ADMIN_SHARES_ALIAS <- "admin-shares"

ADMIN_SHARES_STAGING_DIR <- file.path("inst", "scripts", "pin_upload")

ADMIN_SHARES_MANIFEST_PATH <- file.path(
  "inst",
  "extdata",
  "admin_stats_pins_manifest.csv"
)

# The granted administrative depth; see "The `level` convention" above.
ADMIN_SHARES_LEVEL <- 1L

# ---- Vocabularies -----------------------------------------------------

.shares_families <- function() {
  whep:::.admin_family_aliases()
}

.shares_indicators <- function() {
  c(
    "area_harvested",
    "area_planted_or_sown",
    "area_main",
    "area_cultivated",
    "production",
    "yield"
  )
}

.shares_grains <- function() {
  c("admin1", "admin2", "admin3")
}

# The tier-1 sources this script knows how to place, with the T11
# vocabulary that maps their classes and the rule that gives each row its
# container. The livestock readers (`Eurostat_apro_mt_ls_r`,
# `Eurostat_ef_lsk_poultry`, `IBGE_PPM`) are deliberately absent: they
# serve head counts only, which are outside this schema by design.
#
# Every label here must also be DECLARED in
# `whep:::.admin_source_registry()`, or the rows it places would be
# staged into a pin `read_admin_shares()` refuses as an undeclared
# source. `test_admin_shares_pins.R` pins that.
.shares_tier1_spec <- function() {
  tibble::tribble(
    ~source,                 ~vocabulary,            ~container, ~iso3,
    "USDA_NASS",             "admin_items_nass",     "fixed",    "USA",
    "IBGE_PAM",              "admin_items_sidra",    "fixed",    "BRA",
    "Eurostat_apro_cpshr",   "admin_items_eurostat", "nuts",     NA,
    "Eurostat_apro_cpnhr_h", "admin_items_eurostat", "nuts",     NA,
    "JRC_subnational_crops", "admin_items_jrc",      "nuts",     NA
  )
}

# The two-letter country codes NUTS uses, mapped to ISO 3166-1 alpha-3.
# Two of them are not ISO 3166-1 alpha-2: Eurostat codes Greece `EL` and
# the United Kingdom `UK`. Source: Eurostat, "NUTS - Nomenclature of
# territorial units for statistics", country codes table.
.shares_nuts_iso3 <- function() {
  tibble::tribble(
    ~prefix, ~iso3,
    "AT", "AUT", "BE", "BEL", "BG", "BGR", "CH", "CHE",
    "CY", "CYP", "CZ", "CZE", "DE", "DEU", "DK", "DNK",
    "EE", "EST", "EL", "GRC", "ES", "ESP", "FI", "FIN",
    "FR", "FRA", "HR", "HRV", "HU", "HUN", "IE", "IRL",
    "IS", "ISL", "IT", "ITA", "LI", "LIE", "LT", "LTU",
    "LU", "LUX", "LV", "LVA", "ME", "MNE", "MK", "MKD",
    "MT", "MLT", "NL", "NLD", "NO", "NOR", "PL", "POL",
    "PT", "PRT", "RO", "ROU", "RS", "SRB", "SE", "SWE",
    "SI", "SVN", "SK", "SVK", "TR", "TUR", "UK", "GBR"
  )
}

# ---- Reading the family half ------------------------------------------

# Where one family's rows come from, and by which route. Registered on the
# board wins; a staged folder is the fallback and is REPORTED as one, so a
# run can never look like it read the board when it read a local folder.
.shares_family_input <- function(alias, staging_dir) {
  if (whep:::.admin_family_registered(alias)) {
    loaded <- whep:::read_admin_family(alias)
    rows <- loaded[[alias]]
    if (!is.null(rows) && nrow(rows) > 0) {
      return(list(rows = tibble::as_tibble(rows), route = "pin"))
    }
  }
  staged <- .shares_staged_rows(alias, staging_dir)
  if (!is.null(staged)) {
    return(list(rows = staged, route = "staged"))
  }
  list(rows = NULL, route = "absent")
}

# The newest staged version of one alias, read from the parquet
# `stage_admin_pin()` wrote.
.shares_staged_rows <- function(alias, staging_dir) {
  folder <- file.path(staging_dir, alias)
  if (!dir.exists(folder)) {
    return(NULL)
  }
  versions <- sort(list.dirs(folder, recursive = FALSE, full.names = FALSE))
  if (length(versions) == 0) {
    return(NULL)
  }
  path <- file.path(
    folder,
    versions[[length(versions)]],
    paste0(alias, ".parquet")
  )
  if (!file.exists(path)) {
    return(NULL)
  }
  tibble::as_tibble(nanoparquet::read_parquet(path))
}

# Staged folders that are not one of the five families. Named, never read:
# the consent manifest is about the in-house families, and gating a public
# product against it would report the wrong reason.
.shares_foreign_staged <- function(staging_dir) {
  if (!dir.exists(staging_dir)) {
    return(character(0))
  }
  staged <- list.dirs(staging_dir, recursive = FALSE, full.names = FALSE)
  setdiff(setdiff(staged, .shares_families()), ADMIN_SHARES_ALIAS)
}

# ---- The consent manifest ---------------------------------------------

# `utils::read.csv()`, not `data.table::fread()`: `attribution` is prose
# with embedded commas and quotes, and a `fread()` round trip doubles an
# escaped quote silently.
.shares_read_manifest <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "No consent manifest at {.file {path}}.",
      i = "It is written by
           {.file inst/scripts/prepare_admin_stats_pins.R}."
    ))
  }
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}

# ---- Containers -------------------------------------------------------

# ISO3 to the FAOSTAT-style area code of the container. An ISO3 resolving
# to more than one code aborts rather than picking one: `regions_full`
# carries two codes for ETH and SDN (the former and the current polity),
# and silently choosing either would misattribute a whole country.
.shares_area_codes <- function(iso3) {
  wanted <- unique(iso3[!is.na(iso3)])
  lookup <- whep::regions_full |>
    dplyr::filter(.data$iso3c %in% wanted) |>
    dplyr::distinct(.data$iso3c, .data$code)
  ambiguous <- lookup |>
    dplyr::count(.data$iso3c) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(ambiguous) > 0) {
    cli::cli_abort(c(
      "{nrow(ambiguous)} ISO3 code{?s} resolve to more than one
       {.field area_code}: {.val {ambiguous$iso3c}}.",
      i = "Name the container explicitly rather than letting one be
           picked."
    ))
  }
  as.integer(lookup$code[match(iso3, lookup$iso3c)])
}

# The ISO3 a family unit id declares, which is its prefix before the first
# hyphen. `NA` where the id carries none.
.shares_family_iso3 <- function(ids) {
  prefix <- sub("-.*$", "", ids)
  dplyr::if_else(nchar(prefix) == 3L & prefix != ids, prefix, NA_character_)
}

# The ISO3 a NUTS-shaped id declares, through its two-letter prefix.
.shares_nuts_iso3_of <- function(ids) {
  map <- .shares_nuts_iso3()
  prefix <- toupper(substr(ids, 1L, 2L))
  ok <- nchar(ids) >= 2L & grepl("^[A-Za-z]{2}", ids)
  dplyr::if_else(ok, map$iso3[match(prefix, map$prefix)], NA_character_)
}

# ---- One tier-2/3 family onto the contract ----------------------------

.shares_from_family <- function(rows, alias, tier) {
  # WHICH family these rows are is read off their own `source` column and
  # resolved through the registry, on EVERY route. The pin route has just
  # had this proved by `read_admin_family()`; the staged route had not,
  # and the tibble below rewrites `source` to `alias`, so a folder named
  # `admin-stats-japan` holding the Latin American panel's rows -- values
  # attached -- was relabelled Japan and staged as source values, with the
  # reader's gate then seeing nothing but Japan. The column closure of
  # `.admin_family_check()` is deliberately NOT applied here: the tibble
  # below carries the declared measure and nothing else, so an undeclared
  # column cannot reach the contract from this route, and `lane` has to
  # stay admissible for `.shares_drop_lanes()`.
  whep:::.admin_family_check_source(rows, alias)
  # The measurement a family ships is DECLARED, by the same constant the
  # reader's gate reads (`whep:::.admin_family_measure()`), not inferred
  # from whichever column the pin happens to carry. Inferring it made the
  # builder's notion of what a family ships differ from the gate's: a
  # shares-only pin that had lost its `share` column and grown a `value`
  # one was staged as source values, and a value family that grew a
  # `share` column was staged shares-only.
  measure <- whep:::.admin_family_measure(alias)
  if (!rlang::has_name(rows, measure)) {
    cli::cli_abort(c(
      "The {.val {alias}} pin carries no {.field {measure}} column.",
      i = "That family is consented to ship {.field {measure}}; the
           permission is recorded in
           {.file inst/extdata/admin_stats_pins_manifest.csv}.",
      i = "Rebuild the family pin with
           {.file inst/scripts/prepare_admin_stats_pins.R}."
    ))
  }
  # The T23 boundary on the staged route, the way `read_admin_family()`
  # enforces it on the pin route: a `value` column in the shares-only
  # family's folder is refused as the breach it is, not quietly left
  # behind by the measure selection below.
  whep:::.admin_family_check_consent(rows, alias)
  lane <- .shares_drop_lanes(rows, alias)
  rows <- lane$rows
  staged <- tibble::tibble(
    source = alias,
    source_native_id = as.character(rows$source_native_unit_id),
    source_native_name = as.character(rows$source_native_unit_name),
    item_code = as.character(rows$source_native_item_code),
    indicator_used = as.character(rows$indicator_used),
    year = as.integer(rows$year),
    value = if (measure == "value") as.numeric(rows$value) else NA_real_,
    share = if (measure == "share") as.numeric(rows$share) else NA_real_,
    tier = as.integer(tier),
    grain = as.character(rows$grain),
    nuts_version = as.character(rows$nuts_version),
    source_id = alias,
    source_version = as.character(rows$source_version),
    recorded_at = as.character(rows$recorded_at),
    value_flag = as.character(rows$value_flag)
  )
  staged$iso3 <- .shares_family_iso3(staged$source_native_id)
  .shares_common_filters(
    staged,
    counts = list(
      alias = alias,
      tier = tier,
      rows_read = lane$rows_read,
      dropped_lane = lane$n,
      lane_examples = lane$lanes
    )
  )
}

# Only a directly transcribed statistic is an observation. A family pin
# carrying a `lane` column is checked against that; one that does not
# carry it -- which is every family staged on 2026-09-03 -- passes through
# untouched, and the header above records what was measured on the panel
# instead. See "`treatment_year` and the estimation lane".
.shares_drop_lanes <- function(rows, alias) {
  if (!"lane" %in% names(rows)) {
    return(list(
      rows = rows,
      rows_read = nrow(rows),
      n = 0L,
      lanes = NA_character_
    ))
  }
  other <- !rows$lane %in% "observational"
  if (any(other)) {
    cli::cli_warn(c(
      "{sum(other)} row{?s} of {alias} are not
       {.val observational}: {.val {unique(rows$lane[other])}}.",
      i = "They are dropped and counted. A balanced or modelled series is
           not a transcribed statistic."
    ))
  }
  list(
    rows = rows[!other, ],
    rows_read = nrow(rows),
    n = sum(other),
    lanes = .shares_examples(rows$lane[other])
  )
}

# ---- One tier-1 source onto the contract ------------------------------

# One column of a frame that may not carry it, as a typed all-`NA` vector
# where it does not. An injected tier-1 source need only carry the columns
# it actually has, and the vocabulary join it goes through returns what it
# was given; a bare `$` then emits tibble's "Unknown or uninitialised
# column" warning on the legitimate path where every row was dropped, and
# `R CMD check` surfaces it. A column the CONTRACT requires is still
# refused -- by `.admin_shares_check_pin()`, on its own terms, rather than
# by a warning here.
.shares_column <- function(rows, column) {
  if (!rlang::has_name(rows, column)) {
    return(rep(NA, nrow(rows)))
  }
  rows[[column]]
}

.shares_from_tier1 <- function(rows, label) {
  spec <- .shares_tier1_spec() |> dplyr::filter(.data$source == !!label)
  if (nrow(spec) != 1L) {
    # Computed before the call, not inside it: a `{}` expression starting
    # with a dot is a cli STYLE, not an R expression, since cli 3.4.0, so
    # interpolating it destroyed this message and the list it names.
    known <- .shares_tier1_spec()$source
    cli::cli_abort(c(
      "No tier-1 definition for source {.val {label}}.",
      i = "Known: {.val {known}}."
    ))
  }
  # The rows' own `source` must resolve to the label they are injected
  # under: the tibble below rewrites `source` to that label, and a rewrite
  # that never read the column let rows naming the Latin American panel
  # travel as `IBGE_PAM` with their values attached. The column is
  # REQUIRED, unlike the rest of the reader shape `.shares_column()`
  # tolerates missing: every tier-1 reader emits it, and an injection
  # without it names no producer, so the label alone would decide what
  # the rows are -- the alias-only gate this rule replaced.
  whep:::.admin_family_check_source(rows, label)
  # The container is resolved FIRST, before the vocabulary join, so that a
  # unit id carrying no country code is counted and named under its own
  # cause. JRC's three Excel-mangled region codes (`2-D`, `4-D`, `5-D`,
  # served as `2-Dec`, `4-Dec`, `5-Dec`) happen to appear only on rows of
  # a class the vocabulary drops anyway, so leaving this until later would
  # bury four broken identifiers inside 170,608 ordinary vocabulary drops.
  rows$iso3 <- if (spec$container == "fixed") {
    spec$iso3
  } else {
    .shares_nuts_iso3_of(rows$source_native_unit_id)
  }
  placeless <- is.na(rows$iso3)
  mapped <- .shares_map_vocabulary(rows[!placeless, ], spec)
  kept <- mapped$rows
  staged <- tibble::tibble(
    source = label,
    source_native_id = as.character(
      .shares_column(kept, "source_native_unit_id")
    ),
    source_native_name = as.character(
      .shares_column(kept, "source_native_unit_name")
    ),
    item_code = as.character(.shares_column(kept, "item_prod_code")),
    indicator_used = as.character(.shares_column(kept, "indicator_used")),
    year = as.integer(.shares_column(kept, "year")),
    value = as.numeric(.shares_column(kept, "value")),
    share = NA_real_,
    tier = 1L,
    grain = as.character(.shares_column(kept, "grain")),
    nuts_version = as.character(.shares_column(kept, "nuts_version")),
    source_id = label,
    source_version = as.character(.shares_column(kept, "source_version")),
    recorded_at = as.character(.shares_column(kept, "recorded_at")),
    value_flag = as.character(.shares_column(kept, "value_flag"))
  )
  staged$iso3 <- as.character(.shares_column(kept, "iso3"))
  .shares_common_filters(
    staged,
    counts = c(
      list(
        alias = label,
        tier = 1L,
        rows_read = nrow(rows),
        dropped_no_country_code = sum(placeless),
        country_code_examples = .shares_examples(
          rows$source_native_unit_id[placeless]
        )
      ),
      mapped$counts
    )
  )
}

# The T11 vocabulary join. `class_key` keys a publisher's class by its
# code where it issues one and by its name where it does not, which is
# exactly `source_native_item_code` falling back to
# `source_native_item_name`.
.shares_map_vocabulary <- function(rows, spec) {
  vocabulary <- get(spec$vocabulary, envir = asNamespace("whep"))
  keyed <- rows |>
    dplyr::mutate(
      class_key = dplyr::coalesce(
        .data$source_native_item_code,
        .data$source_native_item_name
      )
    ) |>
    dplyr::left_join(
      dplyr::select(
        vocabulary,
        "class_key",
        "item_prod_code",
        "mapping_kind"
      ),
      by = "class_key",
      relationship = "many-to-one"
    )
  unmapped <- is.na(keyed$mapping_kind)
  if (any(unmapped)) {
    cli::cli_warn(c(
      "{sum(unmapped)} row{?s} of {.val {spec$source}} carry a class
       {.field {spec$vocabulary}} does not know.",
      x = "Class{?es}: {.val {unique(keyed$class_key[unmapped])}}.",
      i = "They are dropped and counted; a served class the vocabulary
           misses is a gap in T11, not an ordinary drop."
    ))
  }
  kept <- keyed[!unmapped & keyed$mapping_kind %in% c("exact", "aggregate"), ]
  summed <- .shares_sum_members(
    keyed[
      !unmapped & keyed$mapping_kind == "sum_member",
    ],
    vocabulary
  )
  list(
    rows = dplyr::bind_rows(kept, summed$rows),
    counts = list(
      dropped_unmapped_class = sum(unmapped),
      dropped_vocabulary = sum(keyed$mapping_kind %in% c("member", "dropped")),
      dropped_sum_member_yield = summed$dropped_yield,
      dropped_sum_member_partial = summed$dropped_partial,
      merged_sum_member = summed$merged
    )
  )
}

# `sum_member` classes sum to their WHEP target, the publisher shipping no
# aggregate for them. They are summed here ONLY where every declared
# member is present for that unit, indicator and year: a partial sum would
# understate the target with nothing on the row to show it. Yields are
# never summed.
.shares_sum_members <- function(rows, vocabulary) {
  empty <- rows[0, ]
  if (nrow(rows) == 0) {
    return(list(
      rows = empty,
      dropped_yield = 0L,
      dropped_partial = 0L,
      merged = 0L
    ))
  }
  yields <- rows$indicator_used %in% "yield"
  keys <- c(
    "source_native_unit_id",
    "source_native_unit_name",
    "iso3",
    "item_prod_code",
    "indicator_used",
    "year",
    "grain",
    "nuts_version",
    "source_version",
    "recorded_at"
  )
  expected <- vocabulary |>
    dplyr::filter(.data$mapping_kind == "sum_member") |>
    dplyr::count(.data$item_prod_code, name = "n_expected")
  grouped <- rows[!yields, ] |>
    dplyr::summarise(
      value = sum(.data$value),
      value_flag = .shares_join_flags(.data$value_flag),
      n_present = dplyr::n_distinct(.data$class_key),
      n_rows = dplyr::n(),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::left_join(expected, by = "item_prod_code")
  complete <- grouped$n_present == grouped$n_expected
  list(
    rows = dplyr::select(
      grouped[complete, ],
      -"n_present",
      -"n_expected",
      -"n_rows"
    ),
    dropped_yield = sum(yields),
    dropped_partial = sum(grouped$n_rows[!complete]),
    # Rows absorbed by the summation. Not a drop -- their quantity is in
    # the summed row -- but a row-count reduction the reconciliation has
    # to know about, or it would read as a filter with no counter.
    merged = sum(grouped$n_rows[complete]) - sum(complete)
  )
}

.shares_join_flags <- function(flags) {
  kept <- unique(flags[!is.na(flags) & nzchar(flags)])
  if (length(kept) == 0) {
    return(NA_character_)
  }
  stringr::str_c(sort(kept), collapse = "; ")
}

# ---- The filters every source goes through ----------------------------

.shares_common_filters <- function(staged, counts) {
  counts$rows_in <- nrow(staged)
  step <- .shares_drop(staged, is.na(staged$indicator_used))
  counts$dropped_headcount <- step$n
  step <- .shares_drop(
    step$rows,
    !step$rows$indicator_used %in% .shares_indicators()
  )
  counts$dropped_indicator <- step$n
  valid_items <- whep::items_prod_full$item_prod_code
  step <- .shares_drop(step$rows, !step$rows$item_code %in% valid_items)
  counts$dropped_item <- step$n
  # The container is resolved BEFORE the grain filter so that a unit id
  # carrying no country at all is attributed to the right cause and named.
  # JRC's Excel-mangled region codes have neither a country prefix nor a
  # grain, and reversing these two steps would bury them in a grain count.
  unresolved <- is.na(step$rows$iso3)
  counts$container_examples <- .shares_examples(
    step$rows$source_native_id[unresolved]
  )
  step <- .shares_drop(step$rows, unresolved)
  counts$dropped_container <- step$n
  ungrained <- !step$rows$grain %in% .shares_grains()
  counts$grain_examples <- .shares_examples(
    step$rows$source_native_id[ungrained]
  )
  step <- .shares_drop(step$rows, ungrained)
  counts$dropped_grain <- step$n
  step <- .shares_drop(
    step$rows,
    is.na(step$rows$value) & is.na(step$rows$share)
  )
  counts$dropped_no_measure <- step$n
  counts$rows_out <- nrow(step$rows)
  .shares_check_counts(counts)
  list(rows = .shares_contract(step$rows), counts = counts)
}

# Every row read is either kept or counted in exactly one drop. Asserting
# it stops a filter being added without a counter -- the failure mode
# where a report says a source contributed all it had while a step quietly
# removed part of it. `rows_read` is what the source handed over;
# `rows_in` is what reached the common filters, which for a tier-1 source
# is already past its vocabulary join.
.shares_check_counts <- function(counts) {
  drops <- sum(unlist(counts[grepl("^dropped_", names(counts))]))
  merged <- counts$merged_sum_member %||% 0L
  if (counts$rows_read == counts$rows_out + drops + merged) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{counts$alias}: {counts$rows_read} row{?s} read do not reconcile
     with {drops} dropped, {merged} merged and {counts$rows_out} kept.",
    i = "A filter without a counter, or a counter counted twice."
  ))
}

.shares_drop <- function(rows, drop) {
  drop <- !is.na(drop) & drop
  list(rows = rows[!drop, ], n = sum(drop))
}

.shares_examples <- function(ids, max_ids = 5L) {
  unique_ids <- sort(unique(ids[!is.na(ids)]))
  if (length(unique_ids) == 0) {
    return(NA_character_)
  }
  shown <- utils::head(unique_ids, max_ids)
  stringr::str_c(shown, collapse = "; ")
}

# The 20 contract columns, in contract order. `level_polity_code` stays
# `NA`: the pin is source-native and resolution is the caller's step
# through `resolve_admin_units()`.
.shares_contract <- function(rows) {
  tibble::tibble(
    area_code = .shares_area_codes(rows$iso3),
    level_polity_code = NA_character_,
    level = ADMIN_SHARES_LEVEL,
    item_prod_code = as.integer(rows$item_code),
    indicator_used = rows$indicator_used,
    year = rows$year,
    value = rows$value,
    share = rows$share,
    source = rows$source,
    tier = rows$tier,
    grain = rows$grain,
    concept_break = FALSE,
    nuts_version = rows$nuts_version,
    source_native_id = rows$source_native_id,
    source_native_name = rows$source_native_name,
    source_id = rows$source_id,
    source_version = rows$source_version,
    recorded_at = rows$recorded_at,
    treatment_year = "observed",
    value_flag = rows$value_flag
  )
}

# ---- Assembly ---------------------------------------------------------

#' Assemble every family and injected tier-1 source onto the contract.
#'
#' Returns the assembled rows, one report row per source and the list of
#' sources excluded by the consent gate.
assemble_admin_shares <- function(
  sources = NULL,
  staging_dir = ADMIN_SHARES_STAGING_DIR,
  manifest_path = ADMIN_SHARES_MANIFEST_PATH
) {
  manifest <- .shares_read_manifest(manifest_path)
  families <- .shares_families() |>
    rlang::set_names() |>
    purrr::map(\(alias) {
      .shares_one_family(alias, staging_dir, manifest)
    })
  tier1 <- .shares_tier1_sources(sources)
  built <- c(families, tier1)
  rows <- built |>
    purrr::map(\(one) one$rows) |>
    purrr::compact() |>
    purrr::list_rbind()
  list(
    rows = rows,
    report = built |>
      purrr::map(\(one) .shares_report_row(one$counts)) |>
      purrr::list_rbind(),
    excluded = built |>
      purrr::keep(\(one) !is.null(one$excluded)) |>
      purrr::map(\(one) one$excluded) |>
      purrr::list_rbind(),
    foreign_staged = .shares_foreign_staged(staging_dir)
  )
}

.shares_one_family <- function(alias, staging_dir, manifest) {
  granted <- alias %in% manifest$alias
  input <- if (granted) {
    .shares_family_input(alias, staging_dir)
  } else {
    list(rows = NULL, route = "excluded_no_consent")
  }
  if (is.null(input$rows)) {
    return(list(
      rows = NULL,
      counts = list(alias = alias, route = input$route),
      excluded = tibble::tibble(
        source = alias,
        reason = if (granted) "not_available" else "no_consent_manifest_row",
        detail = if (granted) {
          "neither registered on the board nor staged locally"
        } else {
          "absent from the consent manifest (T23)"
        }
      )
    ))
  }
  tier <- as.integer(manifest$tier[match(alias, manifest$alias)])
  built <- .shares_from_family(input$rows, alias, tier)
  built$counts$route <- input$route
  list(
    rows = built$rows,
    counts = built$counts,
    excluded = .shares_zero_row_note(alias, built$rows, built$counts)
  )
}

# A source that was read, gated and then filtered down to nothing is an
# exclusion too, and it is named together with the filter that took it.
# Otherwise `admin-stats-france-livestock` -- 58,740 head counts, none of
# which this schema admits -- would leave no trace but a zero in a counts
# table nobody reads twice.
#
# The reason is `"no_rows_in_pin"`, the same word `read_admin_shares()`
# uses for the same state, so that the build-time report and the
# caller-visible one can be read side by side. Only the DETAIL differs,
# and only because it can: this report knows which filter took the rows
# and the reader cannot. The one reason with no counterpart at read time
# is `"not_available"` in `.shares_one_family()`.
.shares_zero_row_note <- function(alias, rows, counts) {
  if (nrow(rows) > 0) {
    return(NULL)
  }
  drops <- unlist(counts[grepl("^dropped_", names(counts))])
  worst <- names(drops)[which.max(drops)]
  tibble::tibble(
    source = alias,
    reason = "no_rows_in_pin",
    detail = stringr::str_c(
      counts$rows_read,
      " row(s) read, all dropped; largest drop ",
      worst,
      " = ",
      max(drops)
    )
  )
}

.shares_tier1_sources <- function(sources) {
  if (is.null(sources) || length(sources) == 0) {
    return(list())
  }
  if (is.null(names(sources)) || any(!nzchar(names(sources)))) {
    cli::cli_abort(
      "{.arg sources} must be a list named by source label."
    )
  }
  names(sources) |>
    rlang::set_names() |>
    purrr::map(\(label) {
      built <- .shares_from_tier1(tibble::as_tibble(sources[[label]]), label)
      built$counts$route <- "injected"
      list(
        rows = built$rows,
        counts = built$counts,
        excluded = .shares_zero_row_note(label, built$rows, built$counts)
      )
    })
}

.shares_report_row <- function(counts) {
  zeros <- list(
    tier = NA_integer_,
    route = NA_character_,
    rows_read = 0L,
    rows_in = 0L,
    rows_out = 0L,
    dropped_lane = 0L,
    dropped_no_country_code = 0L,
    dropped_unmapped_class = 0L,
    dropped_vocabulary = 0L,
    dropped_sum_member_yield = 0L,
    dropped_sum_member_partial = 0L,
    merged_sum_member = 0L,
    dropped_headcount = 0L,
    dropped_indicator = 0L,
    dropped_item = 0L,
    dropped_container = 0L,
    dropped_grain = 0L,
    dropped_no_measure = 0L,
    lane_examples = NA_character_,
    country_code_examples = NA_character_,
    container_examples = NA_character_,
    grain_examples = NA_character_
  )
  utils::modifyList(zeros, counts) |>
    tibble::as_tibble() |>
    dplyr::relocate("alias", "tier", "route", "rows_read", "rows_out")
}

# ---- Staging and manifest ---------------------------------------------

# The two files `create_version()` in `prepare_upload.R` writes, in the
# same `<alias>/<version>/` layout `prepare_admin_stats_pins.R` stages
# into. Nothing is uploaded.
stage_admin_shares_pin <- function(
  data,
  staging_dir = ADMIN_SHARES_STAGING_DIR,
  version = NULL
) {
  version <- version %||% format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  target <- file.path(staging_dir, ADMIN_SHARES_ALIAS, version)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(
    target,
    paste0(ADMIN_SHARES_ALIAS, c(".csv", ".parquet"))
  )
  readr::write_csv(data, paths[[1L]])
  nanoparquet::write_parquet(data, paths[[2L]])
  cli::cli_alert_success("{ADMIN_SHARES_ALIAS}: staged at {.file {target}}")
  tibble::tibble(
    alias = ADMIN_SHARES_ALIAS,
    pin_version = version,
    parquet_file = basename(paths[[2L]]),
    bytes = as.numeric(unname(file.info(paths[[2L]])$size)),
    md5 = unname(tools::md5sum(paths[[2L]]))
  )
}

# The assembled pin's own manifest row, carrying every permission its
# rows travel under. Tier is NA because the pin spans tiers 1 to 3, and
# `measure` says that both measurement columns are in use.
#
# A consented family that contributed NO row is named too. Listing only
# the contributing families left a manifest reader unable to tell a fifth
# consented family that was read and dropped from one that was never
# consented at all -- and `admin-stats-france-livestock` is exactly that
# case, permanently: it ships head counts, which this contract does not
# carry.
.shares_attribution <- function(rows, manifest) {
  used <- sort(unique(rows$source))
  in_house <- intersect(used, manifest$alias)
  public <- setdiff(used, manifest$alias)
  silent <- setdiff(intersect(manifest$alias, .shares_families()), used)
  parts <- c(
    stringr::str_c(
      "assembled admin-shares; in-house families and their recorded ",
      "permissions: ",
      stringr::str_c(
        in_house,
        " -- ",
        manifest$attribution[match(in_house, manifest$alias)],
        collapse = " | "
      )
    ),
    if (length(public) > 0) {
      stringr::str_c(
        "public tier-1 sources under their own licences: ",
        stringr::str_c(public, collapse = ", ")
      )
    },
    if (length(silent) > 0) {
      stringr::str_c(
        "consented in-house families read but contributing no row: ",
        stringr::str_c(silent, collapse = ", "),
        " (the filter that took each is in this script's per-source ",
        "report, the reason in read_admin_shares()$excluded)"
      )
    }
  )
  stringr::str_c(parts, collapse = "; ")
}

# Append (or replace) the assembled pin's row in the shared manifest, so a
# re-run updates its row rather than adding a second one.
write_admin_shares_manifest <- function(
  rows,
  staged,
  manifest,
  path = ADMIN_SHARES_MANIFEST_PATH,
  retrieved_at
) {
  # Every summary is computed before the tibble is built: `tibble()`
  # evaluates its arguments in order and each one sees the columns the
  # earlier ones made, so a column named `rows` would shadow the argument
  # `rows` for every expression after it.
  n_rows <- nrow(rows)
  n_units <- dplyr::n_distinct(rows$source_native_id)
  years <- range(rows$year)
  attribution <- .shares_attribution(rows, manifest)
  entry <- tibble::tibble(
    alias = ADMIN_SHARES_ALIAS,
    tier = NA_integer_,
    measure = "value|share",
    rows = n_rows,
    units = n_units,
    year_min = years[[1L]],
    year_max = years[[2L]],
    attribution = attribution,
    pin_version = staged$pin_version,
    parquet_file = staged$parquet_file,
    bytes = staged$bytes,
    md5 = staged$md5,
    retrieved_at = retrieved_at
  )
  out <- manifest |>
    dplyr::filter(.data$alias != ADMIN_SHARES_ALIAS) |>
    dplyr::bind_rows(entry)
  readr::write_csv(out, path)
  cli::cli_alert_success("{ADMIN_SHARES_ALIAS}: manifest row at {.file {path}}")
  entry
}

# ---- Entry point ------------------------------------------------------

#' Assemble, validate and stage the `admin-shares` pin.
#'
#' Uploads nothing and does not run `data-raw/whep_inputs.R`. Returns the
#' assembled rows, the per-source report, the excluded sources and the
#' manifest row.
prepare_admin_shares_pin <- function(
  sources = NULL,
  staging_dir = ADMIN_SHARES_STAGING_DIR,
  manifest_path = ADMIN_SHARES_MANIFEST_PATH,
  stage = TRUE
) {
  assembled <- assemble_admin_shares(sources, staging_dir, manifest_path)
  rows <- assembled$rows
  if (nrow(rows) == 0) {
    cli::cli_abort(c(
      "No source contributed a row to {.val {ADMIN_SHARES_ALIAS}}.",
      i = "Check the per-family routes; a family reported {.val absent}
           is neither registered nor staged."
    ))
  }
  # The reader's own gate, not just its schema: a pin that
  # `read_admin_shares()` would refuse must not be staged in the first
  # place. It carries the T23 boundary (a shares-only family's rows must
  # be value-free), the consent gate, the unresolved-polity rule and the
  # measurement rules, and it is given the manifest this run is writing
  # into rather than the installed copy.
  whep:::.admin_shares_check_pin(rows, .shares_read_manifest(manifest_path))
  .shares_print_report(assembled)
  if (!stage) {
    return(c(assembled, list(manifest = NULL)))
  }
  staged <- stage_admin_shares_pin(rows, staging_dir)
  entry <- write_admin_shares_manifest(
    rows,
    staged,
    .shares_read_manifest(manifest_path),
    manifest_path,
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  .shares_print_next_steps(staged)
  c(assembled, list(staged = staged, manifest = entry))
}

.shares_print_report <- function(assembled) {
  cli::cli_h2("admin-shares: per-source assembly")
  print(as.data.frame(assembled$report), row.names = FALSE)
  cli::cli_h2("admin-shares: assembled")
  cli::cli_alert_info(c(
    "{nrow(assembled$rows)} row{?s},
     {dplyr::n_distinct(assembled$rows$source_native_id)} unit{?s},
     {min(assembled$rows$year)}-{max(assembled$rows$year)},
     {dplyr::n_distinct(assembled$rows$source)} source{?s}."
  ))
  if (nrow(assembled$excluded) > 0) {
    cli::cli_h3("Excluded, and named")
    print(as.data.frame(assembled$excluded), row.names = FALSE)
  }
  if (length(assembled$foreign_staged) > 0) {
    cli::cli_alert_warning(c(
      "Staged but not a family, so not read here:
       {.val {assembled$foreign_staged}}.",
      "i" = "A public tier-1 product enters through {.arg sources}."
    ))
  }
}

.shares_print_next_steps <- function(staged) {
  cli::cli_alert_info(c(
    "Nothing uploaded. To publish: upload the staged folder to the board,
     add {.val {paste0(ADMIN_SHARES_ALIAS, '/', staged$pin_version, '/')}}
     under that alias's section of {.file _pins.yaml}, add the proposed row
     to {.file inst/extdata/whep_inputs.csv} and rebuild with
     {.file data-raw/whep_inputs.R}. The user authorises registration."
  ))
}
