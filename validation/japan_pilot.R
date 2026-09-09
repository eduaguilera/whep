# The Japan depth-1 pilot, run end to end on real data (issue #1000).
#
# ## What is measured
#
# One constrained spatialization: Japan's national crop totals split across
# its 46 prefectures by MAFF's own reported areas, allocated onto the
# 0.5-degree grid, and every diagnostic the depth chain produces read back
# off disk. It is the first run in which `run_spatialize(level = 1)` meets
# real administrative statistics rather than a fixture, so what it exists to
# prove is that the chain -- allocation layer, `read_admin_shares()`,
# `resolve_admin_units()`, `resolve_admin_shares()`,
# `allocate_level_crops()`, `reconcile_admin_allocation()`, `seam_gate()` --
# reaches the same number a hand computation reaches, and says so out loud
# when it does not.
#
# THE ANCHOR. Hokkaido, paddy rice (`item_prod_code` 27), year 2000. MAFF
# reports 134,900 ha there; the 46 prefectures report 1,762,002 ha between
# them; FAOSTAT's national total for that year is 1,770,000 ha. Coverage is
# complete, so no residual unit is raised and the reported units rescale
# proportionally: 134,900 * 1,770,000 / 1,762,002 = 135,512.33 ha. That
# number was produced by an earlier investigation and is pinned below as
# `.jp_anchor`. This script recomputes it from the pin and the engine and
# reports the relative difference; a mismatch is a FINDING to be explained,
# never a tolerance to widen.
#
# ## What is approximate here, and why
#
# Said in the script rather than in a commit message, because a reader of the
# numbers below needs all four:
#
#  1. THE SUPPORT IS JAPAN-ONLY. `WHEP_POLYCELL_SUPPORT_PATH` points at a
#     pilot polycell support holding the 46 prefectures and nothing else --
#     504 polycells over 305 cells. A world support would carry every
#     country's level-0 row beside them; this one carries no `area_code` at
#     all, so `read_level_country_grid(level = 0, "year_aware")` returns ZERO
#     rows against it and the allocation layer is the depth grid alone. Every
#     layer row is therefore reported as `unit_outside_level0` ragged
#     coverage, and every country other than Japan would be dropped from the
#     engine for having no cell. The pilot scopes its inputs to container 110
#     and the six items the MAFF family ships rather than letting ~200
#     countries be dropped with a warning: that changes nothing about
#     Japan's answer, because no non-Japanese cell exists to compete for.
#     On a world support neither the ragged rows nor the scoping would be
#     there, so both are the pilot's numbers, not the pipeline's.
#  2. THE REGISTERED WORLD PIN IS UNSOUND (whep#1010). `polycell_support` as
#     registered ships ZERO inland water and zero ice: 533 Mha of lakes and
#     glaciers booked as land, invisible to `territory == land + water + ice`
#     because zero satisfies it. The sound pin is version
#     `20260825T102349Z-1a0eb`. The pilot support was built with the water
#     layer applied -- land 36,995,862.69 -> 36,685,203.95 ha, inland water
#     310,658.74 ha (0.84%), ice 0 on all 504 polycells -- so this run does
#     not inherit that defect, but neither can it be compared against a world
#     run that does.
#  3. THE ALIAS ROWS ARE INJECTED. Resolving `JPN-HOKKAIDO` to
#     `JPN-HOKKAIDO-1871-2025` needs alias rows scoped to the code system
#     `whep-lab-japan`, and those rows are a whep-polities deliverable that
#     has NOT been published: `polity_label_aliases` ships 1,007 rows over 15
#     sources and none of them is a code system `resolve_admin_units()`
#     names. Without them every administrative unit resolves to `NA` and the
#     run aborts with `whep_run_admin_unresolved`. This script builds the
#     46-row identity map (`polity_code = <source_native_id>-1871-2025`,
#     every one of which already exists in `polities`) and installs it into
#     the package namespace, so the PRODUCTION call path resolves it:
#     `run_spatialize()` reaches `resolve_admin_units()` through
#     `.admin_resolve_units()`, which passes no `aliases` argument and reads
#     the package table. Injecting the table rather than mocking the resolver
#     is deliberate -- it exercises `.alias_route_one()`'s scoping and
#     specificity rules for real, which is the route the published rows will
#     take.
#
#     THE SLUG IS THE CODE SYSTEM, NOT THE SOURCE. The alias rows' `source`
#     must be `"whep-lab-japan"`. Writing the pin's own `source` value,
#     `"admin-stats-japan"`, resolves 0 of 32,095 rows:
#     `resolve_admin_units()` builds the slug from `code_system` and never
#     reads the rows' own `source` column when matching. Measured both ways;
#     the script asserts it below so the trap cannot come back silently.
#  4. TIER A IS VACUOUS ON THIS FAMILY. MAFF ships values and no share, so
#     the share the gate judges is the source's own value over the group
#     total. Tier A checks a reported share against that same re-derivation,
#     so it compares a quantity with itself. Tier B and tier C are
#     unaffected. The run warns about this itself; it is repeated here
#     because the tier-A pass below is not evidence.
#
# ## Two diagnostics the driver computes and throws away
#
# `allocate_level_crops()` returns a `straddle` table and a `bridges` table,
# and `reconcile_admin_allocation()` returns a third; `.write_admin_outputs()`
# writes eleven CSVs and none of them is any of the three, nor does
# `.admin_run_record()` carry them. A depth run therefore cannot be audited
# for cell straddling or for carried years from its own output directory.
# This script recomputes both from the run's outputs so the pilot can report
# them. That is a WORKAROUND for a defect in `R/run_spatialize.R`, reported
# rather than patched here.
#
# ## The allocator's refusal, and why this leg pre-scans
#
# Decision T31(d) refuses a `(container, item, year)` group whose units ALL
# report and whose reported areas still miss the national total by more than
# both tolerances (0.10 relative AND 1,000 ha absolute).
# `.alloc_refuse_discrepancy()` aborts the entire run on the first such group,
# naming only the worst -- correct fail-closed behaviour, and a run that
# aborts yields no diagnostics whatever. So this leg evaluates the same rule
# itself, first, over the whole stated scope, prints every group it finds, and
# runs the engine without those groups' NATIONAL rows. Their admin-share rows
# stay in the constraint, so the seam gate and the gap report still see them.
#
# NOTHING IS REPAIRED AND NO TOLERANCE IS WIDENED. Which side of a refused
# group is wrong -- the FAOSTAT national total, the prefecture series, or the
# item mapping between them -- is a methodological question, and this leg
# neither answers it nor hides it: the refused groups are printed, written to
# `validation/cache/pilot_japan/prescan.csv`, counted in the METRIC line, and
# recorded per item in the baseline over the full scope, so a change in them
# fails the judge even though the engine never saw them.
#
# ## Inputs
#
#   WHEP_POLYCELL_SUPPORT_PATH  required; the pilot polycell support parquet.
#                               Absent, or carrying no `JPN-*` prefecture
#                               polycell, => the script skips, exit 0. A
#                               clean checkout reads the registered world pin
#                               here, which has no prefecture rows, so a
#                               clean checkout skips.
#   VAL_JP_INPUT_DIR            required; a directory of prepared
#                               spatialization parquets -- the same one
#                               `run_spatialize()` reads as the `input_dir`
#                               element of its `paths` argument. Wants
#                               `country_areas.parquet`,
#                               `crop_patterns.parquet`,
#                               `gridded_cropland.parquet`,
#                               `type_cropland.parquet` and, optionally,
#                               `multicropping.parquet`. It is read through
#                               `arrow::open_dataset()` and scoped to Japan's
#                               bounding box before anything is materialised,
#                               because `type_cropland.parquet` is a 268 MB
#                               world layer of which this pilot wants 305
#                               cells.
#   VAL_JP_YEARS                comma-separated years. Default 1961:2022, the
#                               span the MAFF family covers.
#   VAL_JP_ITEMS                comma-separated `item_prod_code`s. Default
#                               the six the family ships: 15 wheat, 27 rice
#                               paddy, 44 barley, 116 potatoes, 122 sweet
#                               potatoes, 236 soybeans.
#
# ## Usage
#
#   Rscript validation/japan_pilot.R
#   Rscript validation/japan_pilot.R --record    # re-record the baseline
#   Rscript validation/japan_pilot.R --refresh   # rebuild the scoped inputs
#
# Exits non-zero when the anchor moves or any recorded row moves, so CI can
# gate on it.

suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})

source("validation/validate.R")

jp_baseline <- "validation/gt_japan_pilot.json"
jp_cache <- "validation/cache/pilot_japan"

# Japan's reporting `area_code`, and the depth it is granted. Both are
# constants of this pilot rather than settings: the support holds one
# container and the administrative statistics are admin1.
.jp_container <- 110L
.jp_level <- 1L

# THE PINNED ANCHOR. Produced by an earlier investigation and reproduced here
# every run; see the header. `target_ha` is the unit target
# `build_level_crop_targets()` must emit; `conservation_rel` is the relative
# residual the container's conservation row must stay under.
.jp_anchor <- list(
  unit = "JPN-HOKKAIDO-1871-2025",
  item_prod_code = 27L,
  year = 2000L,
  reported_ha = 134900,
  admin_sum_ha = 1762002,
  national_ha = 1770000,
  target_ha = 135512.33199508287,
  conservation_rel = 8.7e-16
)

# Relative slack on a recorded number. Every number here is a deterministic
# count or ratio over a fixed run, so nothing larger is forgiven; a recorded
# number is a measured state to re-record when it changes, not a tolerance.
.jp_floor <- 1e-6

# The anchor is judged harder than the rest: it is an arithmetic identity
# over three integers, so it must reproduce to near double precision, not to
# the recording floor. 1e-9 leaves room for the allocator's own summation
# order and for nothing else.
.jp_anchor_floor <- 1e-9

# --- Scope --------------------------------------------------------------------

.jp_years <- function() {
  set <- Sys.getenv("VAL_JP_YEARS", "")
  if (!nzchar(set)) {
    return(1961:2022)
  }
  sort(unique(as.integer(strsplit(set, ",")[[1L]])))
}

.jp_items <- function() {
  set <- Sys.getenv("VAL_JP_ITEMS", "")
  if (!nzchar(set)) {
    return(c(15L, 27L, 44L, 116L, 122L, 236L))
  }
  sort(unique(as.integer(strsplit(set, ",")[[1L]])))
}

.jp_skip <- function(reason, ...) {
  message <- c(...)
  names(message)[1L] <- "i"
  cli::cli_inform(message, .envir = parent.frame())
  cat(sprintf("METRIC status=skipped reason=%s\n", reason))
  quit(save = "no", status = 0L)
}

.jp_try <- function(what, expr) {
  tryCatch(
    force(expr),
    error = function(e) {
      cli::cli_alert_warning(
        "Could not read {.val {what}}: {conditionMessage(e)}"
      )
      NULL
    }
  )
}

# --- The injected alias map ---------------------------------------------------

# The 46-row identity map, built from the pin's own identifiers so it cannot
# drift against them. `source` is the CODE SYSTEM slug, never the pin's own
# `source` value; see the header.
jp_build_aliases <- function(shares) {
  ids <- sort(unique(shares$source_native_id))
  tibble::tibble(
    source_label = ids,
    source = "whep-lab-japan",
    year_start = NA_integer_,
    year_end = NA_integer_,
    polity_code = paste0(ids, "-1871-2025"),
    common_name = sub("^JPN-", "", ids),
    confidence = "high",
    observed_rows = NA_integer_
  )
}

# Install the rows the published table does not carry yet, so the production
# call path -- which passes no `aliases` -- resolves them. The binding is
# unlocked and relocked around the write.
jp_inject_aliases <- function(aliases) {
  ns <- asNamespace("whep")
  published <- get("polity_label_aliases", envir = ns)
  if ("whep-lab-japan" %in% published$source) {
    cli::cli_alert_success(
      "{.code polity_label_aliases} already carries {.val whep-lab-japan}
       rows; nothing injected."
    )
    return(nrow(published))
  }
  unlockBinding("polity_label_aliases", ns)
  assign(
    "polity_label_aliases",
    dplyr::bind_rows(published, aliases),
    envir = ns
  )
  lockBinding("polity_label_aliases", ns)
  cli::cli_warn(c(
    "!" = "{nrow(aliases)} alias row{?s} INJECTED into
           {.code polity_label_aliases}, which publishes {nrow(published)}.",
    i = "They are a whep-polities deliverable that has not landed; this run
         stands in for it. See the script header."
  ))
  nrow(published)
}

# The trap, asserted rather than remembered: the same rows under the pin's own
# `source` value must resolve NOTHING. A change that made the resolver read
# the rows' `source` column would silently make both spellings work, and the
# next family's slug would then be wrong with no symptom.
jp_assert_slug_trap <- function(shares, aliases) {
  named <- dplyr::rename(shares, source_native_unit_id = "source_native_id")
  good <- resolve_admin_units(named, "whep-lab-japan", aliases = aliases)
  bad <- resolve_admin_units(
    named,
    "whep-lab-japan",
    aliases = dplyr::mutate(aliases, source = "admin-stats-japan")
  )
  n_good <- sum(!is.na(good$rows$level_polity_code))
  n_bad <- sum(!is.na(bad$rows$level_polity_code))
  cli::cli_alert_info(
    "Slug check: {.val whep-lab-japan} resolves {n_good}/{nrow(named)};
     {.val admin-stats-japan} resolves {n_bad}/{nrow(named)}."
  )
  if (n_good != nrow(named) || n_bad != 0L) {
    cli::cli_abort(c(
      "The alias slug rule changed.",
      x = "Expected the code-system slug to resolve every row and the pin's
           own source value to resolve none.",
      i = "Got {n_good} and {n_bad} of {nrow(named)}."
    ))
  }
  c(resolved = n_good, under_pin_source = n_bad)
}

# --- The scoped inputs --------------------------------------------------------

# Japan's cells, read off the allocation layer the run itself will build, so
# the scoping cannot select a cell the layer does not have.
jp_layer <- function() {
  grid0 <- read_level_country_grid(level = 0L, grid_vintage = "year_aware")
  grid_deep <- read_level_country_grid(
    level = .jp_level,
    containers = .jp_container
  )
  granted <- tibble::tibble(area_code = .jp_container, level = .jp_level)
  build_allocation_layer(grid0, grid_deep, granted)
}

# One prepared parquet, scoped before it is materialised. The bounding box is
# pushed into `arrow`; the exact cell set is applied afterwards, in R.
# `type_cropland.parquet` is 268 MB of world cells and a bare `collect()`
# would pull all of it into memory to keep 305.
jp_scope_input <- function(src_dir, file_name, cells, items) {
  path <- file.path(src_dir, file_name)
  if (!file.exists(path)) {
    return(NULL)
  }
  # Arrow evaluates the predicate in its own expression language and cannot
  # call `min()` on an R vector inside one, so the box is resolved to four
  # scalars here rather than referred to inside the filter.
  container <- .jp_container
  lon_lo <- min(cells$lon)
  lon_hi <- max(cells$lon)
  lat_lo <- min(cells$lat)
  lat_hi <- max(cells$lat)
  ds <- arrow::open_dataset(path)
  cols <- names(ds)
  if ("item_prod_code" %in% cols) {
    ds <- dplyr::filter(ds, item_prod_code %in% items)
  }
  if ("area_code" %in% cols) {
    ds <- dplyr::filter(ds, area_code == container)
  }
  gridded <- all(c("lon", "lat") %in% cols)
  if (gridded) {
    ds <- dplyr::filter(
      ds,
      lon >= lon_lo,
      lon <= lon_hi,
      lat >= lat_lo,
      lat <= lat_hi
    )
  }
  rows <- tibble::as_tibble(dplyr::collect(ds))
  if (gridded) {
    rows <- dplyr::semi_join(rows, cells, by = c("lon", "lat"))
  }
  rows
}

# The national totals land as `country_areas_full.parquet`, NOT under the name
# the engine reads. The pre-scan below needs the untrimmed table on every run,
# and the engine's own `country_areas.parquet` is regenerated from it each
# time; writing the trimmed table over the cached one would make a second run
# scan a table the first run had already edited, and the refusal would quietly
# disappear.
.jp_national_full <- "country_areas_full.parquet"

jp_write_inputs <- function(src_dir, cells, items, dest) {
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  wanted <- c(
    country_areas.parquet = .jp_national_full,
    crop_patterns.parquet = "crop_patterns.parquet",
    gridded_cropland.parquet = "gridded_cropland.parquet",
    type_cropland.parquet = "type_cropland.parquet",
    multicropping.parquet = "multicropping.parquet"
  )
  written <- purrr::imap(wanted, function(out_name, src_name) {
    rows <- jp_scope_input(src_dir, src_name, cells, items)
    if (is.null(rows)) {
      return(NULL)
    }
    nanoparquet::write_parquet(rows, file.path(dest, out_name))
    tibble::tibble(
      source = src_name,
      file = out_name,
      n_rows = nrow(rows),
      # The scope the cache was built at, carried in the manifest so a later
      # run with a different `VAL_JP_ITEMS` rebuilds instead of silently
      # reusing it. `crop_patterns.parquet` is item-filtered here, so a cache
      # narrower than the request is missing patterns the engine would need,
      # and one wider is not the scope that was asked for either.
      scope_items = paste(sort(items), collapse = "|")
    )
  })
  dplyr::bind_rows(written)
}

# --- The two diagnostics the run does not persist -----------------------------

jp_straddle <- function(allocation, layer) {
  whep:::.alloc_straddle(allocation, layer)
}

# The resolved constraint, rebuilt through the same three public calls
# `.admin_constraint()` makes. The run does not persist its resolved shares
# either -- only the seams it derived from them -- so the only way to say
# which unit-item-years were actually observed is to redo the resolution.
# The allocator's own `targets` table cannot answer it: decision T31(b)'s
# pattern extension gives EVERY granted unit a row in every year, observed or
# not, so a gap count taken from the targets is identically zero.
jp_constraint <- function(read, container) {
  scoped <- dplyr::filter(read$shares, .data$area_code %in% container)
  units <- resolve_admin_units(
    dplyr::rename(scoped, source_native_unit_id = "source_native_id"),
    whep:::.admin_code_systems_for(scoped$source)
  )
  kept <- units$rows |>
    dplyr::rename(source_native_id = "source_native_unit_id") |>
    dplyr::select(-"alias_source") |>
    dplyr::filter(!is.na(.data$level_polity_code))
  resolve_admin_shares(kept, not_shipped = read$not_shipped)
}

# What the MAFF panel does NOT observe, per unit-item series.
# `.alloc_bridge_report()` counts CARRIED years, and this family carries none
# -- every row is `treatment_year = "observed"` -- so its report is empty and
# says nothing about the interior gaps that are actually there. A gap here is
# a year with no row at all between two years that have one.
jp_gap_series <- function(observed) {
  key <- c("area_code", "item_prod_code", "level_polity_code")
  observed |>
    dplyr::arrange(
      .data$area_code,
      .data$item_prod_code,
      .data$level_polity_code,
      .data$year
    ) |>
    dplyr::mutate(
      gap = .data$year - dplyr::lag(.data$year) - 1L,
      .by = dplyr::all_of(key)
    ) |>
    dplyr::mutate(gap = dplyr::if_else(is.na(.data$gap), 0L, .data$gap)) |>
    dplyr::summarise(
      n_obs = dplyr::n(),
      first_year = min(.data$year),
      last_year = max(.data$year),
      n_gap_runs = sum(.data$gap > 0L),
      gap_years = sum(.data$gap),
      max_run = max(c(0L, .data$gap)),
      .by = dplyr::all_of(key)
    )
}

# --- The pre-scan, and what the allocator refuses -----------------------------

# How many units of the layer exist in each year, using the package's own
# validity predicate rather than a re-derived one. This is the `n_units` the
# allocator's `.alloc_group_state()` compares its reporting count against.
jp_valid_units <- function(layer, years) {
  purrr::map_dfr(years, function(yr) {
    valid <- whep:::.filter_country_grid_year(layer, yr)
    tibble::tibble(
      year = as.integer(yr),
      n_units_valid = dplyr::n_distinct(valid$level_polity_code)
    )
  })
}

# THE REFUSAL, MEASURED BEFORE THE ENGINE RUNS. Decision T31(d) refuses a
# `(container, item, year)` group whose units ALL report and whose reported
# areas still miss the national total by more than both tolerances:
# `.alloc_refuse_discrepancy()` aborts the whole run on the first one it
# finds, naming only the worst. That is the right fail-closed behaviour and
# it is not overridden here -- but a run that aborts produces no diagnostics
# at all, so the same rule is evaluated first, over the FULL stated scope, and
# every refused group is named. The rule is replicated from
# `.alloc_group_state()` (`basis == "admin_sum"` iff every valid unit reports
# an absolute value) and from `.alloc_refuse_discrepancy()` (both tolerances,
# taken from `.alloc_own_defaults()` rather than written out again).
jp_prescan <- function(shares, national, valid) {
  own <- whep:::.alloc_own_defaults()
  shares |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::summarise(
      n_units_valued = dplyr::n_distinct(.data$level_polity_code),
      admin_sum = sum(.data$value),
      .by = c("area_code", "item_prod_code", "year")
    ) |>
    dplyr::inner_join(
      national,
      by = c("area_code", "item_prod_code", "year")
    ) |>
    dplyr::left_join(valid, by = "year") |>
    dplyr::mutate(
      coverage = .data$n_units_valued / .data$n_units_valid,
      coverage_complete = .data$n_units_valued == .data$n_units_valid,
      basis = dplyr::if_else(
        .data$coverage_complete,
        "admin_sum",
        "residual"
      ),
      discrepancy_ha = .data$harvested_area_ha - .data$admin_sum,
      discrepancy_frac = dplyr::if_else(
        .data$harvested_area_ha > 0,
        .data$discrepancy_ha / .data$harvested_area_ha,
        NA_real_
      ),
      breaches_both = abs(.data$discrepancy_frac) > own$tolerance_relative &
        abs(.data$discrepancy_ha) > own$tolerance_absolute,
      refused = .data$coverage_complete & .data$breaches_both
    ) |>
    dplyr::arrange(.data$item_prod_code, .data$year)
}

jp_prescan_rows <- function(prescan) {
  prescan |>
    dplyr::summarise(
      n_groups = dplyr::n(),
      n_complete = sum(.data$coverage_complete),
      n_breaching = sum(.data$breaches_both),
      n_refused = sum(.data$refused),
      coverage_min = min(.data$coverage),
      median_disc_frac = stats::median(.data$discrepancy_frac),
      min_disc_frac = min(.data$discrepancy_frac),
      max_disc_frac = max(.data$discrepancy_frac),
      .by = "item_prod_code"
    ) |>
    dplyr::arrange(.data$item_prod_code) |>
    dplyr::mutate(key = paste0("item|", .data$item_prod_code))
}

jp_gap_summary <- function(series) {
  series |>
    dplyr::summarise(
      n_series = dplyr::n(),
      n_series_with_gaps = sum(.data$n_gap_runs > 0L),
      gap_years = sum(.data$gap_years),
      observed_years = sum(.data$n_obs),
      max_run = max(.data$max_run),
      .by = "item_prod_code"
    ) |>
    dplyr::arrange(.data$item_prod_code) |>
    dplyr::mutate(key = paste0("item|", .data$item_prod_code))
}

# --- Measurement --------------------------------------------------------------

jp_item_rows <- function(groups, breach, targets) {
  allocated <- dplyr::summarise(
    targets,
    target_ha = sum(.data$target_ha),
    .by = "item_prod_code"
  )
  breached <- breach |>
    dplyr::filter(.data$in_force) |>
    dplyr::summarise(
      n_breach = dplyr::n(),
      breach_ha = sum(.data$over_ha),
      .by = "item_prod_code"
    )
  groups |>
    dplyr::summarise(
      n_groups = dplyr::n(),
      coverage_min = min(.data$coverage),
      coverage_mean = mean(.data$coverage),
      max_abs_disc_frac = max(abs(.data$discrepancy_frac)),
      n_beyond = sum(.data$beyond_tolerance, na.rm = TRUE),
      .by = "item_prod_code"
    ) |>
    dplyr::left_join(allocated, by = "item_prod_code") |>
    dplyr::left_join(breached, by = "item_prod_code") |>
    dplyr::mutate(
      n_breach = dplyr::coalesce(.data$n_breach, 0L),
      breach_ha = dplyr::coalesce(.data$breach_ha, 0)
    ) |>
    dplyr::arrange(.data$item_prod_code) |>
    dplyr::mutate(key = paste0("item|", .data$item_prod_code))
}

# THE GRAIN EACH TIER'S VERDICT IS ACTUALLY TAKEN AT. Tier A judges one row
# per series and its `pass` is that row's. Tier B's `pass` is a CONTAINER gate
# keyed on `(area_code, level, basis)` -- the key `admin_seam_gate.R` warns
# must include `basis` and must NOT include the item -- and it is stamped onto
# every unit-seam row that fed it. Counting failing ROWS therefore multiplies
# one gate by every unit, item and seam year inside it: on this run ONE
# failing container gate reads as 3,087 row failures, and adding
# `item_prod_code` to the key reads it as four. Tier C is keyed on
# `(area_code, seam_year)`.
.jp_gate_grain <- function() {
  list(
    tier_a = c("area_code", "level", "item_prod_code"),
    tier_b = c("area_code", "level", "basis"),
    tier_c = c("area_code", "seam_year")
  )
}

jp_gate_rows <- function(gate) {
  grains <- .jp_gate_grain()
  purrr::map_dfr(names(gate), function(nm) {
    tbl <- gate[[nm]]
    scored <- !is.null(tbl) &&
      nrow(tbl) > 0L &&
      rlang::has_name(tbl, "pass")
    n_rows <- if (is.null(tbl)) 0L else nrow(tbl)
    if (!scored) {
      return(tibble::tibble(
        key = paste0("gate|", nm),
        n_rows = n_rows,
        n_gates = 0L,
        n_pass = 0L,
        n_fail = 0L,
        n_withheld = 0L
      ))
    }
    # `pass` comes back off a CSV, where a tier that withheld every verdict
    # reads as an all-empty column. Coerced rather than assumed logical.
    gates <- tbl |>
      dplyr::mutate(pass = as.logical(.data$pass)) |>
      dplyr::distinct(dplyr::pick(dplyr::all_of(c(grains[[nm]], "pass"))))
    tibble::tibble(
      key = paste0("gate|", nm),
      n_rows = n_rows,
      n_gates = nrow(gates),
      n_pass = sum(gates$pass, na.rm = TRUE),
      n_fail = sum(!gates$pass, na.rm = TRUE),
      n_withheld = sum(is.na(gates$pass))
    )
  })
}

# --- Baseline I/O and judging -------------------------------------------------

jp_record_group <- function(measured, drop_cols = character()) {
  rows <- dplyr::select(measured, -dplyr::any_of(drop_cols))
  stats::setNames(
    lapply(seq_len(nrow(rows)), function(i) {
      as.list(dplyr::select(rows[i, ], -"key"))
    }),
    rows$key
  )
}

jp_recorded_tbl <- function(recorded) {
  if (length(recorded) == 0) {
    return(NULL)
  }
  purrr::map_dfr(names(recorded), function(k) {
    row <- lapply(recorded[[k]], function(v) if (is.null(v)) NA else v)
    tibble::as_tibble(row) |> dplyr::mutate(key = k)
  })
}

# Judge every measured row against its record, and equally loudly the other
# way: a recorded row that stopped being measured is a series leaving the
# pilot, which is exactly what a baseline exists to notice.
jp_judge <- function(measured, recorded, num_fields) {
  reference <- jp_recorded_tbl(recorded)
  if (is.null(reference)) {
    return(dplyr::transmute(
      measured,
      key = .data$key,
      fail = TRUE,
      why = "not recorded"
    ))
  }
  joined <- dplyr::left_join(
    measured,
    reference,
    by = "key",
    suffix = c("", "_rec")
  )
  drifted <- purrr::map(num_fields, function(f) {
    was <- as.numeric(joined[[paste0(f, "_rec")]])
    now <- as.numeric(joined[[f]])
    both_na <- is.na(was) & is.na(now)
    dplyr::coalesce(
      !both_na & abs(now - was) / pmax(abs(was), .jp_floor) > .jp_floor,
      !both_na
    )
  })
  judged <- joined |>
    dplyr::mutate(
      missing_record = !.data$key %in% reference$key,
      moved = Reduce(`|`, drifted),
      fail = .data$missing_record | dplyr::coalesce(.data$moved, TRUE),
      why = dplyr::case_when(
        .data$missing_record ~ "not recorded",
        .data$fail ~ "moved against baseline",
        .default = ""
      )
    ) |>
    dplyr::select("key", "fail", "why")
  dropped <- reference |>
    dplyr::anti_join(measured, by = "key") |>
    dplyr::transmute(
      key = .data$key,
      fail = TRUE,
      why = "recorded row no longer measured"
    )
  dplyr::bind_rows(judged, dropped)
}

# --- Driver -------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
record <- "--record" %in% args
refresh <- "--refresh" %in% args

years <- .jp_years()
items <- .jp_items()

support <- .jp_try("polycell_support", read_polycell_support())
if (is.null(support)) {
  .jp_skip(
    "no_polycell_support",
    "Skipping the Japan pilot: no polycell support could be read.",
    i = "Point {.envvar WHEP_POLYCELL_SUPPORT_PATH} at the pilot support."
  )
}
prefectures <- unique(support$polity_code[grepl(
  "^JPN-[A-Z]",
  support$polity_code
)])
if (length(prefectures) == 0L) {
  .jp_skip(
    "support_has_no_prefectures",
    "Skipping the Japan pilot: the polycell support carries no Japanese
     prefecture polycell.",
    i = "The registered world pin has none. Point
         {.envvar WHEP_POLYCELL_SUPPORT_PATH} at the pilot support built by
         {.file inst/scripts/build_pilot_polycell_support.R}."
  )
}

src_dir <- Sys.getenv("VAL_JP_INPUT_DIR")
if (!nzchar(src_dir) || !dir.exists(src_dir)) {
  .jp_skip(
    "no_VAL_JP_INPUT_DIR",
    "Skipping the Japan pilot: {.envvar VAL_JP_INPUT_DIR} is unset or does
     not point at a directory.",
    i = "It wants the prepared spatialization parquets
         {.file country_areas.parquet}, {.file crop_patterns.parquet},
         {.file gridded_cropland.parquet} and
         {.file type_cropland.parquet}."
  )
}

n_support_cells <- dplyr::n_distinct(paste(support$lon, support$lat))
granted_depth <- .jp_level
cli::cli_h1("Japan pilot at depth {granted_depth}")
cli::cli_alert_info(
  "{length(years)} year{?s} ({min(years)}-{max(years)}), {length(items)}
   item{?s}, {length(prefectures)} prefecture{?s} over {n_support_cells}
   cell{?s} of support."
)

read <- .jp_try("admin-shares", read_admin_shares())
if (is.null(read)) {
  .jp_skip(
    "no_admin_shares",
    "Skipping the Japan pilot: the {.val admin-shares} pin could not be
     read."
  )
}
japan <- dplyr::filter(read$shares, .data$source == "admin-stats-japan")
if (nrow(japan) == 0L) {
  .jp_skip(
    "no_japan_admin_shares",
    "Skipping the Japan pilot: the {.val admin-shares} pin carries no
     {.val admin-stats-japan} row."
  )
}
cli::cli_alert_info(
  "Admin shares: {nrow(japan)} row{?s},
   {dplyr::n_distinct(japan$source_native_id)} unit{?s},
   {dplyr::n_distinct(japan$item_prod_code)} item{?s},
   {min(japan$year)}-{max(japan$year)}."
)

aliases <- jp_build_aliases(japan)
in_polities <- sum(aliases$polity_code %in% whep::polities$polity_code)
if (in_polities != nrow(aliases)) {
  .jp_skip(
    "aliases_not_in_polities",
    "Skipping the Japan pilot: {nrow(aliases) - in_polities} of
     {nrow(aliases)} injected polity code{?s} do not exist in
     {.code polities}."
  )
}
slug <- jp_assert_slug_trap(japan, aliases)
n_published <- jp_inject_aliases(aliases)

layer <- jp_layer()
cells <- dplyr::distinct(layer, .data$lon, .data$lat)
ragged <- attr(layer, "ragged_coverage")
unclaimed <- attr(layer, "unclaimed_land")
cli::cli_alert_info(
  "Allocation layer: {nrow(layer)} compartment{?s} over {nrow(cells)}
   cell{?s}, {dplyr::n_distinct(layer$level_polity_code)} unit{?s};
   {nrow(ragged)} ragged row{?s}, {nrow(unclaimed)} unclaimed
   cell-epoch{?s}."
)

input_dir <- file.path(jp_cache, "input")
out_dir <- file.path(jp_cache, "run")
manifest_path <- file.path(jp_cache, "input_manifest.csv")
wanted_scope <- paste(sort(items), collapse = "|")
cached <- if (file.exists(manifest_path)) {
  readr::read_csv(manifest_path, show_col_types = FALSE, progress = FALSE)
}
reusable <- !refresh &&
  !is.null(cached) &&
  rlang::has_name(cached, "scope_items") &&
  all(cached$scope_items == wanted_scope)
manifest <- if (reusable) {
  cli::cli_alert_info(
    "Using the cached scoped inputs in {.path {input_dir}}."
  )
  cached
} else {
  built <- jp_write_inputs(src_dir, cells, items, input_dir)
  dir.create(jp_cache, showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(built, manifest_path)
  built
}
cli::cli_h2("Scoped inputs")
print(manifest, n = Inf)
if (!"type_cropland.parquet" %in% manifest$file) {
  .jp_skip(
    "no_type_cropland",
    "Skipping the Japan pilot: {.file type_cropland.parquet} is not in
     {.path {src_dir}}.",
    i = "The pilot runs the {.val whep} preset, whose LUH2 type-aware
         allocation needs it; running without it would be a different
         method, not a degraded one."
  )
}

full_path <- file.path(input_dir, .jp_national_full)
national_path <- file.path(input_dir, "country_areas.parquet")
national_full <- tibble::as_tibble(nanoparquet::read_parquet(full_path))
national <- national_full |>
  dplyr::mutate(
    area_code = as.integer(.data$area_code),
    item_prod_code = as.integer(.data$item_prod_code),
    year = as.integer(.data$year)
  ) |>
  dplyr::filter(.data$year %in% years, .data$item_prod_code %in% items)

constraint <- jp_constraint(read, .jp_container)
cli::cli_alert_info(
  "Constraint: {nrow(constraint$shares)} winning row{?s} over
   {nrow(constraint$coverage)} group{?s}; {nrow(constraint$dropped)} dropped,
   {nrow(constraint$excluded)} held out, {nrow(constraint$seams)} seam{?s}.
   Indicator{?s} kept:
   {.val {sort(unique(constraint$shares$indicator_used))}}."
)

cli::cli_h2("Pre-scan: coverage and discrepancy over the full stated scope")
prescan <- jp_prescan(
  constraint$shares,
  dplyr::select(
    national,
    "area_code",
    "item_prod_code",
    "year",
    "harvested_area_ha"
  ),
  jp_valid_units(layer, years)
)
prescan_rows <- jp_prescan_rows(prescan)
print(dplyr::select(prescan_rows, -"key"), n = Inf, width = Inf)
readr::write_csv(prescan, file.path(jp_cache, "prescan.csv"))

refused <- dplyr::filter(prescan, .data$refused)
if (nrow(refused) > 0L) {
  cli::cli_h3("Groups decision T31(d) refuses")
  refused |>
    dplyr::select(
      "item_prod_code",
      "year",
      "n_units_valued",
      "n_units_valid",
      "admin_sum",
      "harvested_area_ha",
      "discrepancy_ha",
      "discrepancy_frac"
    ) |>
    print(n = Inf, width = Inf)
  cli::cli_warn(c(
    "!" = "{nrow(refused)} of {nrow(prescan)} group{?s} report a COMPLETE set
           of units whose areas still miss the national total beyond both
           T31(d) tolerances. {.fn allocate_level_crops} aborts the whole run
           on them, so the engine below is run WITHOUT their national rows.",
    i = "The rows are listed above and in
         {.path {file.path(jp_cache, 'prescan.csv')}}; nothing is repaired
         and no tolerance is widened. Which of them is a bad national total,
         a bad prefecture series or an item-mapping mismatch is a
         methodological question this leg does not answer.",
    i = "Their admin-share rows STAY in the constraint, so the seam gate and
         the gap report below still see them."
  ))
}
# Always regenerated from the untrimmed table, so the run's input is a pure
# function of the requested scope and the pre-scan, and a second run scans
# what the first one did rather than what the first one left behind.
nanoparquet::write_parquet(
  national_full |>
    dplyr::filter(as.integer(.data$item_prod_code) %in% items) |>
    dplyr::anti_join(
      dplyr::select(refused, "item_prod_code", "year"),
      by = c("item_prod_code", "year")
    ),
  national_path
)

unlink(out_dir, recursive = TRUE)
cli::cli_h2("The run")
t0 <- Sys.time()
run <- run_spatialize(
  preset = "whep",
  years = years,
  components = "landuse",
  overrides = list(
    level = .jp_level,
    output_level = .jp_level,
    granted_containers = .jp_container,
    # The CFT aggregation pools several items into one row, which is the
    # reason `admin_seam_gate.R` never reads it either. Nothing below is
    # measured on it, so it is not written.
    aggregate_to_cft = FALSE
  ),
  paths = list(input_dir = input_dir, out_dir = out_dir)
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cli::cli_alert_success("Run finished in {round(elapsed)}s.")

# `data.table::fwrite()` writes a ZERO-ROW table as its header alone, and
# `readr` then types every column as character -- so a run with no capacity
# breach, which is a perfectly ordinary run over a narrow scope, would crash
# the first `filter()` on `in_force` and the first `sum()` on `over_ha`
# instead of reporting zero. Every column this leg reads as a logical or a
# number is coerced at the boundary; on a populated table the coercion is a
# no-op.
jp_read_csv <- function(nm, logicals = character(), numerics = character()) {
  readr::read_csv(
    file.path(out_dir, nm),
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(logicals), as.logical),
      dplyr::across(dplyr::any_of(numerics), as.numeric)
    )
}
targets <- jp_read_csv("admin_targets.csv")
group_coverage <- jp_read_csv("admin_group_coverage.csv")
conservation <- jp_read_csv("admin_conservation.csv")
breach <- jp_read_csv(
  "admin_breach.csv",
  "in_force",
  c("item_prod_code", "over_ha", "rf_over_ha", "ir_over_ha", "n_cells")
)
groups <- jp_read_csv(
  "admin_reconciliation.csv",
  "beyond_tolerance",
  c(
    "item_prod_code",
    "coverage",
    "national_total",
    "admin_sum",
    "discrepancy",
    "discrepancy_frac"
  )
)
seams <- jp_read_csv("admin_seams.csv")
gate_numbers <- c("n_gated", "n_beyond", "frac_beyond", "threshold")
gate <- list(
  tier_a = jp_read_csv("admin_seam_gate_a.csv", "pass"),
  tier_b = jp_read_csv(
    "admin_seam_gate_b.csv",
    c("pass", "beyond_quantile"),
    gate_numbers
  ),
  tier_c = jp_read_csv("admin_seam_gate_c.csv", "pass")
)
allocation <- tibble::as_tibble(nanoparquet::read_parquet(
  file.path(out_dir, "gridded_landuse_crops.parquet")
))

# --- The anchor ---------------------------------------------------------------

anchor_expected <- .jp_anchor$target_ha
cli::cli_h2("The anchor: Hokkaido, paddy rice, 2000")
anchor_row <- targets |>
  dplyr::filter(
    .data$level_polity_code == .jp_anchor$unit,
    .data$item_prod_code == .jp_anchor$item_prod_code,
    .data$year == .jp_anchor$year
  )
anchor_ha <- if (nrow(anchor_row) == 1L) anchor_row$target_ha else NA_real_
anchor_rel <- abs(anchor_ha - anchor_expected) / anchor_expected
anchor_ok <- isTRUE(anchor_rel <= .jp_anchor_floor)
print(anchor_row, width = Inf)
print(
  dplyr::filter(
    group_coverage,
    .data$item_prod_code == .jp_anchor$item_prod_code,
    .data$year == .jp_anchor$year
  ),
  width = Inf
)
anchor_cons <- conservation |>
  dplyr::filter(
    .data$grain == "container",
    .data$item_prod_code == .jp_anchor$item_prod_code,
    .data$year == .jp_anchor$year
  )
print(anchor_cons, width = Inf)
cons_rel <- abs(anchor_cons$difference_frac)
# The recorded conservation figure is a BOUND -- "conserved to 8.7e-16
# relative" -- not a value to reproduce digit for digit, so it is judged as
# one. A run that conserves better than the record passes; a run that
# conserves worse is the regression this exists to catch.
cons_bound <- .jp_anchor$conservation_rel
cons_ok <- length(cons_rel) == 1L && isTRUE(cons_rel <= cons_bound)
cli::cli_alert_info(
  "Recorded {.val {anchor_expected}} ha; measured {.val {anchor_ha}} ha;
   relative difference {.val {anchor_rel}}."
)
cli::cli_alert_info(
  "Container conservation for this group: {.val {cons_rel}} relative, against
   a recorded bound of {.val {cons_bound}}."
)
if (!anchor_ok) {
  cli::cli_alert_danger(
    "THE ANCHOR MOVED. That is a finding to explain, not a tolerance to
     widen."
  )
}
if (!cons_ok) {
  cli::cli_alert_danger(
    "The anchor's container no longer conserves within the recorded bound."
  )
}

# --- The diagnostics ----------------------------------------------------------

cli::cli_h2("Coverage and discrepancy, per item")
item_rows <- jp_item_rows(groups, breach, targets)
print(dplyr::select(item_rows, -"key"), n = Inf, width = Inf)

cli::cli_h2("Groups beyond tolerance")
beyond <- dplyr::filter(groups, .data$beyond_tolerance)
if (nrow(beyond) == 0L) {
  cli::cli_alert_success("No group is beyond both tolerances.")
} else {
  beyond |>
    dplyr::select(
      "year",
      "item_prod_code",
      "coverage",
      "national_total",
      "admin_sum",
      "discrepancy",
      "discrepancy_frac"
    ) |>
    print(n = 30, width = Inf)
}

cli::cli_h2("Capacity breaches")
if (nrow(breach) == 0L) {
  cli::cli_alert_success("No unit target exceeded its cells' capacity.")
} else {
  breach |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_in_force = sum(.data$in_force),
      # `total_over_ha` and `max_over_ha`, not `over_ha` twice: inside one
      # `summarise()` a later expression sees the column an earlier one just
      # made, so naming the sum `over_ha` would make the maximum a maximum
      # over that single scalar.
      total_over_ha = sum(.data$over_ha[.data$in_force]),
      max_over_ha = max(.data$over_ha),
      .by = "mc_basis"
    ) |>
    print(n = Inf, width = Inf)
  breach |>
    dplyr::filter(.data$in_force) |>
    dplyr::slice_max(.data$over_ha, n = 10) |>
    print(n = 10, width = Inf)
}

cli::cli_h2("Straddling (recomputed; the run does not write it)")
straddle <- jp_straddle(allocation, layer)
straddle_summary <- straddle |>
  dplyr::mutate(
    sibling_ha = .data$straddle_sibling * .data$allocated_ha,
    foreign_ha = .data$straddle_foreign * .data$allocated_ha
  ) |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    allocated_ha = sum(.data$allocated_ha),
    sibling_frac = sum(.data$sibling_ha, na.rm = TRUE) /
      sum(.data$allocated_ha),
    foreign_frac = sum(.data$foreign_ha, na.rm = TRUE) /
      sum(.data$allocated_ha),
    n_cell_limited = sum(.data$cell_limited),
    .by = "item_prod_code"
  ) |>
  dplyr::arrange(.data$item_prod_code)
print(straddle_summary, n = Inf, width = Inf)

cli::cli_h2("Interior gaps in the MAFF series")
observed <- dplyr::distinct(
  constraint$shares,
  .data$area_code,
  .data$item_prod_code,
  .data$level_polity_code,
  .data$year
)
gap_series <- jp_gap_series(observed)
gap_rows <- jp_gap_summary(gap_series)
print(dplyr::select(gap_rows, -"key"), n = Inf, width = Inf)
n_gapped <- sum(gap_rows$n_series_with_gaps)
n_series <- sum(gap_rows$n_series)
n_possible <- dplyr::n_distinct(observed$level_polity_code) *
  dplyr::n_distinct(observed$item_prod_code)
cli::cli_alert_info(
  "{n_gapped} of {n_series} unit-item series carry an interior gap
   ({n_possible} unit x item combinations exist); the longest gap is
   {max(gap_rows$max_run)} year{?s}."
)
bridges <- whep:::.alloc_bridge_report(
  dplyr::mutate(observed, treatment = "observed")
)
cli::cli_alert_info(
  "The run's own bridge report holds {nrow(bridges)} row{?s}: this family
   ships {.field treatment_year} = {.val observed} on every row, so nothing
   is carried and no gap is bridged. The gaps above are UNFILLED."
)

cli::cli_h2("Seams")
seams |>
  dplyr::summarise(n = dplyr::n(), .by = c("item_prod_code", "seam_kind")) |>
  tidyr::pivot_wider(
    names_from = "seam_kind",
    values_from = "n",
    values_fill = 0L
  ) |>
  print(n = Inf, width = Inf)

cli::cli_h2("The seam gate")
gate_rows <- jp_gate_rows(gate)
print(gate_rows, n = Inf, width = Inf)
if (nrow(gate$tier_b) > 0L) {
  cli::cli_text("Tier B at its own gate grain:")
  gate$tier_b |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_items = dplyr::n_distinct(.data$item_prod_code),
      n_gated = dplyr::first(.data$n_gated),
      n_beyond = dplyr::first(.data$n_beyond),
      frac_beyond = dplyr::first(.data$frac_beyond),
      threshold = dplyr::first(.data$threshold),
      gate_status = dplyr::first(.data$gate_status),
      pass = as.logical(dplyr::first(.data$pass)),
      .by = dplyr::all_of(.jp_gate_grain()$tier_b)
    ) |>
    print(n = Inf, width = Inf)
  cli::cli_text("Tier B rows beyond the reference quantile, per item:")
  gate$tier_b |>
    dplyr::filter(.data$basis == "observed_both_sides") |>
    dplyr::summarise(
      n_pairs = dplyr::n(),
      n_beyond_quantile = sum(as.logical(.data$beyond_quantile), na.rm = TRUE),
      .by = c("item_prod_code", "seam_kinds")
    ) |>
    dplyr::arrange(.data$item_prod_code) |>
    print(n = Inf, width = Inf)
}
if (nrow(gate$tier_c) == 0L) {
  cli::cli_alert_warning(c(
    "!" = "Tier C is EMPTY, and always is on a {.fn run_spatialize} depth
           run: {.fn .admin_seam_gate} calls {.fn seam_gate} with the shares
           and the seams and no cells, so the cell-level tier has nothing to
           judge.",
    i = "Tier C on this run's grid is what
         {.file validation/admin_seam_gate.R} exists to do; point its
         {.envvar WHEP_SPATIALIZE_OUT_DIR} at this leg's output directory."
  ))
}
verdict <- run$admin$summary$seam_gate_verdict
verdict_txt <- paste(
  names(verdict),
  vapply(verdict, format, character(1)),
  sep = "=",
  collapse = ", "
)
cli::cli_alert_info("Verdict: {verdict_txt}.")

# --- The record ---------------------------------------------------------------

run_row <- tibble::tibble(
  key = "run",
  n_years = length(years),
  n_items = length(items),
  n_units = dplyr::n_distinct(targets$level_polity_code),
  n_cells = nrow(cells),
  n_layer_rows = nrow(layer),
  n_ragged = nrow(ragged),
  n_unclaimed = nrow(unclaimed),
  n_shares_pin = nrow(japan),
  n_shares_resolved = run$admin$summary$n_shares_resolved,
  n_shares_dropped = run$admin$summary$n_shares_dropped,
  n_prescan_groups = nrow(prescan),
  n_prescan_complete = sum(prescan$coverage_complete),
  n_prescan_breaching = sum(prescan$breaches_both),
  n_refused = nrow(refused),
  worst_disc_frac = prescan$discrepancy_frac[
    which.max(abs(prescan$discrepancy_frac))
  ],
  n_targets = nrow(targets),
  n_groups = nrow(groups),
  n_beyond_tolerance = sum(groups$beyond_tolerance, na.rm = TRUE),
  n_breach_rows = nrow(breach),
  breach_ha = sum(breach$over_ha[breach$in_force]),
  n_seams = nrow(seams),
  n_seam_kinds = dplyr::n_distinct(seams$seam_kind),
  n_series = n_series,
  n_series_possible = n_possible,
  n_gap_series = n_gapped,
  gap_years = sum(gap_rows$gap_years),
  max_gap_run = max(gap_rows$max_run),
  allocated_ha = sum(allocation$rainfed_ha + allocation$irrigated_ha),
  anchor_ha = anchor_ha
)

cli::cli_h2("Measured")
print(run_row, width = Inf)

if (record) {
  baseline <- list(
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    scope = list(
      container = .jp_container,
      level = .jp_level,
      years = paste0(min(years), "-", max(years)),
      items = paste(items, collapse = "|"),
      preset = "whep",
      support = basename(Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH")),
      aliases_published = n_published,
      aliases_injected = nrow(aliases),
      slug_resolved = unname(slug[["resolved"]]),
      slug_under_pin_source = unname(slug[["under_pin_source"]])
    ),
    note = paste(
      "Japan depth-1 pilot on the real MAFF admin-shares pin and the",
      "Japan-only pilot polycell support. Every number is a measured state",
      "to be re-recorded when an input changes, not a tolerance. The anchor",
      "is judged separately and harder; see `.jp_anchor` in the script. The",
      "support is Japan-only and the alias rows are injected: both are",
      "stated in the script header and neither is a property of the",
      "pipeline."
    ),
    anchor = .jp_anchor,
    run = jp_record_group(run_row),
    prescan = jp_record_group(prescan_rows, "item_prod_code"),
    items = jp_record_group(item_rows, "item_prod_code"),
    gate = jp_record_group(gate_rows),
    gaps = jp_record_group(gap_rows, "item_prod_code")
  )
  writeLines(
    jsonlite::toJSON(baseline, auto_unbox = TRUE, pretty = TRUE, digits = 17),
    jp_baseline
  )
  cli::cli_alert_success("Recorded into {.path {jp_baseline}}.")
}

baseline <- if (file.exists(jp_baseline)) {
  jsonlite::fromJSON(jp_baseline, simplifyVector = FALSE)
} else {
  list()
}

judged <- dplyr::bind_rows(
  jp_judge(run_row, baseline$run, setdiff(names(run_row), "key")) |>
    dplyr::mutate(group = "run"),
  jp_judge(
    prescan_rows,
    baseline$prescan,
    c(
      "n_groups",
      "n_complete",
      "n_breaching",
      "n_refused",
      "coverage_min",
      "median_disc_frac",
      "min_disc_frac",
      "max_disc_frac"
    )
  ) |>
    dplyr::mutate(group = "prescan"),
  jp_judge(
    item_rows,
    baseline$items,
    c(
      "n_groups",
      "coverage_min",
      "coverage_mean",
      "max_abs_disc_frac",
      "n_beyond",
      "target_ha",
      "n_breach",
      "breach_ha"
    )
  ) |>
    dplyr::mutate(group = "items"),
  jp_judge(
    gate_rows,
    baseline$gate,
    c("n_rows", "n_gates", "n_pass", "n_fail", "n_withheld")
  ) |>
    dplyr::mutate(group = "gate"),
  jp_judge(
    gap_rows,
    baseline$gaps,
    c(
      "n_series",
      "n_series_with_gaps",
      "gap_years",
      "observed_years",
      "max_run"
    )
  ) |>
    dplyr::mutate(group = "gaps")
)

cli::cli_h2("Judged against {.path {jp_baseline}}")
failed <- dplyr::filter(judged, .data$fail)
if (nrow(failed) == 0L) {
  cli::cli_alert_success("All {nrow(judged)} recorded rows match.")
} else {
  print(dplyr::select(failed, "group", "key", "why"), n = 40, width = Inf)
}

n_gates <- sum(gate_rows$n_gates, na.rm = TRUE)
n_gate_fail <- sum(gate_rows$n_fail, na.rm = TRUE)
n_fail <- nrow(failed)
cat(sprintf(
  paste0(
    "METRIC status=run n_years=%d n_items=%d n_units=%d n_targets=%d ",
    "n_prescan_groups=%d n_prescan_breaching=%d n_refused=%d ",
    "worst_disc_frac=%.4f n_groups=%d n_beyond_tolerance=%d ",
    "n_breach_rows=%d breach_ha=%.6g ",
    "n_seams=%d n_seam_kinds=%d n_series=%d n_gap_series=%d max_gap_run=%d ",
    "n_gates=%d n_gate_failures=%d anchor_ha=%.5f anchor_rel=%.3e ",
    "anchor_ok=%d anchor_cons_rel=%.3e anchor_cons_ok=%d n_failed=%d\n"
  ),
  run_row$n_years,
  run_row$n_items,
  run_row$n_units,
  run_row$n_targets,
  run_row$n_prescan_groups,
  run_row$n_prescan_breaching,
  run_row$n_refused,
  run_row$worst_disc_frac,
  run_row$n_groups,
  run_row$n_beyond_tolerance,
  run_row$n_breach_rows,
  run_row$breach_ha,
  run_row$n_seams,
  run_row$n_seam_kinds,
  run_row$n_series,
  run_row$n_gap_series,
  run_row$max_gap_run,
  n_gates,
  n_gate_fail,
  anchor_ha,
  anchor_rel,
  as.integer(anchor_ok),
  if (length(cons_rel) == 1L) cons_rel else NA_real_,
  as.integer(cons_ok),
  n_fail
))

if (!anchor_ok) {
  cli::cli_abort(
    "The Hokkaido anchor moved: recorded {.val {anchor_expected}} ha,
     measured {.val {anchor_ha}} ha."
  )
}
if (!cons_ok) {
  cli::cli_abort(
    "The anchor's container conserves to {.val {cons_rel}} relative, worse
     than the recorded bound {.val {cons_bound}}."
  )
}
if (n_fail > 0) {
  cli::cli_abort(
    "{n_fail} recorded row{?s} moved against {.path {jp_baseline}}; re-record
     with {.code --record} once the change is understood."
  )
}
