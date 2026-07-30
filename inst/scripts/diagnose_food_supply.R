# -----------------------------------------------------------------------
# diagnose_food_supply.R
#
# Benchmarks WHEP's commodity-balance `food` element, and the per-capita
# protein derived from it, against FAOSTAT's own Food Balance Sheets. Written
# to settle issue #360, which reported that the `food` element over-allocates
# supply to trade-heavy countries (Netherlands 7.0 kg/cap/day against a
# claimed FAOSTAT ~2.0).
#
# What it measures, for one year:
#
#   1. Four candidate over-allocation mechanisms inside build_cbs():
#        M1  `food := domestic_supply` in .cbs_final_balance() assigns rather
#            than increments, replacing an estimated destiny split with the
#            whole domestic supply (the dp_mask block).
#        M2  .add_global_destiny_shares() applies a *global* food share where
#            a country has no share of its own.
#        M3  The trade -> CBS crosswalks dedup pairs, not keys, so a
#            one-to-many mapping duplicates value instead of splitting (#164).
#        M4  Destinies exceeding domestic supply, i.e. a broken supply-use
#            identity (the #143 signature).
#
#   2. WHEP food tonnes and derived protein against FAOSTAT FBS, read from
#      the pin. The per-capita FBS elements are dropped by .extract_fao()'s
#      cb_elements filter but are present in the pin itself.
#
# Result on the 2010 build (see the PR that added this script): every
# mechanism measures as noise, WHEP's food quantity tracks FAOSTAT at a
# median ratio of 1.015, and the per-capita protein is inflated by a median
# 1.21 because whep::biomass_coefs carries no edible-portion nitrogen
# (Edible_N_kgFM is entirely empty, issue #361), so the coalesce chain falls
# through to whole-product N x 6.25.
#
# Usage:
#   source("inst/scripts/diagnose_food_supply.R")
#   res <- diagnose_food_supply(year = 2010, out_dir = "diag_out")
# -----------------------------------------------------------------------

# Countries used for readable per-country reporting, with the WHEP figure
# issue #360 reported for each. The issue's own FAOSTAT column is not carried:
# it disagrees with FAOSTAT (it gives the Netherlands ~2.0 against an actual
# 2.90) and was part of what made the issue look like a defect.
.dfs_reference <- function() {
  tibble::tribble(
    ~area_iso3c, ~label,             ~issue_claim,
    "NLD",       "Netherlands",      7.0,
    "POL",       "Poland",           5.6,
    "CHN",       "China (mainland)", 4.8,
    "USA",       "USA",              2.9,
    "BRA",       "Brazil",           2.3,
    "NGA",       "Nigeria",          1.8,
    "IND",       "India",            1.4
  )
}

#' Benchmark the CBS food element and derived protein against FAOSTAT FBS.
#'
#' @param year Calendar year to diagnose.
#' @param out_dir Directory for the CSV outputs.
#' @return A named list with the mechanism attribution, the static crosswalk
#'   check, the paired WHEP/FAOSTAT comparison and the reference-country table.
diagnose_food_supply <- function(year = 2010L, out_dir = ".") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  cli::cli_h1("M3 - static crosswalk fan-out check")
  fanout <- .dfs_crosswalk_fanout()
  print(as.data.frame(fanout))

  cli::cli_h1("Building CBS for {year}")
  built <- .dfs_build(year)
  rows <- .dfs_row_diagnostics(.dfs_wide(built$cbs))

  cli::cli_h1("Mechanism attribution")
  attrib <- .dfs_attribute(rows, built$global_share_rows)
  print(as.data.frame(attrib))

  cli::cli_h1("WHEP against FAOSTAT FBS")
  cmp <- .dfs_compare(.dfs_whep_national(rows), .dfs_fbs(year))
  .dfs_report(cmp)

  refs <- .dfs_reference_table(cmp)
  print(as.data.frame(refs))

  .dfs_write(out_dir, attrib, fanout, cmp, refs)
  list(attrib = attrib, fanout = fanout, cmp = cmp, refs = refs)
}

# ---- Build -------------------------------------------------------------

# Build one year of CBS, capturing which destiny rows fell back to the global
# share (M2). The wrapper delegates to the real function and only records; the
# original binding is restored on exit.
.dfs_build <- function(year) {
  captured <- new.env(parent = emptyenv())
  original <- whep:::.add_global_destiny_shares
  instrumented <- function(df) {
    out <- original(df)
    captured$rows <- .dfs_capture_global(out)
    out
  }
  utils::assignInNamespace(".add_global_destiny_shares", instrumented, "whep")
  on.exit(
    utils::assignInNamespace(".add_global_destiny_shares", original, "whep"),
    add = TRUE
  )

  primary <- build_primary_production(start_year = year, end_year = year)
  cbs <- build_commodity_balances(primary, start_year = year, end_year = year)
  list(cbs = tibble::as_tibble(cbs), global_share_rows = captured$rows)
}

# Food-destiny rows with no country share of their own, which therefore take
# the global fallback. The tonnes are what that fallback contributes.
.dfs_capture_global <- function(out) {
  dt <- data.table::as.data.table(out)
  if (!all(c("dest_share", "dest_share_global") %in% names(dt))) {
    return(NULL)
  }
  dt[
    element == "food" & is.na(dest_share) & !is.na(dest_share_global),
    .(global_share_t = domestic_supply * dest_share_global)
  ] |>
    tibble::as_tibble()
}

# ---- CBS internal diagnostics ------------------------------------------

# Long CBS to one row per country-item, all elements as columns.
.dfs_wide <- function(cbs) {
  cbs |>
    dplyr::select("year", "area_code", "item_cbs_code", "element", "value") |>
    tidyr::pivot_wider(
      names_from = "element",
      values_from = "value",
      values_fill = 0
    ) |>
    .dfs_ensure_elements()
}

# Guarantee every element column exists, so the arithmetic never silently
# skips a term that happens to be absent this year.
.dfs_ensure_elements <- function(wide) {
  needed <- c(
    "production",
    "import",
    "export",
    "stock_variation",
    "domestic_supply",
    "food",
    "feed",
    "seed",
    "processing",
    "other_uses"
  )
  missing <- setdiff(needed, names(wide))
  if (length(missing) > 0L) {
    wide[missing] <- 0
  }
  wide
}

# Per-row identity checks. `over_alloc` is the tonnage by which the destinies
# exceed domestic supply; `dp_signature` marks rows carrying the fingerprint
# of the M1 overwrite, food set to the whole domestic supply while the other
# destinies are still non-zero.
.dfs_row_diagnostics <- function(wide) {
  wide |>
    dplyr::mutate(
      other_destinies = .data$feed +
        .data$seed +
        .data$processing +
        .data$other_uses,
      over_alloc = pmax(
        .data$food + .data$other_destinies - .data$domestic_supply,
        0
      ),
      supply_gap = .data$production +
        .data$import -
        .data$export -
        .data$stock_variation -
        .data$domestic_supply,
      dp_signature = abs(.data$food - .data$domestic_supply) < 1e-6 &
        .data$other_destinies > 0
    )
}

# Global tonnage moved by each mechanism, so their sizes are comparable.
.dfs_attribute <- function(rows, global_share_rows) {
  total <- sum(rows$food, na.rm = TRUE)
  m1 <- sum(rows$food[rows$dp_signature], na.rm = TRUE)
  m2 <- if (is.null(global_share_rows)) {
    NA_real_
  } else {
    sum(global_share_rows$global_share_t, na.rm = TRUE)
  }
  over <- sum(rows$over_alloc, na.rm = TRUE)
  gap <- sum(abs(rows$supply_gap), na.rm = TRUE)
  tibble::tibble(
    mechanism = c(
      "total food element",
      "M1 default-destiny overwrite",
      "M2 global destiny-share fallback",
      "M4 destiny over-allocation",
      "supply-side gap"
    ),
    tonnes = c(total, m1, m2, over, gap),
    share_of_food = c(1, m1, m2, over, gap) / c(1, rep(total, 4))
  )
}

# Both trade bridges dedup *pairs*. A key appearing with more than one target
# fans out on merge, duplicating value rather than splitting it (#164).
.dfs_crosswalk_fanout <- function() {
  trade <- whep::cbs_trade_codes |>
    dplyr::distinct(.data$item_code_trade, .data$item_cbs) |>
    dplyr::count(.data$item_code_trade) |>
    dplyr::filter(.data$n > 1)
  items <- whep::items_full |>
    dplyr::distinct(.data$item_cbs, .data$item_cbs_code) |>
    dplyr::count(.data$item_cbs) |>
    dplyr::filter(.data$n > 1)
  tibble::tibble(
    bridge = c("item_code_trade -> item_cbs", "item_cbs -> item_cbs_code"),
    keys_fanning_out = c(nrow(trade), nrow(items)),
    max_targets = c(.dfs_max_n(trade), .dfs_max_n(items))
  )
}

.dfs_max_n <- function(x) {
  if (nrow(x) == 0L) 0L else max(x$n)
}

# ---- FAOSTAT FBS benchmark ---------------------------------------------

# FAO FBS item codes at or above 2900 are aggregates (Grand Total, Vegetal and
# Animal Products, Cereals, ...) and 2501 is Population; everything else is a
# leaf commodity. Summing leaves therefore reproduces the reported total
# without double counting.
.dfs_is_leaf <- function(code) {
  code < 2900 & code != 2501
}

# National FAOSTAT reference: population, reported Grand Total protein, and
# food supply summed over leaf commodities.
.dfs_fbs <- function(year) {
  f <- whep_read_file("faostat-fbs-new")
  data.table::setDT(f)
  data.table::setnames(
    f,
    c("Area Code", "Item Code", "Element", "Year", "Value"),
    c("area_code", "item_code", "element", "year", "value")
  )
  # `year` is both the argument and a column; bind it to a distinct name so
  # data.table's j/i scoping cannot resolve the column instead.
  target_year <- year
  .dfs_fbs_assemble(f[year == target_year])
}

.dfs_fbs_assemble <- function(f10) {
  pop <- f10[
    item_code == 2501L & element == "Total Population - Both sexes",
    .(area_code, fao_pop = value * 1000)
  ]
  prot <- f10[
    item_code == 2901L &
      element == "Protein supply quantity (g/capita/day)",
    .(area_code, fao_protein_g_day = value)
  ]
  food <- f10[
    .dfs_is_leaf(item_code) &
      element == "Food supply quantity (kg/capita/yr)",
    .(fao_food_kg_day = sum(value, na.rm = TRUE) / 365),
    by = area_code
  ]
  Reduce(
    function(a, b) merge(a, b, by = "area_code"),
    list(pop, prot, food)
  ) |>
    tibble::as_tibble()
}

# Protein per kg fresh matter, following build_food_supply()'s coalesce chain:
# edible-portion N, then whole-product N, then dry-matter N; times 6.25.
.dfs_protein_lookup <- function() {
  nutrition <- whep::biomass_coefs |>
    dplyr::transmute(
      Name_biomass = .data$Name_biomass,
      protein_frac_kgfm = dplyr::coalesce(
        .data$Edible_N_kgFM,
        .data$N_kgN_kgFM,
        .data$Product_kgN_kgDM * .data$Product_kgDM_kgFM
      ) *
        6.25
    ) |>
    dplyr::distinct(.data$Name_biomass, .keep_all = TRUE)
  whep::items_full |>
    dplyr::distinct(.data$item_cbs_code, .data$Name_biomass) |>
    dplyr::left_join(nutrition, by = "Name_biomass")
}

# National WHEP food tonnes and protein tonnes.
.dfs_whep_national <- function(rows) {
  rows |>
    dplyr::select("area_code", "item_cbs_code", food_t = "food") |>
    dplyr::left_join(.dfs_protein_lookup(), by = "item_cbs_code") |>
    dplyr::summarise(
      whep_food_t = sum(.data$food_t, na.rm = TRUE),
      whep_protein_t = sum(
        .data$food_t * .data$protein_frac_kgfm,
        na.rm = TRUE
      ),
      .by = "area_code"
    )
}

# Pair the two sources on FAO's own population, so the comparison cannot be
# confounded by a different population series.
.dfs_compare <- function(whep_nat, fao) {
  whep_nat |>
    dplyr::inner_join(fao, by = "area_code") |>
    dplyr::filter(.data$fao_pop > 1e6, .data$fao_protein_g_day > 0) |>
    dplyr::mutate(
      whep_food_kg_day = .data$whep_food_t * 1000 / .data$fao_pop / 365,
      whep_protein_g_day = .data$whep_protein_t * 1e6 / .data$fao_pop / 365,
      food_ratio = .data$whep_food_kg_day / .data$fao_food_kg_day,
      protein_ratio = .data$whep_protein_g_day / .data$fao_protein_g_day
    )
}

# ---- Reporting ---------------------------------------------------------

# The nourishment ceiling that separates Adequate from Over on the SJOS-N
# axis, in g protein/cap/day. Kept literal: whep::nourishment_thresholds is
# not on main.
.dfs_over_ceiling <- function() 85.05

.dfs_report <- function(cmp) {
  ceiling_g <- .dfs_over_ceiling()
  cli::cli_alert_info("Paired countries: {nrow(cmp)}.")
  cli::cli_alert_info(
    "Food kg/cap/day, median FAO {round(stats::median(cmp$fao_food_kg_day), 2)}
     vs WHEP {round(stats::median(cmp$whep_food_kg_day), 2)}
     (ratio {round(stats::median(cmp$food_ratio), 3)})."
  )
  cli::cli_alert_info(
    "Protein g/cap/day, median FAO
     {round(stats::median(cmp$fao_protein_g_day), 1)} vs WHEP
     {round(stats::median(cmp$whep_protein_g_day), 1)}
     (ratio {round(stats::median(cmp$protein_ratio), 3)})."
  )
  cli::cli_alert_info(
    "Share at or above the {ceiling_g} g Over ceiling:
     FAO {round(100 * mean(cmp$fao_protein_g_day >= ceiling_g), 1)}%,
     WHEP {round(100 * mean(cmp$whep_protein_g_day >= ceiling_g), 1)}%."
  )
}

.dfs_reference_table <- function(cmp) {
  bridge <- dplyr::distinct(
    whep::polity_area_crosswalk,
    .data$area_code,
    .data$area_iso3c
  )
  .dfs_reference() |>
    dplyr::inner_join(
      dplyr::inner_join(cmp, bridge, by = "area_code"),
      by = "area_iso3c"
    ) |>
    dplyr::transmute(
      label = .data$label,
      issue_claim = .data$issue_claim,
      whep_food = round(.data$whep_food_kg_day, 2),
      fao_food = round(.data$fao_food_kg_day, 2),
      food_ratio = round(.data$food_ratio, 2),
      whep_protein = round(.data$whep_protein_g_day, 1),
      fao_protein = round(.data$fao_protein_g_day, 1),
      protein_ratio = round(.data$protein_ratio, 2)
    )
}

.dfs_write <- function(out_dir, attrib, fanout, cmp, refs) {
  data.table::fwrite(attrib, file.path(out_dir, "attribution.csv"))
  data.table::fwrite(fanout, file.path(out_dir, "crosswalk_fanout.csv"))
  data.table::fwrite(cmp, file.path(out_dir, "whep_vs_fbs.csv"))
  data.table::fwrite(refs, file.path(out_dir, "reference_countries.csv"))
  cli::cli_alert_success("Diagnostic CSVs written to {.path {out_dir}}.")
}
