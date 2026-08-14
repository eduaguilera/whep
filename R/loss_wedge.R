# The supply-to-intake loss wedge for the SJOS-N nourishment floor.
#
# The floor asks whether supply CAN meet needs. Food that never reaches a mouth
# has to be allowed for, but only the part that no food system avoids: avoidable
# waste is part of the over-nourishment problem, and inflating the floor by it
# would convert a behaviour problem into an apparent adequacy failure.
#
# omega = 0 is equally wrong. It asserts that 100% of edible loss is eliminable,
# which no food system achieves and which SDG target 12.3 does not even aim at
# (it targets HALVING waste). WRAP's UK household taxonomy shows why the binary
# fails: of 110 kg/person/year, 67 is avoidable, 19 is "possibly avoidable"
# (bread crusts, potato skins) and 25 is unavoidable -- the grey zone is
# comparable in size to the unavoidable class.
#
# Hence the half-of-minimum construction, and hence its framing as a LOWER
# BOUND rather than an estimate of achievable loss. Two things about it must not
# be forgotten, and are repeated in the roxygen because that is where they are
# read:
#
# 1. The two steps do not have the same interpretation. The CONSUMPTION-step
#    minimum is sub-Saharan Africa in every single commodity group (cereals 1%,
#    roots 2%, fruit and vegetables 5%, meat 2%, milk 0.1%), and those are
#    scarcity figures, not efficiency figures. The DISTRIBUTION-step minima are
#    genuine best practice (Europe, North America, and Industrialized Asia for
#    milk and meat).
# 2. Only steps at or after retail are composed. FBS food availability is
#    measured at the retail level and "includes any loss or waste at the retail
#    or consumer level" (FAO, New Food Balances, section II p.4), so the three
#    pre-retail steps of Annex 4 are outside the wedge by construction. FBS
#    element 5123 `Losses` is likewise pre-retail and already netted out of the
#    Food element upstream, so subtracting it would double-count; it is not used
#    here at all.

#' Build the supply-to-intake loss wedge.
#'
#' @description
#' Returns the share of retail-level protein supply that does not become intake,
#' per country and year, and the divisor `1 / (1 - omega)` that turns a
#' requirement into a supply floor. It replaces part of the unsourced 1.35
#' multiplier the SJOS-N axis used to carry (whep#753).
#'
#' Rates come from Gustavsson et al. (2011), *Global food losses and food
#' waste*, FAO, Annex 4, which tabulates loss percentages for seven commodity
#' groups in seven world regions at five food-chain steps. Only the two steps at
#' or after the retail shelf are composed, because FBS food availability is
#' already measured there:
#' `omega_group = 1 - (1 - d/2) * (1 - c/2)`, with `d` and `c` the distribution
#' and consumption rates. Country-year `omega` is the protein-weighted mean of
#' the group values over the supplied food basket, so it varies with a
#' country's diet composition while the underlying rates do not.
#'
#' `method = "gustavsson_half_min"` (default) takes the minimum of each rate
#' across the seven regions and halves it. It is a **deliberate lower bound on
#' unavoidable loss, not an estimate of achievable loss**, for two reasons that
#' the halving does not repair. The consumption-step minimum is sub-Saharan
#' Africa in every commodity group, and those are scarcity figures rather than
#' efficiency figures; the distribution-step minima, by contrast, are genuine
#' best practice. On WHEP's 2010 basket it gives `omega` near 2.5%, which is
#' below FAOSTAT's retail-only median of 2.83% on energy (item 21059) even
#' though this wedge additionally spans the household step -- a different basis
#' and a different statistic, so not a contradiction, but confirmation that the
#' construction is conservative.
#'
#' `"gustavsson_min"` drops the halving and roughly doubles the wedge (near 4.9%
#' on the same basket); `"none"` sets it to zero and makes every floor an
#' explicit lower bound. All are alternatives, never fallbacks: the choice is
#' stamped in `method_loss_wedge`.
#'
#' `"gustavsson_regional_actual"` is the sensitivity arm, and it is **not an
#' unavoidable-loss estimate at all**: it drops both the minimum and the halving
#' and gives each country its Annex 1 region's own observed rates. On the 2010
#' world basket that is 14.2% against the default's 2.5%, a floor divisor of
#' 1.166, spanning 4.1% to 21.4% across countries. Its country structure is
#' contested -- Gustavsson's rich-high gradient is the opposite sign to UNEP's
#' Food Waste Index -- so it quantifies that disagreement rather than resolving
#' it.
#'
#' Annex 1 lists 152 countries, covering 99.0% of 2010 world food protein.
#' Areas outside it take the unweighted mean rate across the seven regions by
#' default (`coverage = "global_mean"`), stamped as such in `method_region`;
#' `"annex1_only"` returns nothing for them instead. Annex 1's "China" does not
#' disambiguate, so both WHEP codes take Industrialized Asia: `CHN` (area 41,
#' "China, mainland") and the aggregate area 351, which carries no `iso3c` and
#' is what the FBS pin reports food on. Keying on `iso3c` alone would drop area
#' 351 and with it a fifth of world food protein.
#'
#' Items that Gustavsson's Annex 2 does not place in a commodity group carry no
#' rate rather than borrowing a neighbour's. They are dropped from the weighting,
#' which gives them the basket's mean wedge implicitly, and their share is
#' reported in `protein_grouped_share` so the choice stays visible. On the 2010
#' world basket they are 5.0% of food protein, eggs alone being 3.7%; assigning
#' eggs to meat or to dairy instead moves `omega` by less than 0.1 percentage
#' points either way.
#'
#' @param data Named list of injected inputs. Supply the basket either as
#'   `protein_supply` (`year`, `area_code`, `item_cbs_code`, `protein_t`) or as
#'   `cbs_food` (`year`, `area_code`, `item_cbs_code`, `food_t`), which is
#'   converted through the same nutrition lookup [build_food_supply()] uses.
#'   `biomass_coefs`, `items_full`, `food_loss_wedge`,
#'   `food_loss_item_groups` and `food_loss_regions` override the packaged
#'   tables.
#' @param method `"gustavsson_half_min"` (default), `"gustavsson_min"`,
#'   `"gustavsson_regional_actual"` or `"none"`.
#' @param coverage How areas outside Annex 1's 152 countries are handled by
#'   `"gustavsson_regional_actual"`: `"global_mean"` (default) or
#'   `"annex1_only"`. Ignored by the region-invariant methods.
#' @param protein_basis Passed to the nutrition lookup when the basket is given
#'   as `cbs_food`; see [build_food_supply()]. Defaults to `"edible_portion"`,
#'   matching the supply the floor is compared against.
#' @return A tibble keyed by `year`, `area_code` with `omega`, `floor_divisor`,
#'   `protein_grouped_share`, `method_loss_wedge` and `method_region`, plus the
#'   polity columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_loss_wedge(
#'   data = list(
#'     protein_supply = tibble::tribble(
#'       ~year, ~area_code, ~item_cbs_code, ~protein_t,
#'       2010L, 10L,        2511L,          100,
#'       2010L, 10L,        2605L,          100
#'     )
#'   )
#' )
build_loss_wedge <- function(
  data = list(),
  method = c(
    "gustavsson_half_min",
    "gustavsson_min",
    "gustavsson_regional_actual",
    "none"
  ),
  coverage = c("global_mean", "annex1_only"),
  protein_basis = c("edible_portion", "whole_commodity", "product_nitrogen")
) {
  method <- rlang::arg_match(method)
  coverage <- rlang::arg_match(coverage)
  protein_basis <- rlang::arg_match(protein_basis)
  groups <- data$food_loss_item_groups %||%
    whep::whep_coef_table("food_loss_item_groups")
  wedge <- data$food_loss_wedge %||% whep::whep_coef_table("food_loss_wedge")
  regions <- data$food_loss_regions %||%
    whep::whep_coef_table("food_loss_regions")

  supply <- .lw_protein_supply(data, protein_basis)
  supply |>
    .lw_weight(
      groups,
      .lw_region_wedge(wedge, method),
      .lw_assign_regions(supply, regions, method, coverage)
    ) |>
    dplyr::mutate(
      floor_divisor = 1 / (1 - .data$omega),
      method_loss_wedge = method
    ) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# The basket, either injected as protein directly or derived from food tonnes
# through the SAME nutrition lookup build_food_supply() uses, so the weights and
# the supply the floor is compared against cannot drift apart.
.lw_protein_supply <- function(data, protein_basis) {
  cols <- c("year", "area_code", "item_cbs_code")
  if (!is.null(data$protein_supply)) {
    .check_columns(
      data$protein_supply,
      c(cols, "protein_t"),
      "data$protein_supply"
    )
    return(dplyr::select(
      data$protein_supply,
      dplyr::all_of(c(cols, "protein_t"))
    ))
  }
  if (is.null(data$cbs_food)) {
    cli::cli_abort(
      "Supply the basket as {.field data$protein_supply} or
       {.field data$cbs_food}."
    )
  }
  .check_columns(data$cbs_food, c(cols, "food_t"), "data$cbs_food")
  data$cbs_food |>
    .food_join_nutrition(
      .food_nutrition_lookup(
        data$items_full %||% whep::items_full,
        data$biomass_coefs %||% whep::biomass_coefs,
        protein_basis
      )
    ) |>
    dplyr::summarise(
      protein_t = sum(.data$food_t * .data$protein_frac_kgfm, na.rm = TRUE),
      .by = dplyr::all_of(cols)
    )
}

# One wedge per region and commodity group. The three region-invariant methods
# collapse to a single pseudo-region so the weighting joins the same way for
# all four; only "gustavsson_regional_actual" keeps Annex 4's regional detail,
# plus a synthesized "global_mean" region for countries Annex 1 omits.
.lw_region_wedge <- function(wedge, method) {
  .check_columns(
    wedge,
    c("region", "loss_group", "step", "loss_pct"),
    "data$food_loss_wedge"
  )
  if (method != "gustavsson_regional_actual") {
    return(dplyr::mutate(
      .lw_group_wedge(method, wedge),
      region = "region_invariant"
    ))
  }
  wedge |>
    dplyr::summarise(
      loss_pct = mean(.data$loss_pct),
      .by = c("loss_group", "step")
    ) |>
    dplyr::mutate(region = "global_mean") |>
    dplyr::bind_rows(
      dplyr::select(wedge, "region", "loss_group", "step", "loss_pct")
    ) |>
    dplyr::summarise(
      omega_group = 1 - prod(1 - .data$loss_pct / 100),
      .by = c("region", "loss_group")
    )
}

# The region each area's rates come from, and where that region came from.
# Never a silent fallback: an area outside Annex 1 is stamped "global_mean",
# and under `coverage = "annex1_only"` it gets no region and no wedge at all.
.lw_assign_regions <- function(supply, regions, method, coverage) {
  areas <- dplyr::distinct(supply, .data$area_code)
  if (method != "gustavsson_regional_actual") {
    return(dplyr::mutate(
      areas,
      region = "region_invariant",
      method_region = "region_invariant"
    ))
  }
  out <- areas |>
    dplyr::left_join(.lw_area_regions(regions), by = "area_code") |>
    dplyr::mutate(
      method_region = dplyr::if_else(
        is.na(.data$region),
        NA_character_,
        "annex1"
      )
    )
  if (coverage == "global_mean") {
    out <- dplyr::mutate(
      out,
      method_region = dplyr::coalesce(.data$method_region, "global_mean"),
      region = dplyr::coalesce(.data$region, "global_mean")
    )
  }
  .lw_warn_unregioned(out)
  out
}

# Annex 1 is transcribed on `iso3c`, which is the readable key, but a few areas
# WHEP reports food on carry no `iso3c` at all -- the China aggregate, area
# 351, is the one that matters, at a fifth of world food protein. Those rows
# name their area code directly.
.lw_area_regions <- function(regions) {
  .check_columns(
    regions,
    c("iso3c", "area_code", "region"),
    "data$food_loss_regions"
  )
  by_iso <- regions |>
    dplyr::filter(!is.na(.data$iso3c)) |>
    dplyr::select("iso3c", "region") |>
    dplyr::inner_join(
      whep::regions_full |>
        dplyr::filter(!is.na(.data$iso3c), !is.na(.data$code)) |>
        dplyr::distinct(
          iso3c = .data$iso3c,
          area_code = as.integer(.data$code)
        ),
      by = "iso3c"
    ) |>
    dplyr::select("area_code", "region")
  regions |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      region = .data$region
    ) |>
    dplyr::bind_rows(by_iso) |>
    dplyr::distinct()
}

# One wedge per commodity group. The across-region minimum of each step, halved
# for the default method, then composed multiplicatively: the consumption step
# acts on what survives distribution, so adding the two rates would overstate
# the wedge.
.lw_group_wedge <- function(method, wedge) {
  if (method == "none") {
    return(dplyr::mutate(
      dplyr::distinct(wedge, .data$loss_group),
      omega_group = 0
    ))
  }
  halving <- if (method == "gustavsson_half_min") 2 else 1
  wedge |>
    dplyr::summarise(
      rate = min(.data$loss_pct) / 100 / halving,
      .by = c("loss_group", "step")
    ) |>
    dplyr::summarise(
      omega_group = 1 - prod(1 - .data$rate),
      .by = "loss_group"
    )
}

# Protein-weight the group wedges over each country-year's own basket. Items
# with no Annex 2 group are excluded from both sides of the ratio rather than
# counted at zero loss, and their share is reported.
.lw_weight <- function(supply, groups, region_wedge, areas) {
  .check_columns(
    groups,
    c("item_cbs_code", "loss_group"),
    "data$food_loss_item_groups"
  )
  keyed <- supply |>
    dplyr::left_join(
      dplyr::select(groups, "item_cbs_code", "loss_group"),
      by = "item_cbs_code"
    ) |>
    # Both region columns come off this one join, and `method_region` then
    # rides through the grouping below. Reading it back with a second join on
    # `area_code` would add a year-free territorial key for nothing.
    dplyr::left_join(
      dplyr::select(areas, "area_code", "region", "method_region"),
      by = "area_code"
    ) |>
    dplyr::left_join(region_wedge, by = c("region", "loss_group")) |>
    dplyr::mutate(grouped = !is.na(.data$omega_group))
  # Distinct output names: reusing `protein_t` here would redefine it before
  # the later expressions read it, and each would then see the country-year
  # scalar instead of the item column.
  out <- keyed |>
    dplyr::summarise(
      total_t = sum(.data$protein_t, na.rm = TRUE),
      grouped_t = sum(.data$protein_t[.data$grouped], na.rm = TRUE),
      wedge_t = sum(
        (.data$protein_t * .data$omega_group)[.data$grouped],
        na.rm = TRUE
      ),
      .by = c("year", "area_code", "method_region")
    ) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      omega = dplyr::if_else(
        .data$grouped_t > 0,
        .data$wedge_t / .data$grouped_t,
        NA_real_
      ),
      protein_grouped_share = dplyr::if_else(
        .data$total_t > 0,
        .data$grouped_t / .data$total_t,
        NA_real_
      ),
      method_region = .data$method_region
    )
  .lw_warn_ungrouped(out)
  out
}

# A country-year whose entire basket falls outside Annex 2 has no basis for a
# wedge at all. That is a mapping failure, not a zero, so it is named. An area
# that simply has no region is a different problem, reported separately, so it
# is excluded here rather than counted twice.
.lw_warn_ungrouped <- function(out) {
  bad <- dplyr::filter(out, is.na(.data$omega), !is.na(.data$method_region))
  if (nrow(bad) == 0L) {
    return(invisible())
  }
  areas <- unique(bad$area_code)
  cli::cli_warn(c(
    "!" = "No Annex 2 commodity group covers any protein in {nrow(bad)}
           country-year{?s}, whose wedge is therefore missing.",
    "i" = "Area code{?s}: {areas}."
  ))
}

# Under `coverage = "annex1_only"` an area Annex 1 does not list gets no wedge.
# That is the point of the setting, but it must still be visible.
.lw_warn_unregioned <- function(areas) {
  bad <- dplyr::filter(areas, is.na(.data$region))
  if (nrow(bad) == 0L) {
    return(invisible())
  }
  codes <- bad$area_code
  cli::cli_warn(c(
    "!" = "Gustavsson's Annex 1 lists no region for {nrow(bad)} area{?s}, whose
           wedge is therefore missing.",
    "i" = "Area code{?s}: {codes}.",
    "i" = "Use {.code coverage = \"global_mean\"} to give them the mean rate
           across the seven regions instead."
  ))
}
