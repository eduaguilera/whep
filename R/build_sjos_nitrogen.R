# End-to-end SJOS-N assembly (Module 5, Task 5.1). Composes the Safe and Just
# Operating Space for nitrogen modules into one named list of analysis-output
# tables from a single coherent set of inputs. The point of this driver is
# consistency: the same nitrogen balance feeds both the surplus surface and the
# process-based pathway boundary; the same nourishment normalization feeds both
# the 2-way classification and the per-capita boundary scatter; and the one
# country-aggregated surplus-mode exceedance feeds the classification, the
# footprint extension and the trade footprint. No module function is modified
# here: this file only wires them together, so a genuine composition mismatch
# (a column two modules disagree on, an empty join) surfaces as a failure rather
# than being papered over.
#
# The heavy real inputs (the gridded balance, the Schulte-Uebbing critical-N
# archive, the commodity balances, the multi-regional IO model) are absent in
# the tested environment, so every module is driven through its injected
# fixture: `data` supplies each module's inputs, and `example = TRUE` swaps in a
# single coherent fixture set (R/toy_examples.R) whose grid keys, country codes
# and years line up so no join is empty. The footprint is traced through the
# documented `data$fp_flows` seam of build_sjos_n_footprint() (a domestic-closure
# test double for the IO model) so the extension total is conserved end to end.

#' Assemble the end-to-end SJOS-N output tables.
#'
#' @description
#' Composes the Safe and Just Operating Space for nitrogen (SJOS-N) modules into
#' a named list of analysis-output tables from one coherent set of inputs. The
#' gridded soil-surface nitrogen surplus ([calculate_n_surplus()]) is compared to
#' the Schulte-Uebbing critical nitrogen layer for the surplus-mode boundary
#' ([build_n_boundary_exceedance()], at grid and country resolution) and the same
#' balance's process-based losses are routed to their medium-specific critical
#' loads for the pathway boundary ([build_n_pathway_exceedance()]). The
#' nourishment axis ([build_food_supply()] then [normalize_nourishment()]) is
#' crossed with the country-aggregated exceedance into the 2-way classification
#' ([classify_sjos_n()]) and, via the per-capita anthropogenic reactive nitrogen
#' ([build_n_percapita()]), into the boundary-versus-nourishment scatter
#' ([build_n_boundary_percapita()]). The country exceedance finally becomes an
#' embodied-nitrogen trade footprint ([build_sjos_n_footprint()]).
#'
#' The same nitrogen balance feeds the surplus and the pathway boundaries, the
#' same nourishment feeds the classification and the scatter, and the one
#' country exceedance feeds the classification, the footprint extension and the
#' footprint: consistency is enforced by construction. When `example = TRUE`, a
#' single coherent fixture set drives the whole chain without any real data.
#'
#' @param data Named list of injected module inputs. When `example = FALSE` it
#'   must carry a `balance` ([build_nitrogen_balance()] output), a `critical`
#'   ([read_critical_n()] critical surplus), a `critical_loads` list (the three
#'   medium critical loads for the pathway boundary), `cbs_food`, `population`,
#'   `n_inputs`, and optionally `biomass_coefs` / `items_full` for the food
#'   supply, `manure_mgmt_nh3_n_t` for the pathway boundary when
#'   `nh3_source = "total_agricultural"`, and either an `io` model or `fp_flows`
#'   for the footprint. When neither `io` nor `fp_flows` is present the footprint
#'   is traced through a domestic-closure fixture built from the extension.
#'   Defaults to `list()`.
#' @param surplus_method Surplus definition passed to [calculate_n_surplus()],
#'   `"harvest_removal"` (default) or `"full_balance"`.
#' @param boundary_land_use Land-use scope stamp passed to
#'   [build_n_boundary_exceedance()], `"all"` (default) or `"ara"`.
#' @param nh3_source Air-pressure scope passed to [build_n_pathway_exceedance()],
#'   `"soil"` (default) or `"total_agricultural"`.
#' @param footprint_category Which per-crop nitrogen mass the footprint traces,
#'   `"exceedance"` (default), `"within_boundary"` or `"production"`.
#' @param example If `TRUE`, drive the whole chain from the coherent fixture set
#'   instead of `data`. Defaults to `FALSE`.
#' @return A named list of SJOS-N output tables: `surplus` (per-crop gridded
#'   surplus), `boundary_surplus` (a list with the `grid` and `country`
#'   surplus-mode exceedance), `boundary_pathway` (the pathway-mode exceedance
#'   with `binding_boundary`), `nourishment` (per-capita food supply with the
#'   normalized adequacy score and class), `scatter` (the per-capita boundary
#'   versus nourishment points), `sjos_class` (the 2-way classification) and
#'   `footprint` (a list with the `fp_all` and `fp_food` embodied-nitrogen
#'   footprints).
#' @export
#' @examples
#' build_sjos_nitrogen(example = TRUE)
build_sjos_nitrogen <- function(
  data = list(),
  surplus_method = "harvest_removal",
  boundary_land_use = "all",
  nh3_source = "soil",
  footprint_category = "exceedance",
  example = FALSE
) {
  data <- if (isTRUE(example)) .sjos_n_example_data() else data
  opts <- list(
    surplus_method = surplus_method,
    boundary_land_use = boundary_land_use,
    nh3_source = nh3_source,
    footprint_category = footprint_category
  )
  surplus <- calculate_n_surplus(data$balance, method = opts$surplus_method)
  boundary <- .sjos_boundary_surplus(surplus, data, opts)
  nourishment <- .sjos_nourishment(data)
  list(
    surplus = surplus,
    boundary_surplus = boundary,
    boundary_pathway = .sjos_boundary_pathway(data, opts),
    nourishment = nourishment,
    scatter = .sjos_scatter(data, nourishment),
    sjos_class = classify_sjos_n(boundary$country, nourishment),
    footprint = .sjos_footprint(boundary$country, data, opts)
  )
}

# ---- Private helpers -------------------------------------------------------

# The surplus-mode boundary at both grid (the per-crop map table Module 4 keys
# on) and country (the aggregate the classification and footprint consume). Both
# come from the same surplus and critical layer, so the two resolutions cannot
# diverge.
.sjos_boundary_surplus <- function(surplus, data, opts) {
  list(
    grid = .sjos_exceedance(
      surplus,
      data$critical,
      opts$boundary_land_use,
      "grid"
    ),
    country = .sjos_exceedance(
      surplus,
      data$critical,
      opts$boundary_land_use,
      "country"
    )
  )
}

# One surplus-mode exceedance call, parameterised by resolution.
.sjos_exceedance <- function(surplus, critical, land_use, resolution) {
  build_n_boundary_exceedance(
    surplus = surplus,
    critical = critical,
    land_use = land_use,
    resolution = resolution,
    metric = "surplus"
  )
}

# The pathway-mode boundary from the same balance, routing each process-based
# loss to its medium-specific critical load. The manure-management ammonia (only
# read for nh3_source = "total_agricultural") is forwarded as a focused list.
.sjos_boundary_pathway <- function(data, opts) {
  build_n_pathway_exceedance(
    balance = data$balance,
    critical_loads = data$critical_loads,
    nh3_source = opts$nh3_source,
    resolution = "grid",
    data = list(manure_mgmt_nh3_n_t = data$manure_mgmt_nh3_n_t)
  )
}

# The nourishment "just" axis: per-capita food supply normalized to the adequacy
# score and Under/Adequate/Over class. This one table feeds both the 2-way
# classification and the per-capita boundary scatter.
.sjos_nourishment <- function(data) {
  build_food_supply(method = "whep_native", data = data) |>
    normalize_nourishment()
}

# The per-capita boundary-versus-nourishment scatter: country anthropogenic
# reactive nitrogen per capita, normalized against the world per-capita
# planetary boundary and joined to the same nourishment normalization.
.sjos_scatter <- function(data, nourishment) {
  data$n_inputs |>
    build_n_percapita(population = data$population) |>
    build_n_boundary_percapita(nourishment = nourishment)
}

# The embodied-nitrogen trade footprint from the country exceedance. When no IO
# model or pre-traced flows are supplied, a domestic-closure fixture stands in
# for the IO model so the extension total is conserved end to end.
.sjos_footprint <- function(country_exc, data, opts) {
  build_sjos_n_footprint(
    exceedance = country_exc,
    io = data$io,
    category = opts$footprint_category,
    data = .sjos_fp_data(country_exc, data, opts)
  )
}

# Resolve the footprint's tracing input: an injected IO model (the real path),
# injected pre-traced flows, or the domestic-closure fixture built from the
# extension.
.sjos_fp_data <- function(country_exc, data, opts) {
  if (!is.null(data$io)) {
    return(list())
  }
  if (rlang::has_name(data, "fp_flows")) {
    return(list(fp_flows = data$fp_flows))
  }
  list(
    fp_flows = .sjos_fp_flows_fixture(country_exc, opts$footprint_category)
  )
}

# A domestic-closure test double for the IO model: build the footprint extension
# from the country exceedance and assign every crop's nitrogen to domestic food
# consumption in its own country, so the traced total equals the extension total
# (conservation) and the per-crop item_cbs_code survives into the footprint.
.sjos_fp_flows_fixture <- function(country_exc, category) {
  build_n_exceedance_extension(country_exc, category = category) |>
    dplyr::transmute(
      year = .data$year,
      origin_area = .data$area_code,
      target_area = .data$area_code,
      target_fd = "food",
      target_item = .data$item_cbs_code,
      value = .data$impact_u
    )
}
