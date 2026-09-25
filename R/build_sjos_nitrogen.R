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
# Real calls never fabricate domestic closure: they require an IO model or
# explicitly supplied pre-traced flows.

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
#'   medium critical loads for the pathway boundary), `cbs_food` and
#'   `n_inputs`, and optionally `population` (read with [read_population()]
#'   at its own default composition over the years of `cbs_food` and
#'   `n_inputs` when absent; inject a table to use any other source),
#'   `biomass_coefs` / `items_full` for the food supply,
#'   `manure_mgmt_nh3_n_t` for the pathway boundary when
#'   `nh3_source = "total_agricultural"`, and either an `io` model or
#'   `fp_flows` for the footprint. A real call without either source aborts
#'   rather than fabricating a domestic-only footprint. `grassland` is the
#'   [build_n_boundary_exceedance()] grassland-split input, only used and
#'   only optional when `boundary_land_use = "all"` and
#'   `grassland_split = "image_density"` (see `grassland_split`); a real call
#'   without it and without `critical` to build it from aborts rather than
#'   guessing a var/threshold to match.
#'   Defaults to `list()`.
#' @param surplus_method Surplus definition passed to [calculate_n_surplus()],
#'   `"harvest_removal"` (default) or `"full_balance"`.
#' @param boundary_land_use Land-use scope stamp passed to
#'   [build_n_boundary_exceedance()], `"ara"` (default, the robust historical
#'   comparison) or `"all"` (cropland and intensive grassland compared like
#'   for like against the critical allowance, extensive grassland against
#'   IMAGE's 2010 budget; see `grassland_split`) (issue #1285).
#' @param grassland_split Grassland treatment passed to
#'   [build_n_boundary_exceedance()], used only when
#'   `boundary_land_use = "all"`: `"image_density"` (default) splits each
#'   cell into a managed and an extensive component, `"none"` compares one
#'   cell pressure with the deposited `"all"`-scope allowance. Under
#'   `"image_density"`, `data$grassland` is used when supplied (its four
#'   elements, see [build_n_boundary_exceedance()]); otherwise it is built
#'   from [build_grassland_intensity_classes()], the IMAGE 2010 extensive
#'   budget, and the `"ara"`/`"igl"` critical layers matched to
#'   `data$critical`'s own var and threshold. Ignored (no extra reads) when
#'   `boundary_land_use` is not `"all"`.
#' @param nh3_source Air-pressure scope passed to [build_n_pathway_exceedance()],
#'   `"soil"` (default) or `"total_agricultural"`.
#' @param footprint_category Which per-crop nitrogen mass the footprint traces,
#'   `"exceedance"` (default), `"within_boundary"` or `"production"`.
#' @param nourishment_thresholds Which band the "just" axis classifies against:
#'   `"composed"` (default) builds it per country and year from
#'   [build_nourishment_band()]'s four sourced terms, or `"flat"` restores the
#'   retired 62.1 / 85.05 pair. `"flat"` survives for continuity and
#'   sensitivity only: of its five underlying numbers only the 46 g/cap/day
#'   floor was ever sourced, and the 1.35 multiplier behind both bounds was a
#'   preliminary presentation figure (whep#753).
#' @param nourishment_band Named list of options for the composed band, ignored
#'   when `nourishment_thresholds = "flat"`. `quality_method` and
#'   `quality_variant` select the protein-quality tier and its bracket
#'   ([build_protein_quality()]); `wedge_method` and `wedge_coverage` select the
#'   loss wedge ([build_loss_wedge()]); `shortfall`, `ceiling` and
#'   `requirement_sd` go to [build_nourishment_band()] itself, and `ceiling` is
#'   the sensitivity knob the band's own documentation asks callers to sweep.
#'   An option this list does not name **aborts** rather than being ignored, so
#'   a mistyped knob cannot silently run the default and be reported as a
#'   sensitivity. Defaults to `list()`, which leaves every builder on its own
#'   default.
#' @param example If `TRUE`, drive the whole chain from the coherent fixture set
#'   instead of `data`. Defaults to `FALSE`.
#' @return A named list of SJOS-N output tables: `surplus` (per-crop gridded
#'   surplus), `boundary_surplus` (a list with the `grid` and `country`
#'   surplus-mode exceedance), `boundary_pathway` (the pathway-mode exceedance
#'   with `binding_boundary`), `nourishment` (per-capita food supply with the
#'   normalized adequacy score and class), `scatter` (the per-capita boundary
#'   versus nourishment points; it and `nourishment` carry
#'   `method_population`, `"read_population"` or `"supplied"`), `sjos_class`
#'   (the 2-way classification) and `footprint` (a list with the `fp_all` and
#'   `fp_food` embodied-nitrogen footprints).
#' @export
#' @examples
#' build_sjos_nitrogen(example = TRUE)
build_sjos_nitrogen <- function(
  data = list(),
  surplus_method = "harvest_removal",
  boundary_land_use = "ara",
  grassland_split = c("image_density", "none"),
  nh3_source = "soil",
  footprint_category = "exceedance",
  nourishment_thresholds = c("composed", "flat"),
  nourishment_band = list(),
  example = FALSE
) {
  grassland_split <- rlang::arg_match(grassland_split)
  nourishment_thresholds <- rlang::arg_match(nourishment_thresholds)
  data <- if (isTRUE(example)) .sjos_n_example_data() else data
  # `[[` not `$`: `data$population` partially matches `data$population_age`
  # when the caller left `population` out, and would divide by the age table.
  method_population <- if (is.null(data[["population"]])) {
    "read_population"
  } else {
    "supplied"
  }
  data$population <- data[["population"]] %||% .sjos_read_population(data)
  opts <- list(
    surplus_method = surplus_method,
    boundary_land_use = boundary_land_use,
    grassland_split = grassland_split,
    nh3_source = nh3_source,
    footprint_category = footprint_category,
    nourishment_thresholds = nourishment_thresholds,
    nourishment_band = .sjos_band_options(nourishment_band),
    example = isTRUE(example)
  )
  surplus <- calculate_n_surplus(data$balance, method = opts$surplus_method)
  boundary <- .sjos_boundary_surplus(surplus, data, opts)
  nourishment <- .sjos_nourishment(data, opts)
  sjos_class <- classify_sjos_n(boundary$country, nourishment)
  list(
    surplus = surplus,
    boundary_surplus = boundary,
    boundary_pathway = .sjos_boundary_pathway(data, opts),
    nourishment = dplyr::mutate(
      nourishment,
      method_population = .env$method_population
    ),
    scatter = .sjos_scatter(data, nourishment) |>
      dplyr::mutate(method_population = .env$method_population),
    sjos_class = sjos_class,
    footprint = .sjos_footprint(
      boundary$country,
      data,
      opts,
      sjos_class
    )
  )
}

# ---- Private helpers -------------------------------------------------------

# The one denominator both per-capita axes divide by (#484): the nourishment
# supply and the anthropogenic-N scatter, read once over every year either
# covers so the two cannot be divided by different tables. It takes
# read_population()'s own default composition rather than choosing one here,
# so the choice of source lives in one place (#1133) and a caller who wants
# another injects `data$population`.
.sjos_read_population <- function(data) {
  years <- sort(unique(c(data$cbs_food$year, data$n_inputs$year)))
  read_population(years = years) |>
    dplyr::select("year", "area_code", "population")
}

# The surplus-mode boundary at both grid (the per-crop map table Module 4 keys
# on) and country (the aggregate the classification and footprint consume). Both
# come from the same surplus and critical layer, so the two resolutions cannot
# diverge. The grassland-split inputs (when applicable) are resolved once here
# so the grid and country calls share the same reads rather than each
# rebuilding them.
.sjos_boundary_surplus <- function(surplus, data, opts) {
  grassland <- .sjos_resolve_grassland(surplus, data, opts)
  list(
    grid = .sjos_exceedance(
      surplus,
      data[["critical"]],
      opts$boundary_land_use,
      "grid",
      opts$grassland_split,
      grassland
    ),
    country = .sjos_exceedance(
      surplus,
      data[["critical"]],
      opts$boundary_land_use,
      "country",
      opts$grassland_split,
      grassland
    )
  )
}

# One surplus-mode exceedance call, parameterised by resolution. grassland_split
# and grassland are forwarded as-is: build_n_boundary_exceedance() itself
# ignores both outside `land_use = "all"`, so passing them through for
# `"ara"`/`"igl"` triggers no extra read and changes nothing.
.sjos_exceedance <- function(
  surplus,
  critical,
  land_use,
  resolution,
  grassland_split = "image_density",
  grassland = NULL
) {
  years <- unique(surplus$year[!is.na(surplus$year)])
  if (length(years) != 1L) {
    cli::cli_abort(
      "The SJOS-N driver requires one explicit actual-pressure year per run."
    )
  }
  build_n_boundary_exceedance(
    surplus = surplus,
    critical = critical,
    land_use = land_use,
    resolution = resolution,
    metric = "surplus",
    actual_year = as.integer(years),
    critical_reference_year = 2010L,
    grassland_split = grassland_split,
    grassland = grassland %||% list(classes = NULL, extensive_budget = NULL)
  )
}

# Resolve the grassland-split inputs once for a boundary_surplus call: NULL
# (no read at all) unless the split is actually requested
# (`boundary_land_use = "all"` and `grassland_split = "image_density"`, the
# only combination build_n_boundary_exceedance() itself does anything with).
# An injected data$grassland is used as-is -- its four elements are checked by
# build_n_boundary_exceedance() itself, not re-validated here; otherwise the
# real inputs are assembled from data$critical.
.sjos_resolve_grassland <- function(surplus, data, opts) {
  if (
    opts$boundary_land_use != "all" || opts$grassland_split != "image_density"
  ) {
    return(NULL)
  }
  data[["grassland"]] %||% .sjos_grassland_inputs(surplus, data)
}

# Assemble the real "image_density" grassland-split inputs when the caller did
# not inject data$grassland: the reclassification-through-time table
# (build_grassland_intensity_classes()) for the surplus's own year(s), the
# IMAGE 2010 extensive budget (.critical_n_extensive_budget() over
# .critical_n_grassland_layers()), and the "ara"/"igl" critical layers read at
# the same var and threshold as data$critical, so they are the same published
# surface build_n_boundary_exceedance() checks them against
# (.nbx_check_layer_consistency()). Never guesses a var/threshold of its own:
# without data$critical there is nothing to match ara/igl to, so this aborts
# rather than reading a default that could silently disagree with the "all"
# layer already in use.
.sjos_grassland_inputs <- function(surplus, data) {
  critical <- data[["critical"]]
  if (is.null(critical)) {
    cli::cli_abort(
      c(
        "{.arg boundary_land_use = \"all\"} with
         {.arg grassland_split = \"image_density\"} needs
         {.field data$critical} to match the {.val ara}/{.val igl} layers
         against.",
        i = "Supply {.field data$grassland} directly (its four elements), or
             {.field data$critical} so the split can build them from the real
             inputs."
      ),
      class = "whep_sjos_grassland_missing"
    )
  }
  .check_columns(critical, c("critical_var", "critical_threshold"), "critical")
  var <- unique(critical$critical_var)
  threshold <- unique(critical$critical_threshold)
  years <- sort(unique(surplus$year[!is.na(surplus$year)]))
  root <- .critn_root_path(.resolve_critical_n_dir(NULL))
  list(
    classes = build_grassland_intensity_classes(years),
    extensive_budget = .critical_n_extensive_budget(
      .critical_n_grassland_layers(root)
    ),
    critical_ara = read_critical_n(
      var = var,
      threshold = threshold,
      land_use = "ara"
    ),
    critical_igl = read_critical_n(
      var = var,
      threshold = threshold,
      land_use = "igl"
    )
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
#
# `nourishment_thresholds = "composed"` is the default and builds the band from
# its four sourced terms per country and year. `"flat"` restores the retired
# 46 x 1.35 / 63 x 1.35 pair, which survives only for continuity and
# sensitivity: of its five numbers only the 46 was ever sourced, and the 1.35
# behind both bounds was a preliminary presentation figure (whep#753).
#
# Building the band needs three inputs beyond the supply itself, and each falls
# back to its own reader when not injected, so a caller that has them can stay
# offline and a caller that does not still gets a band.
.sjos_nourishment <- function(data, opts) {
  supply <- build_food_supply(method = "whep_native", data = data)
  if (opts$nourishment_thresholds == "flat") {
    .sjos_warn_band_ignored(opts$nourishment_band)
    return(normalize_nourishment(supply))
  }
  normalize_nourishment(
    supply,
    thresholds = .sjos_band(data, supply, opts$nourishment_band)
  )
}

# The four terms, assembled on the same country-years the supply covers. The
# per-item protein the wedge and the quality term both need is the column
# build_food_supply() forms and then collapses away, so it is re-formed here
# through the SAME nutrition lookup rather than a parallel one.
.sjos_band <- function(data, supply, band) {
  years <- sort(unique(supply$year))
  population_age <- data$population_age %||%
    read_wpp_population(by = "age_sex", years = years)
  protein_supply <- data$protein_supply %||% .sjos_protein_by_item(data)
  rlang::exec(
    build_nourishment_band,
    data = list(
      requirement = build_protein_requirement(
        data = list(population_age = population_age)
      ),
      requirement_safe = build_protein_requirement(
        data = list(population_age = population_age),
        requirement = "safe"
      ),
      dispersion = data$dispersion %||%
        build_intake_dispersion(
          data = list(habitual_cv = data$habitual_cv) |> purrr::compact(),
          years = years
        ),
      loss_wedge = .sjos_wedge(protein_supply, band),
      quality = .sjos_quality(protein_supply, band),
      supply = supply
    ),
    !!!.sjos_opt(band, c("shortfall", "ceiling", "requirement_sd"))
  )
}

# The band's selectable options, validated by NAME only. Nothing is defaulted
# here: an option the caller did not give is simply not passed on, so each
# builder's own default applies and the two cannot drift apart.
#
# An unrecognised name ABORTS. A sensitivity analysis is exactly the situation
# where a silently ignored knob is worst -- the run completes, the numbers move
# not at all, and the sweep is reported as showing insensitivity.
.sjos_band_options <- function(band) {
  known <- c(
    "quality_method",
    "quality_variant",
    "wedge_method",
    "wedge_coverage",
    "shortfall",
    "ceiling",
    "requirement_sd"
  )
  unknown <- setdiff(names(band) %||% rep("", length(band)), known)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{.arg nourishment_band} has unknown option{?s} {.val {unknown}}.",
      i = "Known option{?s}: {.val {known}}."
    ))
  }
  band
}

.sjos_opt <- function(band, keys) {
  band[intersect(names(band), keys)]
}

# The flat pair has no terms to configure, so band options do nothing there.
# Say so rather than dropping them: a sweep run against `"flat"` by accident
# would otherwise complete, move nothing, and read as insensitivity -- the same
# failure .sjos_band_options() aborts on, arriving by a different route. A
# warning and not an abort, because comparing flat against composed from one
# shared options list is a legitimate thing to do.
.sjos_warn_band_ignored <- function(band) {
  if (length(band) == 0L) {
    return(invisible())
  }
  given <- names(band)
  cli::cli_warn(c(
    "!" = "{.arg nourishment_thresholds} is {.val flat}, so the
           {length(band)} {.arg nourishment_band} option{?s} {.val {given}}
           {?does/do} nothing.",
    "i" = "The flat pair is two constants; only the composed band has terms to
           configure."
  ))
}

# Protein quality: tier 1a per-item digestibility by default, tier 1b class
# rates, or none. Selectable from the driver so the whole chain -- band,
# classification and headcounts -- moves with the tier rather than only the
# quality table.
.sjos_quality <- function(protein_supply, band) {
  args <- .sjos_opt(band, c("quality_method", "quality_variant"))
  names(args) <- c(
    quality_method = "method",
    quality_variant = "variant"
  )[names(args)]
  rlang::exec(
    build_protein_quality,
    data = list(protein_supply = protein_supply),
    !!!args
  )
}

.sjos_wedge <- function(protein_supply, band) {
  args <- .sjos_opt(band, c("wedge_method", "wedge_coverage"))
  names(args) <- c(
    wedge_method = "method",
    wedge_coverage = "coverage"
  )[names(args)]
  rlang::exec(
    build_loss_wedge,
    data = list(protein_supply = protein_supply),
    !!!args
  )
}

# Per-item protein tonnes, through build_food_supply()'s own nutrition lookup so
# the wedge and the quality term are weighted on exactly the supply the band is
# compared against.
.sjos_protein_by_item <- function(data) {
  data$cbs_food |>
    .food_join_nutrition(
      .food_nutrition_lookup(
        data$items_full %||% whep::items_full,
        data$biomass_coefs %||% whep::biomass_coefs,
        "edible_portion"
      )
    ) |>
    dplyr::summarise(
      protein_t = sum(.data$food_t * .data$protein_frac_kgfm, na.rm = TRUE),
      .by = c("year", "area_code", "item_cbs_code")
    )
}

# The per-capita boundary-versus-nourishment scatter: country anthropogenic
# reactive nitrogen per capita, normalized against the world per-capita
# planetary boundary and joined to the same nourishment normalization.
.sjos_scatter <- function(data, nourishment) {
  data$n_inputs |>
    build_n_percapita(population = data$population) |>
    build_n_boundary_percapita(
      nourishment = nourishment,
      population = data$population
    )
}

# The embodied-nitrogen trade footprint from the country exceedance.
.sjos_footprint <- function(country_exc, data, opts, origin_classes) {
  build_sjos_n_footprint(
    exceedance = country_exc,
    io = data$io,
    category = opts$footprint_category,
    data = .sjos_fp_data(country_exc, data, opts, origin_classes)
  )
}

# Resolve the footprint's tracing input: an injected IO model or explicitly
# injected pre-traced flows. Domestic closure is a fixture concern only and is
# supplied by .sjos_n_example_data(); silently creating it here would turn a
# real no-IO analysis into a false 100% domestic footprint.
.sjos_fp_data <- function(country_exc, data, opts, origin_classes) {
  if (!is.null(data$io)) {
    return(list(origin_classes = origin_classes))
  }
  if (rlang::has_name(data, "fp_flows")) {
    return(list(
      fp_flows = data$fp_flows,
      origin_classes = origin_classes
    ))
  }
  if (isTRUE(opts$example)) {
    return(list(
      fp_flows = .sjos_fp_flows_fixture(
        country_exc,
        opts$footprint_category
      ),
      origin_classes = origin_classes
    ))
  }
  cli::cli_abort(c(
    "A real SJOS-N footprint requires an IO model or pre-traced flows.",
    i = "Supply {.field data$io} or {.field data$fp_flows}; domestic closure is
         available only through {.code example = TRUE}."
  ))
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
      origin_item = .data$item_cbs_code,
      target_area = .data$area_code,
      target_fd = "food",
      target_item = .data$item_cbs_code,
      value = .data$impact_u
    )
}
