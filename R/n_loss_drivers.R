# The per-row drivers of the nitrogen loss cascade, built by WHEP itself from
# its own crop land use, CRU climate and LPJmL irrigation depth.
#
# Every IPCC 2019 Tier 1 factor the cascade prices with is keyed on the wet/dry
# class of IPCC 2019 Vol 4 Ch 11 Table 11.1, footnote 4 (p. 11.13), which
# build_cell_moisture_regime() assigns per cell, polity, commodity balance
# item, water regime and year. This file puts that class on every balance row:
# - A row with a class of its own takes it.
# - A row without one (no crop item, as the SOM sequestration rows, or an item
#   WHEP holds no regime land of in that water regime, as grass) takes its
#   cell's class on precipitation alone: no crop means no irrigation, and a
#   rainfed water term is precipitation alone by definition.
# - A cell off CRU's land mask, or one CRU maps for some variables only, has
#   already taken the variables it lacks from its nearest CRU land cell,
#   inside the climate assembly of build_cell_moisture_regime().
# Every row ends with a class; one that cannot is refused, not defaulted.
#
# The Atlantic/Mediterranean options read `climate` and `irrig_type`, mapped
# from the same class: wet -> ATL, dry -> MED (Meisinger's own Humid/Semiarid
# key follows the same split), so one classification drives every method.

#' Build the IPCC 2019 wet/dry drivers of the nitrogen loss cascade.
#'
#' @description
#' Put the IPCC 2019 Tier 1 wet/dry class of [build_cell_moisture_regime()],
#' and the Atlantic/Mediterranean drivers mapped from it, on every row of a
#' gridded nitrogen balance, as the two driver tables
#' [build_nitrogen_balance()] joins: `n_balance_drivers` (per balance key and
#' loss `fert_type`, read by [calculate_nh3()], [calculate_soil_n2o()] and
#' [calculate_indirect_n2o_nh3()]) and `n_balance_leaching_drivers` (per
#' balance key, read by [calculate_n_leaching()]). [build_nitrogen_balance()]
#' calls it itself at grid resolution when neither table is supplied.
#'
#' A row whose cell, polity, commodity balance item and water regime has a
#' class of its own takes it. A row without one takes its cell's class on
#' precipitation alone (no crop, no irrigation): a row with no crop item, as
#' the soil organic matter sequestration rows, and a row of an item WHEP
#' holds no land of in that water regime, as grass. A cell lacking CRU
#' climate variables takes them from its nearest CRU land cell (see
#' [build_cell_moisture_regime()]). Every row ends with a class.
#'
#' `climate` is `"ATL"` on wet rows and `"MED"` on dry rows. `irrig_type`,
#' the [n2o_efs_disaggregated] key of the Atlantic/Mediterranean options, is
#' `"Flooded"` on every rice row (the commodity balance item of the
#' production items classed `Crop_irrig == "Rice"` in [items_prod_full]) in
#' both climates and both water regimes, `"Med_average"` on other irrigated
#' MED rows, `"Rainfed"` on other rainfed MED rows and `"Tier_1"` on every
#' other ATL row (the ATL table defines only `"Tier_1"` and `"Flooded"`).
#'
#' @details
#' Which driver columns each loss method reads, and which this function
#' supplies:
#'
#' | Method | Driver columns | Supplied |
#' |---|---|---|
#' | `nh3`: `"ipcc"` | `fert_type` | yes |
#' | `nh3`: `"manner"`, `"manner_default"` | MANNER's, see below | no |
#' | `n2o`: `"ipcc2019"` | `moisture_regime` | yes |
#' | `n2o`: `"ipcc2019_atl_med"` | `climate` | yes |
#' | `n2o`: `"aguilera"`, `"ipcc2006"` | `climate`, `irrig_type` | yes |
#' | `n2o_indirect`: `"ipcc2019"` | `moisture_regime` | yes |
#' | `n2o_indirect`: `"ipcc2019_atl_med"` | `climate`, `irrig_type` | yes |
#' | `leaching`: `"ipcc_fracleach"` | `moisture_regime`, `water_regime` | yes |
#' | `leaching`: `"meisinger_drainage"` | Meisinger's, see below | yes |
#' | `leaching`: `"image_gnm"` | IMAGE-GNM's, see below | yes |
#'
#' MANNER's drivers are later work: it reads `manner_fertiliser`, `soil_ph`,
#' `rate_kg_ha`, `rainfall_mm`, `irrigated`, `temp_c`, `temp_c_annual_mean`,
#' `windspeed_ms`, `technique`, `system`, `incorporation_delay_h` and
#' `species` (see [calculate_nh3()]), and [build_nitrogen_balance()] refuses
#' it before assembling any input unless its driver table is supplied.
#'
#' The leaching table carries the drivers of `methods$leaching` only.
#'
#' `method_moisture_regime` is [build_cell_moisture_regime()]'s stamp. A row
#' that took its cell's precipitation-only class carries the extra field
#' `cell_rainfed_class=no_crop_item` (no crop item) or
#' `cell_rainfed_class=no_regime_area` (an item without regime land of the
#' row's water regime), and a row in a cell that took climate variables from
#' its nearest CRU cell the field `nearest_cru_cell=<col>[+<col>...]`, naming
#' them.
#'
#' @section Leaching drivers:
#' Under `methods$leaching = "meisinger_drainage"` or `"image_gnm"` every
#' driver is a source layer joined on the cell, polity, commodity balance item
#' and year, never on a name, and stamped:
#' - `land_use`: `"Grassland"` on the grass item (3000), `"Cropland"`
#'   otherwise.
#' - `drainage_mm` (Meisinger) and `runoff_mm` (IMAGE-GNM): the year's LPJmL
#'   deep seepage and total runoff ([read_lpjml_hydrology()] `"drainage"` and
#'   `"runoff"`, from `WHEP_LPJML_RUN_DIR`), whole-cell quantities both water
#'   regime rows of a cell share (`method_drainage`, `method_runoff`).
#' - `soil_field_capacity`, `soil_wilting_point` and `soil_porosity`:
#'   [read_soil_hydraulic()] (`method_soil_hydraulic`).
#' - `som_share` (Meisinger, a fraction) and `soil_organic_carbon_pct`
#'   (IMAGE-GNM): the 0-30 cm soil organic carbon stock of the row's land use
#'   in `data$carbon_balance` ([build_carbon_balance()]; cropland classes
#'   area-weighted), over the HWSD topsoil mass of [read_hwsd_topsoil_mass()];
#'   `som_share` multiplies the carbon fraction by the SOM:SOC factor,
#'   `som_soc_factor_pribyl` (2) of [n_attenuation_constants] by default
#'   (`method_som`). A cell-polity the carbon balance lacks takes the cell's
#'   stock in its other polities, stamped `cell_other_polity`; a cell it
#'   lacks altogether (the carbon balance runs on the CRU and LPJmL grids
#'   only) takes the stock of the nearest cell holding the row's land use,
#'   stamped `nearest_cell_same_land_use=<km>km`.
#' - `c_input_t` (Meisinger): the C the row's inputs carry to the soil, t C per
#'   year: the crop's [build_soil_carbon_inputs()] carbon, pooled over the
#'   item's production items, on crop rows, and `data$carbon_balance`'s
#'   grassland input on the grass item (`method_c_input`). Both regime rows
#'   of an item carry the item's whole C; [build_nitrogen_balance()] divides
#'   it by the item's whole `n_input_full_nosom_t` into `cn_input`. A crop
#'   row without a
#'   carbon input of its item in its cell and polity keeps `c_input_t`
#'   missing, stamped `no_carbon_input`, and the balance gives it its item's
#'   C:N in the polity, else globally (`polity_item_cn`, `global_item_cn`).
#'   A grass row off the carbon balance carries the nearest grassland cell's
#'   C input density as `c_input_mgc_ha` (Mg C/ha/yr), which the balance puts
#'   on the key's grass area, stamped `nearest_cell_same_land_use=<km>km`.
#' - `root_depth_cm` (Meisinger): FAO-56 Table 22's maximum effective rooting
#'   depth ([fao56_root_depth]) of the row's production items, pooled by
#'   WHEP's hectares of each in the row's water regime: by default the upper
#'   end of each range on rainfed rows and the lower end on irrigated rows
#'   (Table 22 footnote 1); an item with no hectares in the regime takes the
#'   plain mean over its production items (`method_root_depth`).
#' - `climate` (ATL on wet rows, MED on dry), `irrig_cat` (`"Irrigated"` or
#'   `"Rainfed"`, from `water_regime`) and `tillage` (`"Tillage"`, the only
#'   class the Meisinger synthetic block holds; `method_tillage`).
#' - `surface_runoff_fraction` and `slope_m_per_km` (IMAGE-GNM):
#'   [read_image_gnm_runoff()] (`method_image_gnm_runoff`).
#' - `temp_c_annual_mean` (IMAGE-GNM): the year's CRU mean annual
#'   temperature, from the climate the classes are built on.
#' - `soil_texture_class` and `soil_drainage_class` (IMAGE-GNM):
#'   [read_soil_texture_fao()] and [read_soil_drainage()]
#'   (`method_soil_texture_fao`, `method_soil_drainage`).
#'
#' A balance cell a layer lacks takes the value of the nearest cell that holds
#' it, by great-circle distance between cell centres, and its stamp carries
#' the distance (`nearest_lpjml_cell=<km>km`, `nearest_hwsd_cell=<km>km`,
#' `nearest_image_gnm_cell=<km>km`). A row without a crop item carries no N:
#' it takes the HWSD topsoil depth (30 cm) and no C input, and a row of that
#' kind carrying N is refused. A row carrying no N whose soil carbon or root
#' depth is otherwise missing takes HWSD's topsoil carbon or the topsoil
#' depth, stamped `zero_n_row`; it leaches nothing.
#'
#' @section Errors:
#' - `whep_n_loss_drivers_rows`: `rows` is missing, empty, lacks a key
#'   column, or holds a missing coordinate or year, or a `water_regime` other
#'   than `"rainfed"` or `"irrigated"`.
#' - `whep_n_loss_drivers_unclassified`: a row is still without a class (for
#'   example a window-mean PET of zero), named by its stamp.
#' - `whep_n_loss_drivers_irrigated_cell_class`: an irrigated row carrying N,
#'   or one whose N is not supplied, has no class of its own and would take
#'   its cell's precipitation-only class. The rainfed/irrigated split gives
#'   an irrigated row N only where the item holds irrigated land, so such a
#'   row means the balance and the moisture regime disagree on the item's
#'   regime land; the error names the count and the items carrying most N.
#' - Every error of [build_cell_moisture_regime()], including
#'   `whep_absent_input` for an absent climate or irrigation layer.
#' - For the leaching drivers, all also `whep_absent_input`:
#'   `whep_n_loss_drivers_carbon_balance` (no `data$carbon_balance`),
#'   `whep_n_loss_drivers_soc` and `whep_n_loss_drivers_root_depth` (a row
#'   carrying N without soil carbon of its land use in any cell, or without
#'   root depth), `whep_n_loss_drivers_temperature` and
#'   `whep_n_loss_drivers_layer` (a layer with no complete cell, or without a
#'   year asked for); a layer whose values are all zero or missing aborts as
#'   [check_inputs_supplied()] does. `whep_n_loss_drivers_no_item_n`: a row
#'   without a crop item carries N.
#'
#' @param rows The balance rows to give drivers: `lon`, `lat`, `area_code`,
#'   `item_cbs_code` (`NA` on rows without a crop), `year` and `water_regime`
#'   (`"rainfed"` or `"irrigated"`), plus optionally `fert_type`, the loss
#'   `fert_type` of each row. With `fert_type`, the loss drivers are built for
#'   its non-missing `(key, fert_type)` pairs; without it, for every key and
#'   the seven loss `fert_type`s (`"Excreta"`, `"Liquid"`, `"Solid"`,
#'   `"SOM"`, `"Synthetic"`, `"Human"`, `"Recycling"`). The leaching drivers
#'   cover every key. `n_input_t`, the N of each row, is needed wherever an
#'   irrigated row has no class of its own: such a row must carry no N.
#' @param window,water,config As in [build_cell_moisture_regime()]. The
#'   window also sets the precipitation-only class of rows without a crop.
#' @param data Optional list of inputs, passed to
#'   [build_cell_moisture_regime()] (`climate`, `crop_depth`, `lpjml_years`,
#'   `lpjml_grid`, `regime_areas`, `country_grid`), plus `moisture_regime`, a
#'   [build_cell_moisture_regime()] result to use instead of building it.
#'   Climate is read once and shared. The leaching drivers read
#'   `carbon_balance` (required) and, each read when absent, `lpjml_drainage`
#'   and `lpjml_runoff` (annual [read_lpjml_hydrology()] tables),
#'   `soil_hydraulic`, `soil_mass`, `soil_texture_fao`, `soil_drainage`,
#'   `image_gnm_runoff` (their readers' results), `carbon_inputs` (a gridded
#'   [build_soil_carbon_inputs()] result) and `regime_areas`.
#' @param methods Named list choosing which leaching drivers are built and
#'   how: `leaching`, the [calculate_n_leaching()] method (`"ipcc_fracleach"`,
#'   the default, `"meisinger_drainage"` or `"image_gnm"`); `som_source`,
#'   `"carbon_balance"` (default) or `"hwsd"` (HWSD's observed topsoil
#'   carbon); `som_factor`, `"pribyl_2010"` (default, 2) or `"conventional"`
#'   (1.724); `root_depth`, `"by_regime"` (default), `"midpoint"`, `"upper"`,
#'   `"lower"`, `"topsoil_30cm"` or `"top_1m"`.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#'
#' @return A named list of two tibbles:
#'   - `n_balance_drivers`: one row per balance key (`lon`, `lat`,
#'     `area_code`, `item_cbs_code`, `year`, `water_regime`) and `fert_type`,
#'     with `moisture_regime` (`"wet"` or `"dry"`), `climate` (`"ATL"` or
#'     `"MED"`), `irrig_type`, `method_moisture_regime`, `method_climate`
#'     (`"from_moisture_regime"`) and `climate_fill_km`.
#'   - `n_balance_leaching_drivers`: one row per balance key, with
#'     `moisture_regime`, `method_moisture_regime` and `climate_fill_km`, plus
#'     the drivers of `methods$leaching` and their stamps (see Leaching
#'     drivers).
#'
#'   Both are join tables keyed on the balance key; the balance they are
#'   joined to carries the polity columns.
#'
#' @source
#' IPCC (2019), 2019 Refinement to the 2006 IPCC Guidelines for National
#' Greenhouse Gas Inventories, Volume 4, Chapter 11, Table 11.1, footnote 4
#' (p. 11.13). Allen, R. G., Pereira, L. S., Raes, D. & Smith, M. (1998).
#' Crop evapotranspiration. FAO Irrigation and Drainage Paper 56, Table 22.
#' Pribyl, D. W. (2010). A critical review of the conventional SOC to SOM
#' conversion factor. Geoderma 156, 75-83.
#' \doi{10.1016/j.geoderma.2010.02.003}.
#' @export
#'
#' @examples
#' build_n_loss_drivers(example = TRUE)
build_n_loss_drivers <- function(
  rows = NULL,
  window = c("trailing_30", "annual", "normal"),
  water = c("precip_irrig", "precip"),
  data = NULL,
  config = list(),
  methods = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_n_loss_drivers())
  }
  window <- rlang::arg_match(window)
  water <- rlang::arg_match(water)
  opts <- .nld_methods(methods)
  rows <- .nld_check_rows(rows)
  years <- sort(unique(rows$.year))
  spec <- .cmr_spec(years, window, water, config)
  data <- as.list(data)
  data$.cru_climate <- data$.cru_climate %||% .cmr_source_climate(data, spec)
  classes <- data$moisture_regime %||%
    build_cell_moisture_regime(years, window, water, data, config)
  cells <- .cmr_cell_classes(
    dplyr::distinct(rows, lon = .data$.lon, lat = .data$.lat),
    spec,
    data
  )
  drivers <- .nld_row_classes(rows, classes, cells) |>
    .nld_check_classified() |>
    .nld_atl_med()
  leaching <- .nld_leaching_drivers(drivers, opts, data, spec)
  .nld_tables(drivers, rows, leaching, opts$leaching)
}

# ---- Private helpers ---------------------------------------------------------

.nld_key <- function() {
  c("lon", "lat", "area_code", "item_cbs_code", "year", "water_regime")
}

# The loss fert_types of .nb_loss_rows() (R/n_balance.R), in the loss
# functions' Title-case vocabulary.
.nld_loss_fert_types <- function() {
  c("Excreta", "Liquid", "Solid", "SOM", "Synthetic", "Human", "Recycling")
}

# The driver columns the loss table carries, after the key; the leaching
# table's are .nld_leaching_cols() (R/n_leaching_drivers.R).
.nld_loss_cols <- function() {
  c(
    "moisture_regime",
    "climate",
    "irrig_type",
    "method_moisture_regime",
    "method_climate",
    "climate_fill_km"
  )
}

# The rows with normalised join columns beside their own (two-decimal cell
# centres and integer codes, the form build_cell_moisture_regime() keys on),
# so the tables are returned in the caller's own key values.
.nld_check_rows <- function(rows) {
  key <- .nld_key()
  if (!is.data.frame(rows) || nrow(rows) == 0L) {
    cli::cli_abort(
      c(
        "{.arg rows} must be a data frame of balance rows.",
        i = "Give the balance key: {.field {key}}."
      ),
      class = "whep_n_loss_drivers_rows"
    )
  }
  missing <- setdiff(key, names(rows))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "{.arg rows} is missing column{?s} {.field {missing}}.",
        i = "The drivers are per water regime: build them on the rainfed and
             irrigated rows of a split balance."
      ),
      class = "whep_n_loss_drivers_rows"
    )
  }
  .nld_check_row_values(rows)
  tibble::as_tibble(rows) |>
    dplyr::mutate(
      .lon = round(as.numeric(.data$lon), 2),
      .lat = round(as.numeric(.data$lat), 2),
      .area = as.integer(.data$area_code),
      .item = as.integer(.data$item_cbs_code),
      .year = as.integer(.data$year)
    )
}

.nld_check_row_values <- function(rows) {
  incomplete <- !stats::complete.cases(rows[c("lon", "lat", "year")])
  bad_regime <- !rows$water_regime %in% c("rainfed", "irrigated")
  if (any(incomplete) || any(bad_regime)) {
    cli::cli_abort(
      c(
        "{.arg rows} has {sum(incomplete)} row{?s} without a cell or year and
         {sum(bad_regime)} without a valid {.field water_regime}.",
        i = "Expected {.val rainfed} or {.val irrigated}."
      ),
      class = "whep_n_loss_drivers_rows"
    )
  }
}

# Each distinct balance key with its class: its own where
# build_cell_moisture_regime() classed it, else its cell's on precipitation
# alone, stamped with the reason.
.nld_row_classes <- function(rows, classes, cells) {
  join <- c(".lon", ".lat", ".area", ".item", "water_regime", ".year")
  own <- classes |>
    ensure_columns(tibble::tibble(climate_fill_km = double())) |>
    dplyr::transmute(
      .lon = round(as.numeric(.data$lon), 2),
      .lat = round(as.numeric(.data$lat), 2),
      .area = as.integer(.data$area_code),
      .item = as.integer(.data$item_cbs_code),
      water_regime = .data$water_regime,
      .year = as.integer(.data$year),
      moisture_regime = .data$moisture_regime,
      method_moisture_regime = .data$method_moisture_regime,
      climate_fill_km = .data$climate_fill_km,
      .own = TRUE
    )
  cell <- dplyr::transmute(
    cells,
    .lon = .data$lon,
    .lat = .data$lat,
    .year = as.integer(.data$year),
    .cell_regime = .data$moisture_regime,
    .cell_method = .data$method_moisture_regime,
    .cell_km = .data$climate_fill_km
  )
  distinct_cols <- unique(c(.nld_key(), join))
  rows |>
    .nld_key_n(distinct_cols) |>
    dplyr::left_join(own, by = join, relationship = "many-to-one") |>
    dplyr::left_join(
      cell,
      by = c(".lon", ".lat", ".year"),
      relationship = "many-to-one"
    ) |>
    .nld_take_cell_class()
}

# The N of each distinct balance key: the largest its rows carry (a key's
# balance row holds all its input N, each loss row one fert_type's part), and
# NA where the rows carry no N column or a row's N is missing, so an unknown
# N is never read as none.
.nld_key_n <- function(rows, cols) {
  if (!rlang::has_name(rows, "n_input_t")) {
    return(
      rows |>
        dplyr::distinct(dplyr::pick(dplyr::all_of(cols))) |>
        dplyr::mutate(.n_input_t = NA_real_)
    )
  }
  dplyr::summarise(
    rows,
    .n_input_t = max(.data$n_input_t),
    .by = dplyr::all_of(cols)
  )
}

.nld_take_cell_class <- function(rows) {
  from_cell <- is.na(rows$.own)
  .nld_check_irrigated_class(rows, from_cell)
  reason <- dplyr::if_else(
    is.na(rows$.item),
    "cell_rainfed_class=no_crop_item",
    "cell_rainfed_class=no_regime_area"
  )
  rows |>
    dplyr::mutate(
      moisture_regime = dplyr::if_else(
        .env$from_cell,
        .data$.cell_regime,
        .data$moisture_regime
      ),
      method_moisture_regime = dplyr::if_else(
        .env$from_cell,
        .moisture_add_field(.data$.cell_method, .env$reason),
        .data$method_moisture_regime
      ),
      climate_fill_km = dplyr::if_else(
        .env$from_cell,
        .data$.cell_km,
        .data$climate_fill_km
      )
    ) |>
    dplyr::select(
      -".own",
      -".cell_regime",
      -".cell_method",
      -".cell_km"
    )
}

# The precipitation-only class is the class of a row with no irrigation. An
# irrigated row reaches it only when the moisture regime holds no irrigated
# land of its item, and the rainfed/irrigated split gives an irrigated row N
# only where the item holds some: an irrigated row with N here is the two
# disagreeing on the item's regime land, and its irrigated N would be priced
# as rainfed. Irrigated rows without N keep the cell class, stamped.
.nld_check_irrigated_class <- function(rows, from_cell) {
  bad <- from_cell &
    rows$water_regime == "irrigated" &
    !rows$.n_input_t %in% 0
  if (!any(bad)) {
    return(invisible(rows))
  }
  items <- tibble::tibble(
    item = rows$.item[bad],
    n_t = rows$.n_input_t[bad]
  ) |>
    dplyr::summarise(n_t = sum(.data$n_t), .by = "item") |>
    dplyr::arrange(dplyr::desc(.data$n_t)) |>
    utils::head(5L)
  bullets <- rlang::set_names(
    paste0(
      dplyr::coalesce(as.character(items$item), "no crop item"),
      ": ",
      dplyr::if_else(
        is.na(items$n_t),
        "N not supplied",
        paste(signif(items$n_t, 6), "t N")
      )
    ),
    rep("*", nrow(items))
  )
  n_bad <- sum(bad)
  cli::cli_abort(
    c(
      "{n_bad} irrigated balance row{?s} with N, or without a supplied
       {.field n_input_t}, {?has/have} no class of {?its/their} own.",
      x = "The cell's precipitation-only class would price irrigated N as
           rainfed: the balance gives the row irrigated N where the moisture
           regime holds no irrigated land of its item.",
      i = "Items carrying most N ({.field item_cbs_code}):",
      bullets
    ),
    class = "whep_n_loss_drivers_irrigated_cell_class"
  )
}

# Every row must end with a class: the loss methods refuse a missing one, and
# no class here is a default. A row the classifier left without one says why
# in its stamp.
.nld_check_classified <- function(drivers) {
  missing <- is.na(drivers$moisture_regime)
  if (!any(missing)) {
    return(drivers)
  }
  counts <- table(dplyr::coalesce(
    drivers$method_moisture_regime[missing],
    "no climate for the cell"
  ))
  bullets <- rlang::set_names(
    paste0(names(counts), ": ", as.integer(counts)),
    rep("*", length(counts))
  )
  n_missing <- sum(missing)
  cli::cli_abort(
    c(
      "{n_missing} balance row{?s} {?has/have} no wet/dry class.",
      bullets,
      i = "Every row must end with a class; none is defaulted."
    ),
    class = "whep_n_loss_drivers_unclassified"
  )
}

# The Atlantic/Mediterranean drivers, from the wet/dry class.
.nld_atl_med <- function(drivers) {
  rice <- drivers$.item %in% .n_rice_item_cbs_codes()
  drivers |>
    dplyr::mutate(
      climate = dplyr::if_else(.data$moisture_regime == "wet", "ATL", "MED"),
      method_climate = "from_moisture_regime",
      irrig_type = dplyr::case_when(
        .env$rice ~ "Flooded",
        .data$climate == "MED" & .data$water_regime == "irrigated" ~
          "Med_average",
        .data$climate == "MED" ~ "Rainfed",
        .default = "Tier_1"
      )
    )
}

# The two tables, in the caller's own key values.
.nld_tables <- function(drivers, rows, leaching, method) {
  key <- .nld_key()
  drivers <- dplyr::select(
    drivers,
    dplyr::all_of(key),
    dplyr::all_of(.nld_loss_cols())
  )
  loss_keys <- if (rlang::has_name(rows, "fert_type")) {
    rows |>
      dplyr::filter(!is.na(.data$fert_type)) |>
      dplyr::distinct(dplyr::pick(dplyr::all_of(c(key, "fert_type"))))
  } else {
    tidyr::expand_grid(
      dplyr::distinct(drivers, dplyr::pick(dplyr::all_of(key))),
      fert_type = .nld_loss_fert_types()
    )
  }
  list(
    n_balance_drivers = loss_keys |>
      dplyr::left_join(drivers, by = key, relationship = "many-to-one") |>
      tibble::as_tibble(),
    n_balance_leaching_drivers = leaching |>
      dplyr::select(dplyr::all_of(c(key, .nld_leaching_cols(method)))) |>
      tibble::as_tibble()
  )
}
