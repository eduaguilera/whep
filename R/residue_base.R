# The gross crop-residue base behind `get_primary_residues()` (whep#1448).
#
# The `crop_residues` pin's residue is the predecessor's harvest-index model:
# product times the `kg_residue_kg_product_FM` ratio of `biomass_coefs`, times
# the region's Wirsenius (2000) Table 3.16 residue:product ratio over West
# Europe's, times a harvest-index change factor per region and year (see
# `.residue_gross_from_recovered()`). Read as gross residue, world cereal
# residue in dry matter was 16% above the only published global series,
# Smerald, Rahimi & Scheer (2023), doi:10.1038/s41597-023-02587-0, and 25%
# above their constant-ratio method in every year. Three things made it so:
#
# 1. The regional ratio was looked up by `regions_full$region_HANPP`, which
#    carries Wirsenius's eight region NAMES but not his membership: Southeast
#    Asia, Russia, Belarus and the Caucasus took South & Central Asia's ratio
#    (whep#1430). That is a lookup error and is corrected for every crop here.
# 2. The regional factor was Wirsenius's ratio over West Europe's, which is
#    only coherent if `biomass_coefs` holds the West Europe ratio. For cereals
#    it does not (wheat 1.34, barley 1.18, sorghum 1.70 and maize 0.96 kg DM
#    per kg DM, against Wirsenius's West Europe 1.0, 1.0, 1.2 and 1.2).
# 3. Wirsenius's ratios are early-1990s harvest indices, applied unchanged to
#    every year after 2000.
#
# Cereals are therefore recomputed from the pin's own production and harvested
# area with `calculate_crop_residues()`, the estimator that already gives the
# soil carbon and nitrogen chain its residue (`.sci_npp_from_primary_prod()`),
# so both sides of WHEP read the same straw (whep#1003). `"wirsenius"` keeps the
# predecessor's model with 1. and 2. corrected. Other crops keep the pin's
# ratio with 1. corrected: no published global series exists to check a
# replacement against (whep#1399).

# Correct the pin's gross residue and stamp each row with how it was priced.
# Cereal rows with an area are recomputed by `method`; every other row keeps
# the pin's ratio, re-keyed on Wirsenius's own region membership.
.residue_ratio_corrected <- function(residues, products, method) {
  cereal <- .residue_is_cereal(residues$item_prod) &
    !is.na(residues$area_code)
  others <- residues[!cereal, ] |>
    .residue_ratio_membership() |>
    dplyr::mutate(method_residue = "pin")
  cereals <- residues[cereal, ]
  if (nrow(cereals) == 0L) {
    return(others)
  }
  cereals <- if (method == "wirsenius") {
    .residue_cereal_wirsenius(cereals)
  } else {
    .residue_cereal_from_products(cereals, products, method)
  }
  dplyr::bind_rows(others, cereals)
}

# Whether each pin crop name is a cereal: its CBS item belongs to the
# `Cereals` commodity group of `items_full`, the scope of the Smerald et al.
# (2023) series these rows are checked against (`validation/residue_base_dm.R`).
.residue_is_cereal <- function(item_prod) {
  cereal_cbs <- whep::items_full |>
    dplyr::filter(.data$comm_group == "Cereals") |>
    dplyr::pull("item_cbs_code")
  cereal_names <- whep::items_prod_full |>
    dplyr::filter(.data$item_cbs_code %in% cereal_cbs) |>
    dplyr::pull("item_prod")
  as.character(item_prod) %in% cereal_names
}

# Re-key the pin's regional residue:product ratio on Wirsenius's membership.
# The predecessor multiplied by `residue_dm_product_dm` of the area's HANPP
# region; dividing that out and multiplying by the ratio of its Wirsenius
# region changes nothing else in the row. Rows with no area reach no region
# and keep the pin's figure, as in `.residue_gross_from_recovered()`.
.residue_ratio_membership <- function(dt) {
  ratios <- .residue_ratio_lookup(dt)
  dt |>
    dplyr::left_join(
      ratios,
      by = c("item_prod", "area_code"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      prod_ygpit_mg = dplyr::if_else(
        is.na(.data$area_code),
        .data$prod_ygpit_mg,
        .data$prod_ygpit_mg * .data$ratio_wirsenius / .data$ratio_hanpp
      )
    ) |>
    dplyr::select(-dplyr::starts_with("ratio_"))
}

# Wirsenius (2000) Table 3.16 applied directly: residue dry matter is product
# dry matter times the ratio of the area's Wirsenius region, times the pin's
# own harvest-index change factor for the row's HANPP region and year. That
# factor is not shipped, but it is what is left of a pin row once the
# `biomass_coefs` ratio and the HANPP regional factor are divided out, so the
# row is rescaled by the Wirsenius ratio over the predecessor's
# `biomass_coefs` ratio times its regional factor. The result stays fresh
# matter, through the same `Residue_kgDM_kgFM` that converts it back.
.residue_cereal_wirsenius <- function(rows) {
  coefs <- .residue_biomass_ratio(rows$name_biomass)
  rows |>
    dplyr::left_join(
      .residue_ratio_lookup(rows),
      by = c("item_prod", "area_code"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      coefs,
      by = "name_biomass",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      prod_ygpit_mg = .data$prod_ygpit_mg *
        .data$ratio_wirsenius /
        (.data$biomass_ratio_dm * .data$ratio_hanpp / .data$ratio_west_europe),
      method_residue = "wirsenius"
    ) |>
    dplyr::select(-dplyr::starts_with("ratio_"), -"biomass_ratio_dm")
}

# The `biomass_coefs` residue:product ratio in dry matter per dry matter,
# keyed on the pin's `name_biomass`, as the predecessor applied it.
.residue_biomass_ratio <- function(names_biomass) {
  coefs <- whep::biomass_coefs |>
    tibble::as_tibble() |>
    dplyr::filter(.data$Name_biomass %in% names_biomass) |>
    dplyr::distinct(
      name_biomass = .data$Name_biomass,
      biomass_ratio_dm = .data$kg_residue_kg_product_FM *
        .data$Residue_kgDM_kgFM /
        .data$Product_kgDM_kgFM
    )
  missing <- setdiff(
    unique(names_biomass),
    coefs$name_biomass[
      !is.na(coefs$biomass_ratio_dm)
    ]
  )
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "No {.field biomass_coefs} residue:product ratio for the cereal
         residue of {.val {missing}}.",
        "i" = "The {.val wirsenius} method needs it to divide the
          predecessor's ratio out of the pin."
      ),
      class = "whep_residue_cereal_unpriced"
    )
  }
  coefs
}

# Per (pin crop, area): Wirsenius Table 3.16 residue:product ratio of the
# crop's category in the area's HANPP region (the one the pin was written
# with), in its Wirsenius region, and in West Europe.
.residue_ratio_lookup <- function(dt) {
  ratios <- whep::whep_coef_table("residue_recovery") |>
    dplyr::select(
      "cat_krausmann",
      region = "region_krausmann",
      ratio = "residue_dm_product_dm"
    )
  west <- dplyr::filter(ratios, .data$region == "West Europe")
  out <- dt |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::distinct(.data$item_prod, .data$area_code) |>
    .residue_item_category() |>
    dplyr::left_join(.residue_area_regions(), by = "area_code") |>
    dplyr::left_join(
      dplyr::rename(ratios, region_hanpp = "region", ratio_hanpp = "ratio"),
      by = c("cat_krausmann", "region_hanpp")
    ) |>
    dplyr::left_join(
      dplyr::rename(
        ratios,
        region_wirsenius = "region",
        ratio_wirsenius = "ratio"
      ),
      by = c("cat_krausmann", "region_wirsenius")
    ) |>
    dplyr::left_join(
      dplyr::select(west, "cat_krausmann", ratio_west_europe = "ratio"),
      by = "cat_krausmann"
    )
  .check_residue_ratios(out)
  dplyr::select(
    out,
    "item_prod",
    "area_code",
    "ratio_hanpp",
    "ratio_wirsenius",
    "ratio_west_europe"
  )
}

# Crop category of each pin crop name, as `.residue_pin_recovery_rates()`
# resolves it: production item code first, then `Cat_Krausmann`.
.residue_item_category <- function(dt) {
  categories <- whep::items_prod_full |>
    dplyr::distinct(
      item_prod_code = as.character(.data$item_prod_code),
      cat_krausmann = .data$Cat_Krausmann
    )
  dt |>
    add_item_prod_code(name_column = "item_prod") |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code)) |>
    dplyr::left_join(categories, by = "item_prod_code") |>
    dplyr::select(-"item_prod_code")
}

# Each area's HANPP region, UN M49 sub-region and Wirsenius region.
.residue_area_regions <- function() {
  whep::regions_full |>
    dplyr::filter(!is.na(.data$code)) |>
    dplyr::transmute(
      area_code = as.integer(.data$code),
      region_hanpp = .data$region_HANPP,
      region_un_sub = .data$region_UN_sub
    ) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE) |>
    dplyr::mutate(
      region_wirsenius = .residue_wirsenius_region(
        .data$region_hanpp,
        .data$region_un_sub
      )
    )
}

# A pin row with an area whose ratio cannot be found was not written by the
# rule being corrected. `.residue_gross_from_recovered()` has already refused a
# row whose recovery rate is missing, and both live in one table, so this
# only fires if the table loses a row.
.check_residue_ratios <- function(out) {
  bad <- is.na(out$ratio_hanpp) |
    is.na(out$ratio_wirsenius) |
    is.na(out$ratio_west_europe)
  if (!any(bad)) {
    return(invisible(NULL))
  }
  crops <- sort(unique(as.character(out$item_prod[bad])))
  cli::cli_abort(
    c(
      "No Wirsenius residue:product ratio for some crop residue rows.",
      "i" = "Crops: {.val {crops}}."
    ),
    class = "whep_residue_pin_ratio"
  )
}

# Wirsenius's own region for a row, from its HANPP region and UN M49
# sub-region (whep#1398, whep#1430). The HANPP labels carry Wirsenius's eight
# region NAMES but not his membership (Table 3.1, p. 58): HANPP files Southeast
# Asia, Russia and Belarus, and the Caucasus under South and Central Asia,
# where Wirsenius has them in East Asia, East Europe and North Africa & West
# Asia. `residue_feed_regions.csv` lists the (HANPP, sub-region) pairs that
# differ; every other pair keeps its HANPP label. The table and this helper
# are the ones PR #1431 (Wirsenius Table 3.20 feed shares) introduces, so the
# feed shares, the recovery rates and the residue ratio share one membership.
#
# Not separable on these two labels, and so left on the HANPP label: Greece,
# Serbia and Montenegro, which Wirsenius has in East Europe (Yugoslavia) and
# HANPP in West Europe, and which share their pair with Italy, Spain and
# Portugal.
.residue_wirsenius_region <- function(region_hanpp, region_un_sub) {
  overrides <- whep::whep_coef_table("residue_feed_regions") |>
    dplyr::select("region_hanpp", "region_un_sub", "region_wirsenius")
  # A NA sub-region is a key here (the USSR row), and dplyr joins NA to NA.
  tibble::tibble(region_hanpp, region_un_sub) |>
    dplyr::left_join(overrides, by = c("region_hanpp", "region_un_sub")) |>
    dplyr::mutate(
      region_wirsenius = dplyr::coalesce(
        .data$region_wirsenius,
        .data$region_hanpp
      )
    ) |>
    dplyr::pull("region_wirsenius")
}

# Cereal residue from the pin's own production and harvested area, through
# `calculate_crop_residues()` with its modern-variety harvest-index correction
# (keyed on the HANPP region, the membership that table is written in). The
# pin's `Product` rows equal the `primary_prod` pin's tonnes, so the crop base
# is the one the residue rows were built on. The result is stored as fresh
# matter through the crop's own `Residue_kgDM_kgFM`, the coefficient
# `get_primary_residues()` converts it back with, so `value_dm` is the
# estimator's dry matter.
#
# Each cereal books its residue to one CBS residue item in the pin (Straw, or
# Other crop residues for fonio), which the residue rows give; the production
# rows drive the estimate.
.residue_cereal_from_products <- function(rows, products, method) {
  items <- .residue_cereal_items(rows)
  crops <- .residue_cereal_production(products, items$item_prod)
  .check_cereal_products(rows, crops)
  crops |>
    dplyr::left_join(.residue_area_regions(), by = "area_code") |>
    add_item_prod_code(name_column = "item_prod") |>
    dplyr::transmute(
      year = .data$year,
      area = .data$area,
      area_code = .data$area_code,
      item_prod = .data$item_prod,
      item_prod_code = as.character(.data$item_prod_code),
      production_t = .data$production_t,
      area_ha = .data$area_ha,
      region_hanpp = .data$region_hanpp
    ) |>
    calculate_crop_residues(method = method) |>
    .check_cereal_priced() |>
    dplyr::left_join(items, by = "item_prod", relationship = "many-to-one") |>
    dplyr::mutate(
      prod_ygpit_mg = .data$residue_dm_t / .data$residue_kgdm_kgfm,
      product_residue = "Residue",
      method_residue = method
    ) |>
    dplyr::select(
      "year",
      "area",
      "area_code",
      "item_prod",
      "item_cbs",
      "item_cbs_crop",
      "name_biomass",
      "product_residue",
      "prod_ygpit_mg",
      "method_residue"
    )
}

# The residue item, CBS crop and biomass name each cereal books its residue
# under, and that biomass name's residue dry-matter content. One per crop: a
# cereal whose residue the pin split over two items could not be recomputed
# from one production row without choosing a split.
.residue_cereal_items <- function(rows) {
  items <- rows |>
    dplyr::distinct(
      .data$item_prod,
      .data$item_cbs,
      .data$item_cbs_crop,
      .data$name_biomass
    )
  split <- unique(items$item_prod[duplicated(items$item_prod)])
  if (length(split) > 0L) {
    cli::cli_abort(
      c(
        "The crop-residue pin books one cereal's residue under more than one
         item: {.val {split}}.",
        "i" = "Cereal residue is recomputed from production, one residue row
          per crop, so the split cannot be kept."
      ),
      class = "whep_residue_cereal_items"
    )
  }
  kgdm <- whep::biomass_coefs |>
    tibble::as_tibble() |>
    dplyr::distinct(
      name_biomass = .data$Name_biomass,
      residue_kgdm_kgfm = .data$Residue_kgDM_kgFM
    )
  items |>
    dplyr::left_join(kgdm, by = "name_biomass", relationship = "many-to-one")
}

# Cereal production (t fresh) and harvested area (ha) per area, crop and year,
# from the pin's `Product` rows.
.residue_cereal_production <- function(products, cereals) {
  if (!all(c("prod_ygpit_mg", "area_ygpit_ha") %in% names(products))) {
    cli::cli_abort(
      "The crop-residue pin has no {.field area_ygpit_ha} column, so cereal
       residue cannot be estimated from yield.",
      class = "whep_residue_cereal_no_product"
    )
  }
  products |>
    dplyr::filter(
      !is.na(.data$area_code),
      .data$item_prod %in% cereals
    ) |>
    dplyr::summarise(
      production_t = sum(.data$prod_ygpit_mg, na.rm = TRUE),
      area_ha = sum(.data$area_ygpit_ha, na.rm = TRUE),
      .by = c("year", "area", "area_code", "item_prod")
    ) |>
    dplyr::filter(.data$production_t > 0)
}

# Every cereal residue row must have the production it is recomputed from,
# and every production row the harvested area the yield-dependent estimate
# needs. Without either, the residue would come out as zero and be dropped: an
# absent input read as no residue. Neither happens on the real pin.
.check_cereal_products <- function(rows, crops) {
  keys <- c("year", "area_code", "item_prod")
  has_mass <- !is.na(rows$prod_ygpit_mg) & rows$prod_ygpit_mg > 0
  orphan <- dplyr::anti_join(
    dplyr::distinct(rows[has_mass, keys]),
    crops,
    by = keys
  )
  no_area <- crops[is.na(crops$area_ha) | crops$area_ha <= 0, ]
  if (nrow(orphan) == 0L && nrow(no_area) == 0L) {
    return(invisible(NULL))
  }
  bad <- sort(unique(c(orphan$item_prod, no_area$item_prod)))
  n_orphan <- nrow(orphan)
  n_no_area <- nrow(no_area)
  cli::cli_abort(
    c(
      "Cereal residue cannot be recomputed from the pin's production.",
      "i" = "{n_orphan} residue rows have no production row and {n_no_area}
        production rows have no harvested area.",
      "i" = "Crops: {.val {bad}}."
    ),
    class = "whep_residue_cereal_no_product"
  )
}

# `calculate_crop_residues()` fills a crop it has no coefficient for with zero.
# For a cereal with production that would be residue silently lost.
.check_cereal_priced <- function(x) {
  bad <- x$production_t > 0 &
    (is.na(x$residue_dm_t) | x$residue_dm_t <= 0)
  if (!any(bad)) {
    return(x)
  }
  crops <- sort(unique(as.character(x$item_prod[bad])))
  cli::cli_abort(
    c(
      "No residue estimate for cereals with production: {.val {crops}}.",
      "i" = "{.fn calculate_crop_residues} has no coefficient for them."
    ),
    class = "whep_residue_cereal_unpriced"
  )
}
