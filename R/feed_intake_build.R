.normalise_feed_cbs <- function(cbs) {
  cbs <- tibble::as_tibble(cbs) |>
    dplyr::rename_with(tolower)
  if (
    !rlang::has_name(cbs, "item_cbs_code") &&
      rlang::has_name(cbs, "item_code")
  ) {
    cbs <- dplyr::rename(cbs, item_cbs_code = "item_code")
  }
  if (
    !rlang::has_name(cbs, "feed") &&
      all(rlang::has_name(cbs, c("element", "value")))
  ) {
    return(
      cbs |>
        dplyr::mutate(
          element = stringr::str_replace_all(
            stringr::str_to_lower(.data$element),
            "[^a-z0-9]+",
            "_"
          ),
          element = stringr::str_replace_all(.data$element, "_$", "")
        ) |>
        dplyr::filter(.data$element == "feed") |>
        dplyr::summarise(
          feed = sum(.data$value, na.rm = TRUE),
          .by = c("year", "area_code", "item_cbs_code")
        )
    )
  }

  cbs |>
    dplyr::select(year, area_code, item_cbs_code, feed)
}

.normalise_feed_primary <- function(primary_prod) {
  primary_prod <- tibble::as_tibble(primary_prod) |>
    dplyr::rename_with(tolower)
  if (
    !rlang::has_name(primary_prod, "item_prod_code") &&
      rlang::has_name(primary_prod, "item_code")
  ) {
    primary_prod <- dplyr::rename(
      primary_prod,
      item_prod_code = "item_code"
    )
  }
  if (
    !rlang::has_name(primary_prod, "item_cbs_code") &&
      rlang::has_name(primary_prod, "item_code_cbs")
  ) {
    primary_prod <- dplyr::rename(
      primary_prod,
      item_cbs_code = "item_code_cbs"
    )
  }
  primary_prod |>
    dplyr::select(
      year,
      area_code,
      item_prod_code,
      dplyr::any_of("item_cbs_code"),
      dplyr::any_of("live_anim_code"),
      unit,
      value
    ) |>
    dplyr::mutate(year = as.integer(year), area_code = as.integer(area_code))
}

.build_bouwman_fcr <- function(conv_bouwman, years) {
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0) {
    return(.empty_bouwman_fcr())
  }

  conv <- tibble::as_tibble(conv_bouwman) |>
    dplyr::rename(feed_type = dplyr::any_of("feedtype")) |>
    dplyr::rename(region_bouwman = dplyr::any_of("region")) |>
    dplyr::mutate(year = as.integer(.data$year))
  all_years <- seq(
    min(c(years, 1950L, conv$year), na.rm = TRUE),
    max(c(years, conv$year), na.rm = TRUE)
  )

  conv |>
    dplyr::mutate(
      conversion_tot = sum(.data$conversion, na.rm = TRUE),
      dm_share = .data$conversion / .data$conversion_tot,
      .by = c("item_bouwman", "year", "region_bouwman")
    ) |>
    (\(df) {
      df |>
        dplyr::left_join(
          df |>
            dplyr::filter(
              .data$item_bouwman %in%
                c("Dairy cattle", "Beef cattle", "Sheep and goats")
            ) |>
            dplyr::summarise(
              dm_share_scav_grazers = mean(.data$dm_share, na.rm = TRUE),
              .by = c("year", "region_bouwman", "feed_type")
            ) |>
            dplyr::filter(.data$feed_type == "scavenging") |>
            dplyr::select(-"feed_type"),
          by = c("year", "region_bouwman")
        )
    })() |>
    (\(df) {
      df |>
        dplyr::left_join(
          df |>
            dplyr::filter(.data$feed_type == "scavenging") |>
            dplyr::rename(dm_share_scav = "dm_share") |>
            dplyr::select(
              year,
              region_bouwman,
              item_bouwman,
              dm_share_scav
            ),
          by = c("year", "region_bouwman", "item_bouwman")
        )
    })() |>
    dplyr::mutate(dm_share_scav = tidyr::replace_na(.data$dm_share_scav, 0)) |>
    (\(df) {
      dplyr::bind_rows(
        df,
        df |>
          dplyr::filter(
            .data$item_bouwman %in% c("Pigs", "Poultry"),
            .data$feed_type == "crops"
          ) |>
          dplyr::mutate(feed_type = "scavenging")
      )
    })() |>
    (\(df) {
      dplyr::bind_rows(
        df,
        df |>
          dplyr::filter(.data$year == 1970L) |>
          dplyr::mutate(
            year = 1950L,
            dm_share_scav_grazers = dplyr::if_else(
              .data$dm_share_scav_grazers < 0.1,
              0.1,
              .data$dm_share_scav_grazers
            )
          )
      )
    })() |>
    dplyr::mutate(
      fcr = dplyr::case_when(
        .data$dm_share_scav != 0 ~ .data$conversion,
        .data$feed_type == "scavenging" ~
          .data$conversion_tot * .data$dm_share_scav_grazers,
        TRUE ~ .data$conversion * (1 - .data$dm_share_scav_grazers)
      )
    ) |>
    dplyr::right_join(tibble::tibble(year = all_years), by = "year") |>
    tidyr::complete(
      year,
      tidyr::nesting(feed_type, region_bouwman, item_bouwman)
    ) |>
    dplyr::arrange(
      .data$year,
      .data$feed_type,
      .data$region_bouwman,
      .data$item_bouwman
    ) |>
    fill_linear(
      fcr,
      time_col = year,
      .by = c("feed_type", "region_bouwman", "item_bouwman")
    ) |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::select(year, region_bouwman, item_bouwman, feed_type, fcr)
}

.build_feed_demand <- function(
  primary_prod,
  items_prod_full,
  animals_codes,
  conv_krausmann,
  regs_codes,
  fcr
) {
  demand_fcr <- .build_feed_demand_fcr(
    primary_prod,
    items_prod_full,
    animals_codes,
    regs_codes,
    fcr
  )
  demand_head <- .build_feed_demand_head(
    primary_prod,
    conv_krausmann,
    regs_codes,
    fcr
  )

  dplyr::bind_rows(demand_fcr, demand_head) |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$feed_type))
}

.build_feed_demand_fcr <- function(
  primary_prod,
  items_prod_full,
  animals_codes,
  regs_codes,
  fcr
) {
  prod_lookup <- .feed_prod_lookup(items_prod_full)

  primary_prod |>
    dplyr::filter(.data$unit == "tonnes", !is.na(.data$value)) |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code)) |>
    dplyr::select(-dplyr::any_of("live_anim_code")) |>
    dplyr::left_join(prod_lookup, by = "item_prod_code") |>
    dplyr::filter(
      .data$group == "Livestock products",
      !is.na(.data$live_anim_code)
    ) |>
    dplyr::summarise(
      value = sum(.data$value, na.rm = TRUE),
      .by = c(
        "year",
        "area_code",
        "item_prod_code",
        "live_anim",
        "live_anim_code"
      )
    ) |>
    .assign_bouwman_feed_class(animals_codes) |>
    dplyr::left_join(regs_codes, by = "area_code") |>
    .with_region_weight() |>
    dplyr::left_join(
      fcr,
      by = c("year", "region_bouwman", "item_bouwman"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      demand_aft = .data$value * .data$fcr * .data$region_weight
    ) |>
    dplyr::filter(!is.na(.data$fcr)) |>
    dplyr::summarise(
      demand_aft = sum(.data$demand_aft, na.rm = TRUE),
      .by = c(
        "year",
        "area_code",
        "live_anim",
        "live_anim_code",
        "feed_type"
      )
    )
}

.build_feed_demand_head <- function(
  primary_prod,
  conv_krausmann,
  regs_codes,
  fcr
) {
  demand_shares_grazers <- fcr |>
    dplyr::filter(
      .data$item_bouwman %in%
        c("Dairy cattle", "Beef cattle", "Sheep and goats")
    ) |>
    dplyr::mutate(
      fcr_tot = sum(.data$fcr, na.rm = TRUE),
      dm_share = .data$fcr / .data$fcr_tot,
      .by = c("item_bouwman", "year", "region_bouwman")
    ) |>
    dplyr::summarise(
      dm_share_grazers = mean(.data$dm_share, na.rm = TRUE),
      .by = c("year", "region_bouwman", "feed_type")
    )

  kraus <- tibble::as_tibble(conv_krausmann) |>
    dplyr::transmute(
      live_anim_code = as.integer(.data$item_cbs_code),
      live_anim = .data$species,
      cf_kraus = .data$conversion
    )

  primary_prod |>
    dplyr::filter(.data$unit == "heads", !is.na(.data$value)) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      live_anim_code = as.integer(.data$item_prod_code),
      value = .data$value
    ) |>
    dplyr::inner_join(kraus, by = "live_anim_code") |>
    dplyr::left_join(regs_codes, by = "area_code") |>
    .with_region_weight() |>
    dplyr::left_join(
      demand_shares_grazers,
      by = c("year", "region_bouwman")
    ) |>
    dplyr::mutate(
      demand_aft = .data$value *
        .data$cf_kraus *
        .data$dm_share_grazers *
        .data$region_weight
    ) |>
    dplyr::summarise(
      demand_aft = sum(.data$demand_aft, na.rm = TRUE),
      .by = c("year", "area_code", "live_anim", "live_anim_code", "feed_type")
    )
}

.feed_items_lookup <- function(items_full) {
  tibble::as_tibble(items_full) |>
    dplyr::transmute(
      item_cbs = .data$item_cbs,
      item_cbs_code = as.integer(.data$item_cbs_code),
      feedtype_graniv = .data$feedtype_graniv,
      feedtype_grazers = .data$feedtype_grazers,
      Name_biomass = .data$Name_biomass
    ) |>
    dplyr::distinct(.data$item_cbs_code, .keep_all = TRUE)
}

.feed_prod_lookup <- function(items_prod_full) {
  tibble::as_tibble(items_prod_full) |>
    dplyr::transmute(
      item_prod_code = as.character(.data$item_prod_code),
      group = .data$group,
      live_anim = .data$live_anim,
      live_anim_code = as.integer(.data$live_anim_code)
    ) |>
    dplyr::filter(!is.na(.data$item_prod_code)) |>
    dplyr::distinct(.data$item_prod_code, .keep_all = TRUE)
}

.feed_biomass_lookup <- function(biomass_coefs) {
  tibble::as_tibble(biomass_coefs) |>
    dplyr::transmute(
      Name_biomass = .data$Name_biomass,
      product_kgdm_kgfm = .data$Product_kgDM_kgFM
    ) |>
    dplyr::distinct(.data$Name_biomass, .keep_all = TRUE)
}

# Bouwman feed region per reporting bucket, as a WEIGHTED lookup: one row per
# (area_code, region_bouwman) carrying the share of the bucket's herd that sits
# in that region. Every area the crosswalk resolves directly gets a single row
# at weight 1, so the join and every product downstream is unchanged for it.
#
# `fallback` fills only the buckets the crosswalk leg leaves empty (whep#467):
# - "member_mix": Rest of World (999) is split across its members' own regions.
# - "none": the status quo, where such a bucket has no region and its feed
#   demand is dropped by `.warn_dropped_mix()` and the share joins.
.feed_region_lookup <- function(
  crosswalk = whep::polity_area_crosswalk,
  fallback = c("member_mix", "none")
) {
  fallback <- rlang::arg_match(fallback)
  direct <- tibble::as_tibble(crosswalk) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      region_bouwman = .data$region
    ) |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$region_bouwman)) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE) |>
    dplyr::mutate(region_weight = 1)
  if (fallback == "none") {
    return(direct)
  }
  dplyr::bind_rows(
    direct,
    dplyr::filter(
      .feed_region_fallbacks(),
      !.data$area_code %in% direct$area_code
    )
  )
}

# The Rest-of-World bucket's Bouwman regions, weighted by the herd its members
# actually carry.
#
# Bucket 999 folds 62 FAOSTAT reporting areas (`folded_reporting_areas()`), 58
# of which have a Bouwman region of their own; the fold discards all 58 and
# leaves the bucket with none. Only 13 of the 62 carry any livestock at all, so
# the bucket's mix is measurable rather than assumed: the weights below are the
# members' livestock units summed over 1850-2023, from a full
# `get_primary_production()` run with
# `options(whep.unfold_rest_of_world = TRUE)` (see `folded_reporting_areas()`),
# which promotes each member to its own `polity_area_code`. Livestock units are
# used rather than the feed demand itself because the IPCC Tier-2 demand model
# is region-dependent, which would make the weights circular; the two bases
# agree on the ranking and on the dominant region (Middle East 0.69 by
# livestock units, 0.82 by Tier-2 demand).
#
# Cross-checked against a source that touches neither the fold nor the unfold
# switch (which is itself unreliable for stocks, whep#589): raw FAOSTAT
# `Stocks` from the `faostat-production` pin converted with `liv_lu_coefs`
# gives the same 13 members and region shares within 0.019 of these (Middle
# East 0.688 there against 0.693 here).
#
# Regenerate with `inst/scripts/row_feed_region_weights.R` after a polities
# re-sync, since a re-sync can move members in or out of the bucket.
.feed_region_fallbacks <- function() {
  .row_member_herds() |>
    dplyr::summarise(
      herd = sum(.data$livestock_units),
      .by = "region_bouwman"
    ) |>
    dplyr::transmute(
      area_code = 999L,
      region_bouwman = .data$region_bouwman,
      region_weight = .data$herd / sum(.data$herd)
    )
}

.row_member_herds <- function() {
  tibble::tribble(
    ~member_area_code, ~member_area_name,      ~region_bouwman,   ~livestock_units,
    212L,              "Syrian Arab Republic", "Middle East",     20797998,
    209L,              "Eswatini",             "Southern Africa",  5429610,
    154L,              "North Macedonia",      "Eastern Europe",   1380710,
    153L,              "New Caledonia",        "Oceania",          1329304,
    299L,              "Palestine",            "Middle East",       683313,
    182L,              "Reunion",              "Eastern Africa",    525447,
    87L,               "Guadeloupe",           "Central America",   352357,
    135L,              "Martinique",           "Central America",   230170,
    61L,               "Equatorial Guinea",    "Western Africa",    127212,
    64L,               "Faroe Islands",        "OECD Europe",        53551,
    47L,               "Cook Islands",         "Oceania",            48958,
    69L,               "French Guiana",        "South America",      42417,
    160L,              "Niue",                 "Oceania",             5668
  )
}

# Areas joined to a weighted region lookup keep their weight; a caller that
# passes a plain (area_code, region_bouwman) lookup gets weight 1, so fixtures
# and the "none" fallback behave exactly as before.
.with_region_weight <- function(df) {
  if (rlang::has_name(df, "region_weight")) {
    return(df)
  }
  dplyr::mutate(df, region_weight = 1)
}

.empty_bouwman_fcr <- function() {
  tibble::tibble(
    year = integer(),
    region_bouwman = character(),
    item_bouwman = character(),
    feed_type = character(),
    fcr = numeric()
  )
}

.empty_feed_intake <- function(local = FALSE) {
  out <- tibble::tibble(
    year = integer(),
    area_code = integer(),
    live_anim_code = integer(),
    item_cbs_code = integer(),
    feed_type = character(),
    supply = numeric(),
    intake = numeric(),
    intake_dry_matter = numeric(),
    loss = numeric(),
    loss_share = numeric()
  )
  if (local) {
    out <- tibble::add_column(
      out,
      sub_territory = character(),
      .after = "area_code"
    )
  }
  out
}
