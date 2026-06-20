.build_feed_intake_from_inputs <- function(
  cbs,
  primary_prod,
  years = NULL,
  items_full = whep::items_full,
  items_prod_full = whep::items_prod_full,
  animals_codes = whep::animals_codes,
  biomass_coefs = whep::biomass_coefs,
  conv_bouwman = whep::conv_bouwman,
  conv_krausmann = whep::conv_krausmann,
  polities_cats = whep::polities_cats
) {
  cbs <- .normalise_feed_cbs(cbs)
  primary_prod <- .normalise_feed_primary(primary_prod)
  if (!is.null(years)) {
    years <- as.integer(years)
    cbs <- dplyr::filter(cbs, .data$year %in% years)
    primary_prod <- dplyr::filter(primary_prod, .data$year %in% years)
  }

  model_years <- sort(unique(c(cbs$year, primary_prod$year)))
  feed_avail <- .build_feed_availability(cbs, items_full, biomass_coefs)
  fcr <- .build_bouwman_fcr(conv_bouwman, model_years)
  demand <- .build_feed_demand(
    primary_prod,
    items_prod_full,
    animals_codes,
    conv_krausmann,
    polities_cats,
    fcr
  )

  .allocate_feed_intake(demand, feed_avail, items_full, biomass_coefs)
}

.build_feed_availability <- function(cbs, items_full, biomass_coefs) {
  items <- .feed_items_lookup(items_full)
  biomass <- .feed_biomass_lookup(biomass_coefs)

  cbs |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      feed
    ) |>
    dplyr::filter(!is.na(.data$feed), .data$feed != 0) |>
    dplyr::left_join(items, by = "item_cbs_code") |>
    dplyr::left_join(biomass, by = "Name_biomass") |>
    dplyr::mutate(
      supply = .data$feed,
      supply_dm = .data$supply * .data$product_kgdm_kgfm,
      avail = .data$supply * 0.9,
      avail_dm = .data$avail * .data$product_kgdm_kgfm,
      graniv_feed_share = .data$avail_dm /
        sum(.data$avail_dm, na.rm = TRUE),
      .by = c("year", "area_code", "feedtype_graniv")
    ) |>
    dplyr::mutate(
      dm_feedtype_tot = sum(.data$avail_dm, na.rm = TRUE),
      grazers_feed_share = .data$avail_dm / .data$dm_feedtype_tot,
      ft = dplyr::case_when(
        is.na(.data$feedtype_grazers) ~ NA_character_,
        is.na(.data$feedtype_graniv) ~ "residues_grazers",
        TRUE ~ .data$feedtype_graniv
      ),
      .by = c("year", "area_code", "feedtype_grazers")
    ) |>
    dplyr::select(
      year,
      area_code,
      feedtype_graniv,
      feedtype_grazers,
      item_cbs,
      item_cbs_code,
      product_kgdm_kgfm,
      graniv_feed_share,
      dm_feedtype_tot,
      grazers_feed_share,
      ft,
      supply,
      supply_dm,
      avail,
      avail_dm
    )
}

.normalise_feed_cbs <- function(cbs) {
  cbs <- tibble::as_tibble(cbs) |>
    dplyr::rename_with(tolower)
  if (
    !rlang::has_name(cbs, "item_cbs_code") &&
      rlang::has_name(cbs, "item_code")
  ) {
    cbs <- dplyr::rename(cbs, item_cbs_code = .data$item_code)
  }
  if (
    !rlang::has_name(cbs, "feed") &&
      all(c("element", "value") %in% names(cbs))
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
      item_prod_code = .data$item_code
    )
  }
  if (
    !rlang::has_name(primary_prod, "item_cbs_code") &&
      rlang::has_name(primary_prod, "item_code_cbs")
  ) {
    primary_prod <- dplyr::rename(
      primary_prod,
      item_cbs_code = .data$item_code_cbs
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
    )
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
            dplyr::select(-.data$feed_type),
          by = c("year", "region_bouwman")
        )
    })() |>
    (\(df) {
      df |>
        dplyr::left_join(
          df |>
            dplyr::filter(.data$feed_type == "scavenging") |>
            dplyr::rename(dm_share_scav = .data$dm_share) |>
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
  polities_cats,
  fcr
) {
  regs_codes <- .feed_region_lookup(polities_cats)
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
  animal_type <- .feed_animal_type_lookup(animals_codes)

  dplyr::bind_rows(demand_fcr, demand_head) |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$feed_type)) |>
    dplyr::mutate(
      demand_tot = sum(.data$demand_aft, na.rm = TRUE) / 1000,
      demand_share = .data$demand_aft / (.data$demand_tot * 1000),
      .by = c("year", "area_code", "feed_type")
    ) |>
    dplyr::left_join(animal_type, by = "live_anim_code")
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
    dplyr::left_join(
      fcr,
      by = c("year", "region_bouwman", "item_bouwman"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(demand_aft = .data$value * .data$fcr) |>
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
    dplyr::left_join(
      demand_shares_grazers,
      by = c("year", "region_bouwman")
    ) |>
    dplyr::mutate(
      demand_aft = .data$value *
        .data$cf_kraus *
        .data$dm_share_grazers
    ) |>
    dplyr::select(
      year,
      area_code,
      live_anim,
      live_anim_code,
      feed_type,
      demand_aft
    )
}

.allocate_feed_intake <- function(
  demand,
  feed_avail,
  items_full,
  biomass_coefs
) {
  if (nrow(demand) == 0) {
    return(.empty_feed_intake())
  }

  cropfeed <- .allocate_crop_feed(demand, feed_avail)
  demand_all <- .build_noncrop_demand(cropfeed, demand)
  noncropfeed <- .allocate_noncrop_feed(
    demand_all,
    feed_avail,
    items_full,
    biomass_coefs
  )

  feed_intake_all <- dplyr::bind_rows(
    cropfeed |>
      dplyr::filter(!.data$feed_type %in% c("grass", "scavenging")),
    noncropfeed
  ) |>
    dplyr::filter(.data$intake > 0) |>
    dplyr::left_join(
      feed_avail |>
        dplyr::select(year, area_code, item_cbs, supply, supply_dm),
      by = c("year", "area_code", "item_cbs")
    ) |>
    dplyr::mutate(
      intake_share_anim = .data$intake / sum(.data$intake, na.rm = TRUE),
      avail_dm_anim = .data$avail_dm_anim3,
      supply = dplyr::if_else(
        !is.na(.data$supply),
        .data$supply * .data$intake_share_anim,
        .data$intake
      ),
      supply_dm = dplyr::if_else(
        !is.na(.data$supply_dm),
        .data$supply_dm * .data$intake_share_anim,
        .data$intake_dm
      ),
      loss = .data$supply - .data$intake,
      loss_dm = .data$supply_dm - .data$intake_dm,
      loss_share = (.data$supply - .data$intake) / .data$supply,
      .by = c("year", "area_code", "item_cbs")
    ) |>
    dplyr::filter(!is.na(.data$item_cbs_code))

  feed_intake_all |>
    dplyr::summarise(
      supply = sum(.data$supply, na.rm = TRUE),
      intake = sum(.data$intake, na.rm = TRUE),
      intake_dry_matter = sum(.data$intake_dm, na.rm = TRUE),
      loss = sum(.data$loss, na.rm = TRUE),
      .by = c(
        "year",
        "area_code",
        "live_anim_code",
        "item_cbs_code",
        "feed_type"
      )
    ) |>
    dplyr::mutate(
      loss_share = dplyr::if_else(
        .data$supply == 0,
        0,
        .data$loss / .data$supply
      )
    ) |>
    dplyr::arrange(
      .data$year,
      .data$area_code,
      .data$live_anim_code,
      .data$item_cbs_code,
      .data$feed_type
    )
}

.allocate_crop_feed <- function(demand, feed_avail) {
  cropfeed <- dplyr::bind_rows(
    demand |>
      dplyr::filter(.data$graniv_grazers == "Granivores") |>
      dplyr::left_join(
        feed_avail |>
          dplyr::filter(!is.na(.data$feedtype_graniv)) |>
          dplyr::select(
            year,
            area_code,
            item_cbs,
            item_cbs_code,
            feed_type = feedtype_graniv,
            feed_share_items = graniv_feed_share
          ),
        by = c("year", "area_code", "feed_type")
      ),
    demand |>
      dplyr::filter(.data$graniv_grazers == "Grazers") |>
      dplyr::left_join(
        feed_avail |>
          dplyr::filter(!is.na(.data$feedtype_grazers)) |>
          dplyr::select(
            year,
            area_code,
            item_cbs,
            item_cbs_code,
            feed_type = feedtype_grazers,
            feed_share_items = grazers_feed_share
          ),
        by = c("year", "area_code", "feed_type")
      )
  )

  cropfeed |>
    dplyr::mutate(
      feed_share_items = dplyr::if_else(
        .data$feed_type %in% c("scavenging", "grass"),
        1,
        .data$feed_share_items
      ),
      demand_dm = .data$demand_aft * .data$feed_share_items
    ) |>
    dplyr::mutate(
      demand_dm_tot = sum(.data$demand_dm, na.rm = TRUE),
      demand_share_anim = .data$demand_dm / sum(.data$demand_dm, na.rm = TRUE),
      .by = c("year", "area_code", "item_cbs", "feed_type")
    ) |>
    dplyr::left_join(
      feed_avail |>
        dplyr::mutate(feed_type = .data$feedtype_grazers) |>
        dplyr::select(
          year,
          area_code,
          feed_type,
          ft,
          item_cbs,
          item_cbs_code,
          avail_dm,
          product_kgdm_kgfm
        ),
      by = c("year", "area_code", "item_cbs", "item_cbs_code", "feed_type")
    ) |>
    dplyr::mutate(
      ft = dplyr::if_else(
        .data$feed_type %in% c("scavenging", "grass"),
        .data$feed_type,
        .data$ft
      ),
      item_cbs = dplyr::case_when(
        .data$feed_type == "scavenging" ~ "Scavenging",
        .data$feed_type == "grass" ~ "Grassland",
        TRUE ~ .data$item_cbs
      )
    ) |>
    .allocate_crop_feed_iterations()
}

.allocate_crop_feed_iterations <- function(cropfeed) {
  cropfeed |>
    dplyr::mutate(
      demand_ft = sum(.data$demand_dm, na.rm = TRUE) / 1000,
      avail_dm_anim1 = .data$avail_dm * .data$demand_share_anim,
      avail_dm_ft = sum(.data$avail_dm_anim1, na.rm = TRUE) / 1000,
      demand_graniv = dplyr::if_else(
        .data$graniv_grazers == "Granivores",
        .data$demand_dm,
        0
      ),
      demand_ft_graniv = sum(.data$demand_graniv, na.rm = TRUE) / 1000,
      scaling_graniv = .data$avail_dm_ft / .data$demand_ft_graniv,
      int_dm_graniv = dplyr::if_else(
        .data$demand_ft_graniv < .data$avail_dm_ft,
        .data$demand_graniv,
        .data$demand_graniv * .data$scaling_graniv
      ),
      avail_dm_net = .data$avail_dm_anim1 - .data$int_dm_graniv,
      demand_dm2 = .data$demand_dm - .data$int_dm_graniv,
      .by = c("year", "area_code", "ft")
    ) |>
    dplyr::mutate(
      sum_demand_dm2 = sum(.data$demand_dm2, na.rm = TRUE),
      demand_share_anim2 = dplyr::if_else(
        .data$sum_demand_dm2 == 0,
        .data$demand_share_anim,
        .data$demand_dm2 / .data$sum_demand_dm2
      ),
      avail_dm2 = sum(.data$avail_dm_net, na.rm = TRUE),
      avail_dm_anim2 = .data$avail_dm2 * .data$demand_share_anim2,
      .by = c("year", "area_code", "item_cbs")
    ) |>
    dplyr::mutate(
      demand_ft2 = sum(.data$demand_dm2, na.rm = TRUE) / 1000,
      avail_dm_ft2 = sum(.data$avail_dm_anim2, na.rm = TRUE) / 1000,
      scaling = .data$avail_dm_ft2 / .data$demand_ft2,
      add_intake = dplyr::case_when(
        .data$demand_ft2 < .data$avail_dm_ft2 ~ .data$demand_dm2,
        TRUE ~ .data$demand_dm2 * .data$scaling
      ),
      add_intake = dplyr::if_else(
        is.na(.data$add_intake) | is.nan(.data$add_intake),
        0,
        .data$add_intake
      ),
      int_dm_raw = .data$int_dm_graniv + .data$add_intake,
      .by = c("year", "area_code", "ft")
    ) |>
    dplyr::mutate(
      demand_nc = dplyr::case_when(
        .data$ft == "scavenging" ~ .data$demand_dm,
        .data$ft == "grass" ~ .data$demand_dm * 0.75,
        TRUE ~ .data$demand_dm - .data$int_dm_raw
      ),
      demand_nc_tot = sum(.data$demand_nc, na.rm = TRUE),
      spare_crop = dplyr::case_when(
        .data$ft %in% c("scavenging", "grass") ~ 0,
        .data$avail_dm_anim2 - .data$add_intake < 0 ~ 0,
        .data$item_cbs %in% c("Straw", "Other crop residues") ~
          (.data$avail_dm_anim2 - .data$add_intake) * 0.5,
        TRUE ~ .data$avail_dm_anim2 - .data$add_intake
      ),
      spare_crop = dplyr::if_else(
        is.na(.data$spare_crop) | is.nan(.data$spare_crop),
        0,
        .data$spare_crop
      ),
      spare_crop_tot = sum(.data$spare_crop, na.rm = TRUE),
      spare_demand = .data$demand_nc_tot / .data$spare_crop_tot,
      scaling_intake = dplyr::case_when(
        is.nan(.data$spare_demand) ~ 1,
        .data$spare_demand > 1 ~ 1,
        is.infinite(.data$spare_demand) ~ 1,
        TRUE ~ .data$spare_demand
      ),
      intake_dm = .data$int_dm_raw + (.data$spare_crop * .data$scaling_intake),
      intake = .data$intake_dm / .data$product_kgdm_kgfm,
      avail_dm_anim = .data$int_dm_graniv + .data$avail_dm_anim2,
      loss_dm = .data$avail_dm_anim - .data$intake_dm,
      loss_share = .data$loss_dm / .data$avail_dm_anim,
      .by = c("year", "area_code", "live_anim")
    ) |>
    dplyr::mutate(avail_dm_anim3 = .data$avail_dm_anim) |>
    dplyr::filter(!is.na(.data$item_cbs))
}

.build_noncrop_demand <- function(cropfeed, demand) {
  intake_wide <- cropfeed |>
    dplyr::summarise(
      intake_dm = sum(.data$intake_dm, na.rm = TRUE),
      .by = c(
        "year",
        "area_code",
        "live_anim",
        "live_anim_code",
        "feed_type"
      )
    ) |>
    tidyr::pivot_wider(
      names_from = feed_type,
      values_from = intake_dm,
      values_fill = 0
    )

  loss_wide <- cropfeed |>
    dplyr::mutate(feed_type = paste0("l_", .data$feed_type)) |>
    dplyr::summarise(
      loss_dm = sum(.data$loss_dm, na.rm = TRUE),
      .by = c(
        "year",
        "area_code",
        "live_anim",
        "live_anim_code",
        "feed_type"
      )
    ) |>
    tidyr::pivot_wider(
      names_from = feed_type,
      values_from = loss_dm,
      values_fill = 0
    )

  demand_wide <- demand |>
    dplyr::mutate(feed_type = paste0("d_", .data$feed_type)) |>
    dplyr::select(
      year,
      area_code,
      live_anim,
      live_anim_code,
      feed_type,
      demand_aft
    ) |>
    tidyr::pivot_wider(
      names_from = feed_type,
      values_from = demand_aft,
      values_fill = 0,
      values_fn = sum
    )

  demand_tot <- demand |>
    dplyr::summarise(
      d_tot = sum(.data$demand_aft, na.rm = TRUE),
      .by = c(
        "year",
        "area_code",
        "live_anim",
        "live_anim_code"
      )
    )

  intake_wide |>
    dplyr::full_join(
      loss_wide,
      by = c("year", "area_code", "live_anim", "live_anim_code")
    ) |>
    dplyr::full_join(
      demand_wide,
      by = c("year", "area_code", "live_anim", "live_anim_code")
    ) |>
    dplyr::full_join(
      demand_tot,
      by = c("year", "area_code", "live_anim", "live_anim_code")
    ) |>
    .add_missing_numeric_cols(
      c(
        "crops",
        "residues",
        "animals",
        "scavenging",
        "grass",
        "d_scavenging",
        "d_grass"
      )
    ) |>
    dplyr::mutate(
      intake_share = (.data$crops + .data$residues + .data$animals) /
        .data$d_tot,
      demand_crops_share = 1 -
        ((.data$d_scavenging + .data$d_grass) / .data$d_tot),
      scaling_raw = (.data$d_tot -
        .data$crops -
        .data$residues -
        .data$animals) /
        (.data$d_scavenging + .data$d_grass),
      scaling = dplyr::case_when(
        is.infinite(.data$scaling_raw) ~ 0,
        .data$scaling_raw < 0 ~ 0,
        TRUE ~ .data$scaling_raw
      ),
      scavenging = .data$d_scavenging * .data$scaling,
      grass = .data$d_grass * .data$scaling,
      intake = .data$crops +
        .data$residues +
        .data$animals +
        .data$scavenging +
        .data$grass,
      mismatch = .data$intake / .data$d_tot
    )
}

.allocate_noncrop_feed <- function(
  demand_all,
  feed_avail,
  items_full,
  biomass_coefs
) {
  items <- .feed_items_lookup(items_full)
  biomass <- .feed_biomass_lookup(biomass_coefs)

  demand_all |>
    dplyr::select(
      year,
      area_code,
      live_anim,
      live_anim_code,
      scavenging,
      grass
    ) |>
    tidyr::pivot_longer(
      c(scavenging, grass),
      names_to = "feed_type",
      values_to = "value"
    ) |>
    dplyr::mutate(
      live_anim_share = .data$value / sum(.data$value, na.rm = TRUE),
      .by = c("year", "area_code", "feed_type")
    ) |>
    dplyr::left_join(
      items |>
        dplyr::select(
          feed_type = feedtype_grazers,
          item_cbs,
          item_cbs_code,
          Name_biomass
        ),
      by = "feed_type",
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      feed_avail |>
        dplyr::filter(.data$item_cbs == "Temporary grassland") |>
        dplyr::select(year, area_code, item_cbs, avail_dm),
      by = c("year", "area_code", "item_cbs")
    ) |>
    dplyr::mutate(
      avail_dm = dplyr::if_else(
        .data$item_cbs == "Temporary grassland",
        tidyr::replace_na(.data$avail_dm, 0),
        .data$avail_dm
      ),
      dm_anim = .data$avail_dm * .data$live_anim_share,
      intake = dplyr::case_when(
        .data$dm_anim < .data$value ~ .data$dm_anim,
        TRUE ~ .data$value
      ),
      intake = dplyr::if_else(.data$item_cbs == "Fallow", 0, .data$intake)
    ) |>
    dplyr::mutate(
      remain_intake_ft = .data$value - sum(.data$intake, na.rm = TRUE),
      intake = dplyr::case_when(
        !is.na(.data$intake) ~ .data$intake,
        .data$remain_intake_ft == 0 ~ .data$value,
        TRUE ~ .data$remain_intake_ft
      ),
      .by = c("year", "area_code", "feed_type", "live_anim")
    ) |>
    dplyr::left_join(biomass, by = "Name_biomass") |>
    dplyr::mutate(
      intake_dm = .data$intake,
      intake = .data$intake_dm / .data$product_kgdm_kgfm,
      avail_dm_anim3 = .data$intake_dm,
      loss_dm = .data$avail_dm_anim3 - .data$intake_dm,
      ft = .data$feed_type
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

.feed_region_lookup <- function(polities_cats) {
  tibble::as_tibble(polities_cats) |>
    dplyr::transmute(
      area_code = as.integer(.data$code),
      region_bouwman = .data$region
    ) |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$region_bouwman)) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
}

.feed_animal_type_lookup <- function(animals_codes) {
  tibble::as_tibble(animals_codes) |>
    dplyr::transmute(
      live_anim_code = as.integer(.data$item_cbs_code),
      graniv_grazers = .data$Graniv_grazers
    ) |>
    dplyr::distinct(.data$live_anim_code, .keep_all = TRUE)
}

.add_missing_numeric_cols <- function(data, cols) {
  for (col in cols) {
    if (!rlang::has_name(data, col)) {
      data[[col]] <- 0
    }
  }
  data
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

.empty_feed_intake <- function(provincial = FALSE) {
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
  if (provincial) {
    out <- tibble::add_column(
      out,
      sub_territory = character(),
      .after = "area_code"
    )
  }
  out
}
