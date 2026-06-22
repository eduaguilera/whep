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
