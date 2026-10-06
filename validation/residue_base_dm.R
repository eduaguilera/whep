# Real-data check of the gross crop-residue base, in dry matter (#1330).
#
# The `crop_residues` pin is fresh matter (#1215, PR #1255). Issue #1132
# compared it, unconverted, with dry-matter literature and found the base
# "~36% too high": 7.209 Pg of Straw + Other crop residues at 2020 against
# ~5.3 Pg, the ~3.90 Pg of cereal residue Smerald et al. give for 2020
# divided by their 73% cereal share. That comparison mixed fresh and dry
# weight. This script makes it in dry matter, cereals against cereals, which
# is the only part of the base with a published, checkable global total.
#
# Ground truth is `gt_residue_base_dm.json`: Smerald, Rahimi & Scheer (2023),
# *Sci. Data* 10:685, doi:10.1038/s41597-023-02587-0, transcribed from the
# authors' own code and outputs (Zenodo doi:10.5281/zenodo.7843730). Their
# residue is grain x residue:product ratio x dry-matter content, by three
# methods (a constant ratio, and two yield-dependent ones); the spread of the
# three is the band WHEP is judged against.
#
# What is checked:
#   1. World cereal residue dry matter lies inside the three-method band,
#      widened by `tolerance`, in every year 1997-2021. This is the gate.
#   2. The pin is still fresh matter (straw fresh/dry > 1), so a later change
#      to the unit basis is noticed rather than silently double-converted.
# What is only reported:
#   - per-crop 2010 and 2020 against the band (the crops offset each other);
#   - the country-year totals Smerald validate against;
#   - #1132's all-crop yardstick, whose 73% cereal share (Shinde et al. 2022,
#     Ind. Crops Prod. 181:114772) could not be opened: assumed, unverified.
#
# Not part of the test suite: it reads the `crop_residues` pin.
#
# Run:  Rscript --no-init-file validation/residue_base_dm.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))

# Outside the band by more than this, relative to the nearest edge, fails.
# 5% is a fraction of the band's own width (RPR to Fischer spans 3-28%).
tolerance <- 0.05
gt <- jsonlite::read_json(
  "validation/gt_residue_base_dm.json",
  simplifyVector = TRUE
)

cli::cli_h1("Gross crop-residue base, dry matter (#1330)")

cereal_codes <- whep::items_full |>
  dplyr::filter(.data$comm_group == "Cereals") |>
  dplyr::pull("item_cbs_code")
residues <- get_primary_residues()
cereals <- dplyr::filter(residues, .data$item_cbs_code_crop %in% cereal_codes)

# 2. Unit basis -------------------------------------------------------------
straw <- dplyr::filter(residues, .data$item_cbs_code_residue == 2105)
fresh_over_dry <- sum(straw$value) / sum(straw$value_dm)
cli::cli_inform(
  "Straw fresh/dry mass ratio over all years: {round(fresh_over_dry, 3)}."
)
if (!(fresh_over_dry > 1)) {
  cli::cli_abort(c(
    "The residue pin no longer reads as fresh matter.",
    "i" = "{.fn get_primary_residues} documents {.field value} as fresh and
           {.field value_dm} as dry (#1215); a ratio of 1 means one of them
           changed basis."
  ))
}

# 1. World cereal residue against the band ----------------------------------
world <- cereals |>
  dplyr::summarise(
    whep_fresh_tg = sum(.data$value) / 1e6,
    whep_dm_tg = sum(.data$value_dm) / 1e6,
    .by = "year"
  ) |>
  dplyr::inner_join(tibble::as_tibble(gt$world_tg_dm), by = "year") |>
  dplyr::mutate(
    band_low = pmin(.data$RPR, .data$bentsen, .data$fischer),
    band_high = pmax(.data$RPR, .data$bentsen, .data$fischer),
    band_mean = (.data$RPR + .data$bentsen + .data$fischer) / 3,
    ratio_to_mean = .data$whep_dm_tg / .data$band_mean,
    outside = .data$whep_dm_tg < .data$band_low * (1 - tolerance) |
      .data$whep_dm_tg > .data$band_high * (1 + tolerance)
  ) |>
  dplyr::arrange(.data$year)

print(
  dplyr::select(
    world,
    "year",
    "whep_fresh_tg",
    "whep_dm_tg",
    "band_low",
    "band_high",
    "ratio_to_mean"
  ),
  n = Inf
)

mean_dm <- mean(world$whep_dm_tg)
cli::cli_inform(c(
  "*" = "1997-2021 mean: WHEP {round(mean_dm)} Tg DM against Smerald
         {gt$world_mean_1997_2021_tg_dm} Tg DM
         ({round(100 * (mean_dm / gt$world_mean_1997_2021_tg_dm - 1), 1)}%).",
  "*" = "Same years in fresh matter: {round(mean(world$whep_fresh_tg))} Tg."
))

# Per crop, reported only ----------------------------------------------------
crop_map <- tibble::tribble(
  ~item_cbs_code_crop , ~crop           ,
                 2511 , "Wheat"         ,
                 2807 , "Rice"          ,
                 2514 , "Maize"         ,
                 2513 , "Barley"        ,
                 2517 , "Millet"        ,
                 2518 , "Sorghum"       ,
                 2515 , "Other cereals" ,
                 2516 , "Other cereals" ,
                 2520 , "Other cereals"
)
per_crop <- cereals |>
  dplyr::filter(.data$year %in% c(2010, 2020)) |>
  dplyr::inner_join(crop_map, by = "item_cbs_code_crop") |>
  dplyr::summarise(
    whep_dm_tg = sum(.data$value_dm) / 1e6,
    .by = c("year", "crop")
  ) |>
  dplyr::inner_join(
    tibble::as_tibble(gt$per_crop_tg_dm),
    by = c("year", "crop")
  ) |>
  dplyr::mutate(
    ratio_to_mean = .data$whep_dm_tg /
      ((.data$RPR + .data$bentsen + .data$fischer) / 3)
  ) |>
  dplyr::arrange(.data$year, dplyr::desc(.data$whep_dm_tg))
print(per_crop)

# Country-years, reported only -----------------------------------------------
countries <- cereals |>
  dplyr::mutate(
    iso3 = stringr::str_sub(.data$reporting_polity_code, 1, 3)
  ) |>
  dplyr::summarise(
    whep_dm_tg = sum(.data$value_dm) / 1e6,
    .by = c("iso3", "year")
  ) |>
  dplyr::inner_join(
    tibble::as_tibble(gt$countries),
    by = c("iso3", "year")
  ) |>
  dplyr::mutate(
    ratio_to_smerald = .data$whep_dm_tg / .data$smerald_tg_dm,
    ratio_to_literature = .data$whep_dm_tg / .data$literature_tg_dm
  )
print(
  dplyr::select(
    countries,
    "iso3",
    "year",
    "author",
    "whep_dm_tg",
    "smerald_tg_dm",
    "literature_tg_dm",
    "ratio_to_smerald",
    "ratio_to_literature"
  ),
  n = Inf,
  width = Inf
)

# #1132's all-crop yardstick, reported only ---------------------------------
all_2020 <- dplyr::filter(residues, .data$year == 2020)
feedable_2020 <- dplyr::filter(
  all_2020,
  .data$item_cbs_code_residue %in% c(2105, 2106)
)
yardstick <- gt$world_tg_dm$fischer[gt$world_tg_dm$year == 2020] / 0.73
cli::cli_inform(c(
  "*" = "2020 Straw + Other crop residues: {round(sum(feedable_2020$value) /
         1e6)} Tg fresh, {round(sum(feedable_2020$value_dm) / 1e6)} Tg DM.",
  "*" = "2020 all residue incl. Firewood: {round(sum(all_2020$value_dm) /
         1e6)} Tg DM.",
  "*" = "Smerald 2020 cereals / 0.73 (share assumed, unverified):
         {round(yardstick)} Tg DM at the top of their band."
))

if (any(world$outside)) {
  bad <- world$year[world$outside]
  cli::cli_abort(c(
    "World cereal residue leaves the published band in
     {cli::qty(length(bad))}year{?s} {bad}.",
    "i" = "Band: Smerald et al. (2023), three methods, widened by
           {tolerance * 100}%."
  ))
}

cli::cli_alert_success(
  "World cereal residue is inside the published dry-matter band in every
   year {min(world$year)}-{max(world$year)}."
)
invisible(list(world = world, per_crop = per_crop, countries = countries))
