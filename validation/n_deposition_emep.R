# HaNi atmospheric N deposition vs the EMEP MSC-W model (external archetype).
#
# ## What this measures
#
# WHEP's deposition input is HaNi (Tian et al. 2022), read by
# `read_n_deposition()` and turned into a rate by `build_n_deposition()`. HaNi
# is a global reconstruction; over Europe the EMEP MSC-W chemical transport
# model is the stronger constraint, because it is driven by the emissions
# European countries report under the Gothenburg Protocol and evaluated against
# the EMEP measurement network. This script puts the two on WHEP's own
# 0.5-degree grid and reports the ratio per country and per year, which is the
# shape any correction between them would have to be parameterised on (#1097).
#
# ## The finding, measured 2026-09-14 with EMEP rv5.6 rep2025
#
# HaNi is not uniformly low over Europe. It is too FLAT: it largely misses both
# the European deposition peak around 1990 and the fall that emission controls
# drove afterwards. Over the 3193 0.5-degree cells assigned to an EMEP core
# country, area-weighted kg N/ha/yr:
#
#   1990   HaNi 9.81   EMEP 14.08   ratio 0.696   gap 2.49 Tg N/yr
#   2000   HaNi 9.65   EMEP 11.28   ratio 0.856   gap 0.94 Tg N/yr
#   2010   HaNi 9.04   EMEP  9.56   ratio 0.946   gap 0.30 Tg N/yr
#   2019   HaNi 8.00   EMEP  8.07   ratio 0.992   gap 0.04 Tg N/yr
#
# Across 1990-2019 HaNi falls 18.4% while EMEP falls 42.7%. Summed over those
# cells and years HaNi is 22.6 Tg N of deposition short, all of it accumulated
# before 2010. The ratio is not monotone -- 2011-2013 sit just above 1 -- so
# the two products genuinely cross rather than converging from one side.
#
# At 1990 the ratio is below 1 in every core country with meaningful coverage.
# Worst are the Netherlands 0.313 (17 cells), Italy 0.386 (130), Denmark 0.429
# (28), Ireland 0.570 (37), Belgium 0.581 (17), Germany 0.585 (189), Croatia
# 0.588 (27), Portugal 0.589 (40) and Poland 0.603 (166). The extremes at two
# and ten cells (Cyprus, Slovenia) are noise in a 0.5-degree comparison and
# should not be read as country estimates; Italy, Germany and Poland carry the
# mass.
#
# So the error is a FUNCTION OF TIME, largest exactly where and when European
# deposition was largest, and it compounds backwards into the pre-1990 period
# where EMEP offers no check at all.
#
# ## Two limits that are part of the finding, not caveats to it
#
# 1. EMEP's domain (lon -30..90, lat 30..82) is much wider than the region EMEP
#    is built to represent. Matching is done per country and the ratio outside
#    Europe is reported but must not be read as a bias estimate: China comes
#    out at 4.75 and Saudi Arabia at 2.78 in 1990, which says the two models
#    disagree at the EMEP domain edge, not that HaNi is four times too high
#    there. `emep_core_iso3` below is the judged subset.
# 2. Neither product is a measurement. EMEP is the stronger constraint FOR
#    EUROPE OVER THIS PERIOD; that is a judgement, and correcting HaNi toward
#    EMEP is a science decision for the maintainer, not something this script
#    or any green CI settles.
#
# ## Method
#
# - HaNi through WHEP's own `read_n_deposition()`, which SUMS the 6x6 native
#   5-arcmin cells into each 0.5-degree block because the source quantity is an
#   extensive mass, divided by the true latitude-dependent cell area to give a
#   density. Averaging instead, or dividing by one fixed area constant, would
#   mis-weight by cos(lat) and would itself look like a latitude-dependent
#   bias -- which is why the comparison goes through the package reader rather
#   than a second implementation of it.
# - EMEP rv5.6 yearly at 0.1 degrees, `DDEP_RDN_m2Grid + DDEP_OXN_m2Grid +
#   WDEP_RDN + WDEP_OXN`, mgN/m2 -> kgN/ha (x 0.01), aggregated to 0.5 degrees
#   by plain mean: a density aggregates by mean, the 0.1-degree grid nests
#   exactly 5x5 inside the 0.5-degree one, and cos(lat) varies by under 0.2 per
#   mille inside a block.
# - Cells with zero in either product dropped, which is also what restricts the
#   comparison to land (HaNi is zero outside its land mask).
# - Cells assigned to a country by `build_cell_polity()`, winner-take-all on
#   `polity_frac`, so a border cell is counted once. That grid, not a bounding
#   box, is what makes the per-country series meaningful; `n_cells` in the
#   output is how many of them the comparison actually matched.
#
# ## Data
#
# EMEP MSC-W model results, EMEP01 rv5.6, 2025 reporting round, from the
# Norwegian Meteorological Institute THREDDS server. Downloaded on demand into
# `validation/cache/emep/` (gitignored, ~78 MB per year). See
# <https://www.emep.int/mscw/mscw_moddata.html>.
#
#   Simpson, D., Benedictow, A., Berge, H., Bergstrom, R., Emberson, L. D.,
#   Fagerli, H., Flechard, C. R., Hayman, G. D., Gauss, M., Jonson, J. E.,
#   Jenkin, M. E., Nyiri, A., Richter, C., Semeena, V. S., Tsyro, S.,
#   Tuovinen, J.-P., Valdebenito, A. and Wind, P. (2012). The EMEP MSC-W
#   chemical transport model - technical description. Atmospheric Chemistry
#   and Physics 12(16), 7825-7865. doi:10.5194/acp-12-7825-2012
#   (verified against the Crossref record, 2026-09-14)
#
# HaNi needs `WHEP_HANI_DIR`; without it the script says so and stops.
#
# ## Usage
#
#   Rscript validation/n_deposition_emep.R [year_min] [year_max]
#
# Defaults to 1990-2019, the overlap of EMEP rv5.6 (1990-) and HaNi (-2019).
# The HaNi aggregate is cached, because reading it is the slow half.

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
year_min <- as.integer(if (length(args) >= 1) args[[1]] else "1990")
year_max <- as.integer(if (length(args) >= 2) args[[2]] else "2019")
years <- seq(year_min, year_max)

emep_dir <- "validation/cache/emep"
emep_base <- paste0(
  "https://thredds.met.no/thredds/fileServer/data/EMEP/2025_Reporting"
)
# The four components of total reactive nitrogen deposition: dry and wet,
# reduced (NHx) and oxidised (NOy). Summing fewer of them is the obvious way to
# manufacture a spurious low bias in EMEP, so they are named once here.
emep_vars <- c(
  "DDEP_RDN_m2Grid",
  "DDEP_OXN_m2Grid",
  "WDEP_RDN",
  "WDEP_OXN"
)

# The countries EMEP is built to represent. The model domain reaches Xinjiang
# and the Arabian peninsula, where the two products disagree by factors of 3-5
# in BOTH directions; reporting that as a HaNi bias would be reading the domain
# edge as a measurement. Judged rows are these; the rest are still written to
# the per-country CSV, flagged.
emep_core_iso3 <- c(
  "ALB",
  "AUT",
  "BEL",
  "BGR",
  "BIH",
  "BLR",
  "CHE",
  "CYP",
  "CZE",
  "DEU",
  "DNK",
  "ESP",
  "EST",
  "FIN",
  "FRA",
  "GBR",
  "GRC",
  "HRV",
  "HUN",
  "IRL",
  "ISL",
  "ITA",
  "LTU",
  "LUX",
  "LVA",
  "MDA",
  "MKD",
  "MLT",
  "MNE",
  "NLD",
  "NOR",
  "POL",
  "PRT",
  "ROU",
  "SRB",
  "SVK",
  "SVN",
  "SWE",
  "UKR"
)

.vd_emep_url <- function(year) {
  # 2023 onwards is a different naming round; this script's window predates it.
  sprintf(
    "%s/EMEP01_rv5.6_year.%dmet_%demis_rep2025.nc",
    emep_base,
    year,
    year
  )
}

.vd_emep_fetch <- function(year) {
  dir.create(emep_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(emep_dir, sprintf("emep_%d.nc", year))
  if (file.exists(path) && file.size(path) > 1e7) {
    return(path)
  }
  cli::cli_alert("Downloading EMEP {year} (~78 MB)...")
  utils::download.file(.vd_emep_url(year), path, mode = "wb", quiet = TRUE)
  path
}

# 0.1-degree centres sit at x.x5, so flooring onto the 0.5-degree lattice is
# exact and each 0.5-degree block collects exactly 25 of them.
.vd_block_centre <- function(coord) {
  floor(coord / 0.5) * 0.5 + 0.25
}

.vd_emep_year <- function(year) {
  nc <- ncdf4::nc_open(.vd_emep_fetch(year))
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  total <- Reduce(`+`, lapply(emep_vars, function(v) ncdf4::ncvar_get(nc, v)))
  # Precomputed, because inside `tibble()` a later expression sees the `lon`
  # column just created, not the coordinate vector, and `length(lon)` would
  # then be the cell count rather than the axis length.
  lon_block <- rep(.vd_block_centre(lon), times = length(lat))
  lat_block <- rep(.vd_block_centre(lat), each = length(lon))
  tibble::tibble(
    lon = lon_block,
    lat = lat_block,
    emep_kgn_ha = as.vector(total) * 0.01
  ) |>
    dplyr::filter(!is.na(.data$emep_kgn_ha)) |>
    dplyr::summarise(
      emep_kgn_ha = mean(.data$emep_kgn_ha),
      .by = c("lon", "lat")
    ) |>
    dplyr::mutate(year = year)
}

.vd_hani <- function(years) {
  cache <- sprintf(
    "validation/cache/hani_deposition_%d_%d.rds",
    min(years),
    max(years)
  )
  if (file.exists(cache)) {
    return(readRDS(cache))
  }
  if (!nzchar(Sys.getenv("WHEP_HANI_DIR"))) {
    cli::cli_abort(c(
      "No HaNi deposition data and no cached aggregate.",
      i = "Set {.envvar WHEP_HANI_DIR}; see
           {.file inst/scripts/download/download_nitrogen.R}."
    ))
  }
  cli::cli_alert("Reading HaNi for {length(years)} year{?s} (slow)...")
  nhx <- read_n_deposition("nhx", years = years)
  noy <- read_n_deposition("noy", years = years)
  out <- dplyr::full_join(
    nhx,
    noy,
    by = c("lon", "lat", "year"),
    suffix = c("_nhx", "_noy")
  ) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      year = .data$year,
      hani_g = dplyr::coalesce(.data$value_g_nhx, 0) +
        dplyr::coalesce(.data$value_g_noy, 0)
    )
  dir.create("validation/cache", recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, cache)
  out
}

# Winner-take-all on `polity_frac`, so a border cell is counted once and the
# per-country means are not double-weighted. The cell area comes from
# `build_cell_polity()`, i.e. the same latitude-dependent area the deposition
# rate was formed with.
.vd_cells <- function() {
  build_cell_polity() |>
    dplyr::slice_max(.data$polity_frac, n = 1, by = c("lon", "lat")) |>
    dplyr::distinct(.data$lon, .data$lat, .keep_all = TRUE) |>
    dplyr::select("lon", "lat", "area_code", "cell_area_ha")
}

.vd_weighted <- function(x, by) {
  dplyr::summarise(
    x,
    n_cells = dplyr::n(),
    hani_kgn_ha = sum(.data$hani_kgn_ha * .data$cell_area_ha) /
      sum(.data$cell_area_ha),
    emep_kgn_ha = sum(.data$emep_kgn_ha * .data$cell_area_ha) /
      sum(.data$cell_area_ha),
    hani_tg = sum(.data$hani_kgn_ha * .data$cell_area_ha) / 1e9,
    emep_tg = sum(.data$emep_kgn_ha * .data$cell_area_ha) / 1e9,
    .by = dplyr::all_of(by)
  ) |>
    dplyr::mutate(
      ratio = .data$hani_kgn_ha / .data$emep_kgn_ha,
      gap_tg = .data$emep_tg - .data$hani_tg
    )
}

emep <- purrr::map_dfr(years, .vd_emep_year)
cells <- .vd_cells()
matched <- .vd_hani(years) |>
  dplyr::inner_join(cells, by = c("lon", "lat")) |>
  dplyr::mutate(hani_kgn_ha = .data$hani_g / 1000 / .data$cell_area_ha) |>
  dplyr::inner_join(emep, by = c("lon", "lat", "year")) |>
  # A zero in either product is an out-of-domain cell, not a measurement of no
  # deposition: HaNi is zero outside its land mask and EMEP outside its grid.
  dplyr::filter(.data$hani_kgn_ha > 0, .data$emep_kgn_ha > 0) |>
  add_area_name()

by_country <- matched |>
  .vd_weighted(c("area_code", "area_name", "year")) |>
  dplyr::left_join(
    dplyr::distinct(whep::regions_full, area_code = .data$code, .data$iso3c),
    by = "area_code"
  ) |>
  dplyr::mutate(emep_core = .data$iso3c %in% emep_core_iso3) |>
  dplyr::arrange(.data$area_name, .data$year)

core <- dplyr::semi_join(
  matched,
  dplyr::filter(by_country, .data$emep_core),
  by = c("area_code", "year")
) |>
  .vd_weighted("year") |>
  dplyr::arrange(.data$year)

out_csv <- "validation/cache/hani_emep_by_country.csv"
utils::write.csv(by_country, out_csv, row.names = FALSE)

cat("\n=== HaNi vs EMEP MSC-W rv5.6, EMEP core countries ===\n")
print(
  dplyr::select(
    core,
    "year",
    "n_cells",
    "hani_kgn_ha",
    "emep_kgn_ha",
    "ratio",
    "gap_tg"
  ),
  n = Inf
)

cat("\n--- worst-biased core countries at", year_min, "---\n")
by_country |>
  dplyr::filter(.data$emep_core, .data$year == year_min, .data$n_cells >= 20) |>
  dplyr::arrange(.data$ratio) |>
  dplyr::select(
    "area_name",
    "n_cells",
    "hani_kgn_ha",
    "emep_kgn_ha",
    "ratio"
  ) |>
  head(10) |>
  print()

first <- core[core$year == year_min, ]
last <- core[core$year == year_max, ]
cat("\nPer-country ratios written to", out_csv, "\n")
cat(paste0(
  sprintf("METRIC n_cells=%d ", first$n_cells),
  sprintf(
    "n_countries=%d ",
    dplyr::n_distinct(by_country$area_code[by_country$emep_core])
  ),
  sprintf("ratio_first=%.3f ratio_last=%.3f ", first$ratio, last$ratio),
  sprintf(
    "hani_change_pct=%.1f ",
    100 * (last$hani_kgn_ha / first$hani_kgn_ha - 1)
  ),
  sprintf(
    "emep_change_pct=%.1f ",
    100 * (last$emep_kgn_ha / first$emep_kgn_ha - 1)
  ),
  sprintf("cum_gap_tg=%.1f\n", sum(core$gap_tg))
))
