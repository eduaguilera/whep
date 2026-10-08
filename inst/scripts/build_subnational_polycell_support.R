# -----------------------------------------------------------------------
# build_subnational_polycell_support.R
#
# Build the `polycell_support_subnational` pin: the province rows a granted
# depth reads beside the world `polycell_support` pin (see
# `.level_default_support()` in R/spatialize_levels.R).
#
# Why a separate pin. The world pin must stay a partition of NATIONAL
# polities: a container beside its own members is refused by
# `.level_check_no_double_claim()`, and every level-0 consumer would count the
# same ground twice. So the provinces live here, and a depth read swaps each
# container out of the world pin and its members in.
#
# Why the neighbours are built too. `build_polycell_support()` apportions a
# cell's inland water over the partition's territory in that cell. Built on
# the provinces alone, a border cell's whole water would land on the province.
# So the build takes the provinces plus every live national polity touching
# their containers (with the containers themselves left out), and only the
# province rows are kept.
#
# `claims = "keep"`: the world build cuts out of a polity with no reporting
# `area_code` the ground a keyed polity also claims (whep#1310). Provinces have
# no reporting code by design and are the data carriers here, so that rule
# would hand their border cells to the neighbours.
#
# Published 2026-09-30 as 20260930T115111Z-15e25: 335 provinces, 16,131 rows,
# 88 neighbour polities, 15.5 min.
#
# Run (from the WHEP repository root):
#   Rscript inst/scripts/build_subnational_polycell_support.R
#
# Environment variables (never hardcode the path):
#   WHEP_LPJML_INPUT_DIR   parent of GLWD/ (download_hydrology.R). REQUIRED.
#   WHEP_NATURALEARTH_DIR  ne_10m_glaciated_areas/ (download_naturalearth.R).
#                          REQUIRED.
#   WHEP_SUBNATIONAL_OUT   output parquet path. REQUIRED.
#   WHEP_SUBNATIONAL_ISO3  optional comma-separated ISO3 list; defaults to the
#                          ten countries with admin statistics below.
#
# Upload with `upload_files(<parquet>, "polycell_support_subnational")` from
# ~/whep_inputs, then freeze the version in inst/extdata/whep_inputs.csv and
# run data-raw/whep_inputs.R.
# -----------------------------------------------------------------------

.sps_env <- function(name) {
  value <- Sys.getenv(name, "")
  if (!nzchar(value)) {
    cli::cli_abort(
      "{.envvar {name}} is unset. A support built without its water or ice
       layer books lakes and glaciers as land (whep#885)."
    )
  }
  value
}

.sps_iso3 <- function() {
  given <- Sys.getenv("WHEP_SUBNATIONAL_ISO3", "")
  if (nzchar(given)) {
    return(stringr::str_trim(stringr::str_split_1(given, ",")))
  }
  c("ARG", "AUS", "BOL", "BRA", "CHL", "COL", "ESP", "FRA", "JPN", "MEX")
}

# Taken from the CURRENT snapshot, never from an earlier build's output: a
# polity whose code changed between snapshots (BRA-DF-1960-2023 became
# BRA-DF-1960-2025) would otherwise drop out without a word.
# RYU-1937-1945 (Okinawa, 1937-1945) is left out: no admin source has a unit
# for it, and it splits every Japanese interval it touches.
.sps_provinces <- function(attrs, live, iso3) {
  keep <- live &
    attrs$iso3_code %in% iso3 &
    attrs$polity_type %in% "subnational"
  sort(setdiff(attrs$polity_code[keep], "RYU-1937-1945"))
}

.sps_neighbours <- function(polities, attrs, live, excluded, containers) {
  old <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old), add = TRUE)
  geoms <- sf::st_make_valid(sf::st_geometry(polities))
  footprint <- sf::st_union(geoms[attrs$polity_code %in% containers])
  candidate <- live &
    !(attrs$polity_code %in% excluded) &
    attrs$polity_type %in% "national"
  touches <- lengths(sf::st_intersects(geoms[candidate], footprint)) > 0
  attrs$polity_code[candidate][touches]
}

.sps_main <- function() {
  input_dir <- .sps_env("WHEP_LPJML_INPUT_DIR")
  ne_dir <- .sps_env("WHEP_NATURALEARTH_DIR")
  out <- .sps_env("WHEP_SUBNATIONAL_OUT")
  polities <- whep::polities
  attrs <- sf::st_drop_geometry(polities)
  live <- whep:::.polity_is_live(attrs$wiki_status) &
    !(attrs$polity_type %in% "aggregate") &
    attrs$has_geometry
  provinces <- .sps_provinces(attrs, live, .sps_iso3())
  edges <- whep::polity_containment
  containers <- unique(edges$container_code[edges$member_code %in% provinces])
  neighbours <- .sps_neighbours(
    polities,
    attrs,
    live,
    c(containers, provinces),
    containers
  )
  cli::cli_alert_info(
    "{length(provinces)} provinces, {length(containers)} containers swapped
     out, {length(neighbours)} neighbour polities."
  )
  support <- whep::build_polycell_support(
    geometries = polities[attrs$polity_code %in% c(provinces, neighbours), ],
    water = whep::read_glwd_water(input_dir),
    ice = whep::read_glaciated_areas(ne_dir),
    subnational = "include",
    claims = "keep"
  )
  kept <- sf::st_drop_geometry(support)
  kept <- tibble::as_tibble(kept[kept$polity_code %in% provinces, ])
  missing <- setdiff(provinces, kept$polity_code)
  if (length(missing) > 0L) {
    cli::cli_abort("No support rows for {.val {missing}}.")
  }
  nanoparquet::write_parquet(kept, out)
  cli::cli_alert_success(
    "{nrow(kept)} rows, {dplyr::n_distinct(kept$polity_code)} provinces,
     written to {.file {out}}."
  )
}

if (sys.nframe() == 0L) {
  .sps_main()
}
