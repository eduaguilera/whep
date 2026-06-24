# One-shot multi-variable scorecard. Runs every variable's deterministic check
# against its pinned ground truth + the cached WHEP production, and prints a
# combined per-variable summary. This is the "sweep all variables" entry point;
# growing ground truth (subnational research, new GT) is done separately by the
# workflows.
#
# Usage: Rscript validation/validate_all.R   (respects VAL_YEAR_MIN/MAX)

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
})

source("validation/validate.R")
source("validation/variables.R")

year_min <- as.integer(Sys.getenv("VAL_YEAR_MIN", "1970"))
year_max <- as.integer(Sys.getenv("VAL_YEAR_MAX", "2010"))
bench_years <- c(1990L, 2000L, 2010L)
production <- readRDS(
  sprintf(".whep_cache/primary_prod_%d_%d.rds", year_min, year_max)
)
lookups <- whep_validation_lookups()
scorecard <- list()
add <- function(variable, archetype, n, ok, flag, note) {
  scorecard[[length(scorecard) + 1L]] <<- tibble::tibble(
    variable = variable,
    archetype = archetype,
    n_checked = n,
    ok = ok,
    flag = flag,
    note = note
  )
}

# 1. stability (internal) ------------------------------------------------------
stab <- system2(
  "Rscript",
  "validation/stability.R",
  stdout = TRUE,
  stderr = FALSE
)
n_disc <- as.integer(sub(
  ".*n_discontinuities=([0-9]+).*",
  "\\1",
  grep("^METRIC", stab, value = TRUE)
))
add(
  "stability",
  "internal",
  NA,
  NA,
  n_disc,
  "year-over-year discontinuities (candidates)"
)

# 2. production (external, vs pinned subnational findings) ---------------------
fin_files <- list.files(
  "validation/cache/findings",
  pattern = "\\.json$",
  full.names = TRUE
)
if (length(fin_files) > 0) {
  fin <- purrr::map_dfr(fin_files, function(f) {
    x <- jsonlite::fromJSON(f)
    if (length(x) == 0) NULL else tibble::as_tibble(x)
  })
  probes <- fin |>
    dplyr::transmute(
      probe_id = sprintf(
        "sub-%s-%s-%d",
        .data$country_iso3,
        tolower(.data$crop),
        as.integer(.data$year)
      ),
      pool = "subnational",
      layer = "production",
      area_iso3 = .data$country_iso3,
      item_name = .data$crop,
      year = as.integer(.data$year),
      element = NA_character_,
      unit = "tonnes"
    )
  corpus <- fin |>
    dplyr::transmute(
      probe_id = probes$probe_id,
      gt_value = as.numeric(.data$value),
      gt_unit = .data$unit,
      source = .data$source,
      url = .data$url,
      definition = .data$basis,
      tolerance_pct = 10,
      confidence = "medium"
    )
  v <- run_validation(probes, list(production = production), corpus, lookups)
  add(
    "production",
    "external",
    nrow(v),
    sum(v$verdict == "pass"),
    sum(v$verdict %in% c("flag_high", "flag_low")),
    "subnational-summed national totals (NASS/IBGE/BPS/SEDAC)"
  )
}

# 2b. production vs USDA FAS PSD (external, independent of FAOSTAT) ------------
psd_path <- "validation/cache/ground_truth/production_psd.json"
if (file.exists(psd_path)) {
  psd <- jsonlite::fromJSON(psd_path) |> tibble::as_tibble()
  p2 <- psd |>
    dplyr::transmute(
      probe_id = sprintf(
        "psd-%s-%s-%d",
        .data$area_iso3,
        tolower(.data$crop),
        as.integer(.data$year)
      ),
      pool = "psd",
      layer = "production",
      area_iso3 = .data$area_iso3,
      item_name = .data$crop,
      year = as.integer(.data$year),
      element = NA_character_,
      unit = "tonnes"
    )
  c2 <- psd |>
    dplyr::transmute(
      probe_id = p2$probe_id,
      gt_value = as.numeric(.data$gt_value),
      gt_unit = .data$gt_unit,
      source = .data$source,
      url = .data$url,
      definition = .data$basis,
      tolerance_pct = 8,
      confidence = "high"
    )
  vp <- run_validation(p2, list(production = production), c2, lookups)
  add(
    "production_psd",
    "external",
    nrow(vp),
    sum(vp$verdict == "pass"),
    sum(vp$verdict %in% c("flag_high", "flag_low")),
    "WHEP vs USDA FAS PSD (independent of FAOSTAT)"
  )
}

# 3. occupation + land_per_tonne (external, vs LCA literature) -----------------
occ_gt <- jsonlite::fromJSON("validation/cache/ground_truth/occupation.json") |>
  tibble::as_tibble() |>
  dplyr::transmute(
    item_cbs_code = as.integer(.data$item_cbs_code),
    lit = .data$gt_value,
    lo = .data$gt_low,
    hi = .data$gt_high
  )
score_occ <- function(extractor, label) {
  w <- extractor(production, bench_years) |>
    dplyr::inner_join(occ_gt, by = "item_cbs_code") |>
    dplyr::mutate(
      m2 = .data$whep_value * 10,
      in_range = .data$m2 >= .data$lo & .data$m2 <= .data$hi
    )
  add(
    label,
    "external",
    nrow(w),
    sum(w$in_range),
    sum(!w$in_range),
    "ha-yr/t vs Poore & Nemecek 2018 range (m2*yr/kg)"
  )
}
score_occ(extract_occupation_intensity, "occupation")
score_occ(extract_land_per_tonne, "land_per_tonne")

# 4. cropping_intensity (bound, vs GAEZ potential) -----------------------------
gaez_path <- "validation/cache/ground_truth/cropping_intensity.json"
if (file.exists(gaez_path)) {
  gz <- jsonlite::fromJSON(gaez_path) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      ceiling = .data$gt_value
    )
  ci <- extract_cropping_intensity(production, bench_years) |>
    dplyr::inner_join(gz, by = "area_code") |>
    dplyr::mutate(within = .data$whep_value <= .data$ceiling * 1.2)
  add(
    "cropping_intensity",
    "bound",
    nrow(ci),
    sum(ci$within),
    sum(!ci$within),
    "observed harvested/physical <= GAEZ potential ceiling"
  )
}

# 4b. cropping_intensity OBSERVED (vs MapSPAM, if the GT has been built) -------
spam_path <- "validation/cache/ground_truth/cropping_intensity_spam.json"
if (file.exists(spam_path)) {
  sp <- jsonlite::fromJSON(spam_path) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      gt = .data$gt_value
    )
  co <- extract_cropping_intensity(production, 2010L) |>
    dplyr::inner_join(sp, by = c("area_code", "item_cbs_code")) |>
    dplyr::mutate(ok = abs(.data$whep_value / .data$gt - 1) <= 0.25)
  add(
    "cropping_intensity_obs",
    "external",
    nrow(co),
    sum(co$ok),
    sum(!co$ok),
    "WHEP observed intensity vs MapSPAM observed (harvested/physical)"
  )
}

# 5. cycle_length (parameter, vs GGCMI growing-season length) ------------------
cyc_path <- "validation/cache/ground_truth/cycle_length.json"
if (file.exists(cyc_path)) {
  cg <- jsonlite::fromJSON(cyc_path) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      item_cbs_code = as.integer(.data$item_cbs_code),
      gt = .data$gt_value
    )
  cc <- extract_cycle_length() |>
    dplyr::inner_join(cg, by = "item_cbs_code") |>
    dplyr::mutate(ok = abs(.data$whep_value / .data$gt - 1) <= 0.30)
  add(
    "cycle_length",
    "parameter",
    nrow(cc),
    sum(cc$ok),
    sum(!cc$ok),
    "WHEP MIRCA cycle vs GGCMI growing-season length (months)"
  )
} else {
  add(
    "cycle_length",
    "parameter",
    nrow(extract_cycle_length()),
    NA,
    NA,
    "GT not pinned"
  )
}

cat("\n=== WHEP validation scorecard ===\n")
dplyr::bind_rows(scorecard) |> print(n = Inf, width = Inf)
