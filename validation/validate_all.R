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

# Scorecard arithmetic for the seam-gate row (#1000/T34-6). `n_gate_failures`
# is the tier A/B/C pass-rate; `n_moved` is `admin_seam_gate.R`'s OWN baseline
# tripwire (validation/admin_seam_gate.R, ~line 527: it `cli_abort()`s the
# whole script when a recorded row moved, whatever the tier gates say). A run
# whose baseline moved has therefore already failed the script that produced
# these numbers, so folding only `n_gate_failures` into the scorecard's `flag`
# column let it read green while `admin_seam_gate.R` exited non-zero -- the
# moved baseline was demoted to free text nobody scans. `flag` is the union of
# both signals so a moved baseline always shows up as a flag, not a footnote.
.asg_scorecard_flag <- function(n_gate_failures, n_moved) {
  n_gate_failures + n_moved
}
# Runs on every `source()` of this file, cache or no cache: a moved baseline
# must inflate the flag even when every tier gate itself passed (the shape the
# bug missed), so the invariant is pinned here rather than only downstream of
# the WHEP_SPATIALIZE_OUT_DIR-gated block that would otherwise be the only
# place exercising it.
stopifnot(
  "a moved seam-gate baseline must inflate the scorecard flag, not just the
   free-text note" = .asg_scorecard_flag(n_gate_failures = 0, n_moved = 3) > 0
)

year_min <- as.integer(Sys.getenv("VAL_YEAR_MIN", "1970"))
year_max <- as.integer(Sys.getenv("VAL_YEAR_MAX", "2010"))
bench_years <- c(1990L, 2000L, 2010L)
# The production build is NOT triggered here on purpose: it takes minutes to
# hours and reads pins, which is not something a validation sweep should start
# without being asked. But a bare readRDS() on a missing cache dies inside
# gzfile() naming only a path, which reads as corruption rather than as setup
# not done -- so say which it is.
production_cache <- sprintf(
  ".whep_cache/primary_prod_%d_%d.rds",
  year_min,
  year_max
)
if (!file.exists(production_cache)) {
  cli::cli_abort(c(
    "No cached WHEP production at {.path {production_cache}}.",
    i = "{.path .whep_cache/} is gitignored, so a fresh checkout has none.",
    i = "Build it once with {.code Rscript validation/rank_countries.R}, or set
         {.envvar VAL_YEAR_MIN}/{.envvar VAL_YEAR_MAX} to a window you have."
  ))
}
production <- readRDS(production_cache)
lookups <- whep_validation_lookups()
# The scorecard accumulates across ~15 independent checks, so `add()` has to
# reach outside itself. It writes into a named environment rather than using
# `<<-`: the target is then stated at the call site instead of resolved by
# whichever frame happens to hold a `scorecard` binding, and it is what
# `assignment_linter` asks for.
scores <- new.env(parent = emptyenv())
scores$rows <- list()
add <- function(variable, archetype, n, ok, flag, note) {
  scores$rows[[length(scores$rows) + 1L]] <- tibble::tibble(
    variable = variable,
    archetype = archetype,
    n_checked = n,
    ok = ok,
    flag = flag,
    note = note
  )
  invisible()
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

# 1b. year scoping (internal) --------------------------------------------------
# `build_x(years = Y)` must equal the full-range build filtered to Y. Three
# violations shipped green on CI (#623, #625, #570) because no automated check
# compared the two; this is that check (#631). It shells out once per layer so
# the session build cache never holds the full production AND the full CBS at
# once.
#
# Each layer needs the full build, which is the very thing this sweep refuses to
# start unasked (see the note above). So a layer runs only when its cached full
# build is already on disk, or when VAL_SCOPING_LAYERS names it explicitly.
scoping_caches <- c(
  production = ".whep_cache/scoping_full_production.rds",
  wide_cbs = ".whep_cache/scoping_full_wide_cbs.rds"
)
scoping_forced <- strsplit(Sys.getenv("VAL_SCOPING_LAYERS", ""), ",")[[1]]
score_scoping <- function(layer) {
  variable <- paste0("year_scoping_", layer)
  if (!layer %in% scoping_forced && !file.exists(scoping_caches[[layer]])) {
    return(add(
      variable,
      "internal",
      NA,
      NA,
      NA,
      sprintf(
        "not run: no %s, and not in VAL_SCOPING_LAYERS",
        scoping_caches[[layer]]
      )
    ))
  }
  out <- system2(
    "Rscript",
    c(
      "validation/year_scoping.R",
      layer,
      Sys.getenv("VAL_SCOPING_YEAR", "2010")
    ),
    stdout = TRUE,
    stderr = FALSE
  )
  metric <- grep("^METRIC", out, value = TRUE)
  if (length(metric) != 1L) {
    return(add(variable, "internal", NA, NA, NA, "no METRIC line reported"))
  }
  num <- function(key) {
    as.numeric(sub(paste0(".*", key, "=([0-9.e+-]+).*"), "\\1", metric))
  }
  add(
    variable,
    "internal",
    num("units"),
    num("units") - num("failing"),
    num("failing"),
    sprintf(
      "scoped vs full-range: %.0f keys only in full, max rel total %.2e",
      num("keys_only_full"),
      num("max_rel_total")
    )
  )
}
purrr::walk(names(scoping_caches), score_scoping)

# 1c. temporary grassland vs FAO 6633 (external) -------------------------------
# Modelled CBS 3002 against FAOSTAT RL item 6633, official (flag "A") rows only.
# Like the scoping layers it needs a production build, so it runs only when its
# cache already exists or VAL_TG_FORCE is set -- the sweep does not start a
# multi-minute build unasked.
tg_cache <- sprintf(
  ".whep_cache/temp_grassland_ha_%s_%s.rds",
  Sys.getenv("VAL_TG_YEAR_MIN", "2001"),
  Sys.getenv("VAL_TG_YEAR_MAX", "2023")
)
if (!nzchar(Sys.getenv("VAL_TG_FORCE")) && !file.exists(tg_cache)) {
  add(
    "temp_grassland_6633",
    "external",
    NA,
    NA,
    NA,
    sprintf("not run: no %s, and VAL_TG_FORCE unset", tg_cache)
  )
} else {
  tg_out <- system2(
    "Rscript",
    "validation/temp_grassland_6633.R",
    stdout = TRUE,
    stderr = FALSE
  )
  tg_metric <- grep("^METRIC", tg_out, value = TRUE)
  if (length(tg_metric) != 1L) {
    add(
      "temp_grassland_6633",
      "external",
      NA,
      NA,
      NA,
      "no METRIC line reported"
    )
  } else {
    tg_num <- function(key) {
      as.numeric(sub(paste0(".*", key, "=([0-9.e+-]+).*"), "\\1", tg_metric))
    }
    add(
      "temp_grassland_6633",
      "external",
      tg_num("n_official_3002"),
      tg_num("n_official_3002") - tg_num("n_failed"),
      tg_num("n_failed"),
      sprintf(
        "CBS 3002 / FAO 6633 = %.2f, whole green-fodder group = %.2f",
        tg_num("sum_ratio_3002"),
        tg_num("sum_ratio_green_on_3002")
      )
    )
  }
}

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
occ_path <- "validation/cache/ground_truth/occupation.json"
if (!file.exists(occ_path)) {
  cli::cli_warn(c(
    "Skipping occupation and land_per_tonne: no ground truth at
     {.path {occ_path}}.",
    i = "{.path validation/cache/} is gitignored, so a fresh clone has none;
         see {.file validation/README.md}."
  ))
} else {
  occ_gt <- jsonlite::fromJSON(occ_path) |>
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
}
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

# N. nourishment axis, per item (external, vs FAOSTAT FBS protein) ------------
# Scored on the PER-ITEM disagreement, not the Grand Total: the axis's net
# excess is small because item errors cancel, so a net-only check reads as
# accurate while individual items are tens of percent out (#500).
nour <- tryCatch(
  system2(
    "Rscript",
    c("validation/nourishment_axis.R", Sys.getenv("VAL_YEAR", "2010")),
    stdout = TRUE,
    stderr = FALSE
  ),
  error = function(e) character(0)
)
nour_metric <- grep("^METRIC", nour, value = TRUE)
if (length(nour_metric) == 1L) {
  num <- function(key) {
    as.numeric(sub(
      paste0(".*", key, "=([0-9.]+).*"),
      "\\1",
      nour_metric
    ))
  }
  add(
    "nourishment_axis",
    "per_item",
    num("items_compared"),
    num("items_compared") - num("items_off_10pct"),
    num("items_off_10pct"),
    sprintf(
      "items within 10%% of FBS protein; net ratio %.3f hides a %.0fx
       cancellation",
      num("net_ratio"),
      num("cancellation")
    )
  )
} else {
  add(
    "nourishment_axis",
    "per_item",
    NA,
    NA,
    NA,
    "needs the CBS build and the faostat-fbs-new pin"
  )
}

# A. admin-unit drift (external, vs the compiled subnational panel) ------------
# T18a (#1000): how much within-country geography a spatialization pattern
# frozen at one reference year cannot represent. The panel is an internal
# compilation that is not redistributed, so the sweep reports "not run" rather
# than starting anything when WHEP_SUBNATIONAL is unset.
adt_out <- if (nzchar(Sys.getenv("WHEP_SUBNATIONAL"))) {
  system2(
    "Rscript",
    "validation/admin_drift_tvd.R",
    stdout = TRUE,
    stderr = FALSE
  )
} else {
  character(0)
}
adt_metric <- grep("^METRIC", adt_out, value = TRUE)
if (length(adt_metric) != 1L || grepl("status=skipped", adt_metric)) {
  add(
    "admin_drift",
    "external",
    NA,
    NA,
    NA,
    "not run: WHEP_SUBNATIONAL unset (the panel is not redistributed)"
  )
} else {
  adt_num <- function(key) {
    as.numeric(sub(paste0(".*", key, "=([0-9.e+-]+).*"), "\\1", adt_metric))
  }
  add(
    "admin_drift",
    "external",
    adt_num("n_tvd_rows"),
    adt_num("n_tvd_rows") - adt_num("n_failed"),
    adt_num("n_failed"),
    sprintf(
      "share-vector TVD vs the frozen reference year; Spain harvested area
       1961 = %.1f%% of the national total in a different province",
      adt_num("spain_area_1961")
    )
  )
}

# B. seam gate (internal, on a spatialization run) ----------------------------
# T29 (#1000): whether a back-cast admin-share table and the cells it produced
# are continuous across every seam the resolver found. It reads one existing
# run_spatialize() output directory and nothing else, so the sweep reports
# "not run" rather than starting a build when WHEP_SPATIALIZE_OUT_DIR is unset.
asg_out <- if (nzchar(Sys.getenv("WHEP_SPATIALIZE_OUT_DIR"))) {
  system2(
    "Rscript",
    "validation/admin_seam_gate.R",
    stdout = TRUE,
    stderr = FALSE
  )
} else {
  character(0)
}
asg_metric <- grep("^METRIC", asg_out, value = TRUE)
if (length(asg_metric) != 1L || grepl("status=skipped", asg_metric)) {
  add(
    "admin_seam_gate",
    "internal",
    NA,
    NA,
    NA,
    "not run: WHEP_SPATIALIZE_OUT_DIR unset (needs a spatialization run)"
  )
} else {
  asg_num <- function(key) {
    as.numeric(sub(paste0(".*", key, "=([0-9.e+-]+).*"), "\\1", asg_metric))
  }
  asg_flag <- .asg_scorecard_flag(
    asg_num("n_gate_failures"),
    asg_num("n_moved")
  )
  add(
    "admin_seam_gate",
    "internal",
    asg_num("n_tier_c_gates"),
    asg_num("n_tier_c_gates") - asg_flag,
    asg_flag,
    sprintf(
      "seam continuity over %d seam(s) of %d kind(s); %d gate failure(s), %d
       recorded row(s) moved",
      asg_num("n_seams"),
      asg_num("n_seam_kinds"),
      asg_num("n_gate_failures"),
      asg_num("n_moved")
    )
  )
}

# C. level-0 grid vintage (internal, on the polycell support) -----------------
# T39 (#1000): what changes when level 0 stops being the 2015 snapshot and
# becomes year-aware. This is a MEASUREMENT behind an open decision, not a
# gate: which vintage is right is the question the measurement exists to
# inform, so there is no pass criterion to score and `ok`/`flag` stay NA while
# the note carries the magnitudes. Building the year-aware grid costs minutes
# and reads the polycell pin, and this sweep does not start a build without
# being asked (the rule stated for the production cache above), so the row
# reports "not run" until the script has been run once and left its cache.
gv_cache_rds <- "validation/cache/grid_vintage_year_aware.rds"
gv_out <- if (file.exists(gv_cache_rds)) {
  system2(
    "Rscript",
    "validation/spatialize_grid_vintage.R",
    stdout = TRUE,
    stderr = FALSE
  )
} else {
  character(0)
}
gv_metric <- grep("^METRIC", gv_out, value = TRUE)
if (length(gv_metric) != 1L || grepl("status=skipped", gv_metric)) {
  add(
    "grid_vintage",
    "internal",
    NA,
    NA,
    NA,
    "not run: no cached year-aware grid (run
     `Rscript validation/spatialize_grid_vintage.R` once)"
  )
} else {
  # `cropland_loss_pct_max` and `at_year` are the literal "NA" when no year
  # carries a cropland weight, so the field is read as text and coerced
  # quietly rather than assumed numeric.
  gv_num <- function(key) {
    suppressWarnings(as.numeric(sub(
      paste0(".*", key, "=([^ ]+).*"),
      "\\1",
      gv_metric
    )))
  }
  add(
    "grid_vintage",
    "internal",
    gv_num("n_years"),
    NA,
    NA,
    sprintf(
      "measurement, not a gate: %.0f cell(s) of the 2015 snapshot absent from
       the year-aware grid in 1851; worst cropland loss %.2f%% at %.0f; %.0f
       engine year(s)",
      gv_num("n_cells_absent_1851"),
      gv_num("cropland_loss_pct_max"),
      gv_num("at_year"),
      gv_num("n_engine_years")
    )
  )
}

# D. Japan depth-1 pilot (internal, one constrained spatialization) ----------
# #1000: the first `run_spatialize(level = 1)` on real administrative
# statistics. It runs an allocation, so it is never started unasked: it needs
# a Japan-only polycell support (`WHEP_POLYCELL_SUPPORT_PATH`) and a directory
# of prepared spatialization parquets (`VAL_JP_INPUT_DIR`), and reports "not
# run" when either is absent.
#
# `flag` is the union of three signals, for the reason the seam-gate row
# above states: the pilot exits non-zero on a moved anchor and on a moved
# recorded row, and neither would show in a gate-failure count alone. The
# anchor is the load-bearing one -- it is the hand-computed Hokkaido number
# the whole leg exists to reproduce -- so `anchor_ok = 0` must flag even when
# every recorded row still matches.
.jp_scorecard_flag <- function(n_gate_failures, n_failed, anchor_ok) {
  n_gate_failures + n_failed + (1L - anchor_ok)
}
stopifnot(
  "a moved Japan anchor must inflate the scorecard flag, not just the
   free-text note" = .jp_scorecard_flag(0, 0, 0L) > 0
)
jp_ready <- nzchar(Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH")) &&
  nzchar(Sys.getenv("VAL_JP_INPUT_DIR"))
jp_out <- if (jp_ready) {
  system2("Rscript", "validation/japan_pilot.R", stdout = TRUE, stderr = FALSE)
} else {
  character(0)
}
jp_metric <- grep("^METRIC", jp_out, value = TRUE)
if (length(jp_metric) != 1L || grepl("status=skipped", jp_metric)) {
  add(
    "japan_pilot",
    "internal",
    NA,
    NA,
    NA,
    "not run: needs WHEP_POLYCELL_SUPPORT_PATH (the Japan pilot support) and
     VAL_JP_INPUT_DIR"
  )
} else {
  # Anchored on the leading space, unlike the older readers above: this
  # METRIC line carries both `n_groups` and `n_prescan_groups`, and an
  # unanchored `n_groups=` matches inside the longer key.
  jp_num <- function(key) {
    # `suppressWarnings` for the same reason `grid_vintage`'s reader has it:
    # a field can legitimately be the literal "NA" -- `anchor_ha` is, when the
    # anchor's group is not in the run at all -- and the coercion should be
    # quiet rather than warn. `anchor_ok` and `n_failed` are integers whatever
    # happens, so the flag below still resolves.
    suppressWarnings(as.numeric(sub(
      paste0(".* ", key, "=(-?[0-9.e+-]+).*"),
      "\\1",
      jp_metric
    )))
  }
  jp_flag <- .jp_scorecard_flag(
    jp_num("n_gate_failures"),
    jp_num("n_failed"),
    as.integer(jp_num("anchor_ok"))
  )
  add(
    "japan_pilot",
    "internal",
    jp_num("n_groups"),
    jp_num("n_groups") - jp_num("n_beyond_tolerance"),
    jp_flag,
    sprintf(
      "Hokkaido paddy rice 2000 = %.2f ha (rel %.1e); %.0f of %.0f pre-scan
       group(s) refused by T31(d), worst %.1f%% short; %.0f group(s) beyond
       tolerance; %.0f of %.0f unit-item series gapped, longest %.0f yr;
       %.0f of %.0f seam gate(s) failing",
      jp_num("anchor_ha"),
      jp_num("anchor_rel"),
      jp_num("n_refused"),
      jp_num("n_prescan_groups"),
      100 * jp_num("worst_disc_frac"),
      jp_num("n_beyond_tolerance"),
      jp_num("n_gap_series"),
      jp_num("n_series"),
      jp_num("max_gap_run"),
      jp_num("n_gate_failures"),
      jp_num("n_gates")
    )
  )
}

cat("\n=== WHEP validation scorecard ===\n")
dplyr::bind_rows(scores$rows) |> print(n = Inf, width = Inf)
