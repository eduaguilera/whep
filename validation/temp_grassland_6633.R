# Temporary-grassland reconstruction check (external ground-truth archetype).
#
# PR #349 nets modelled CBS 3002 ("Temporary grassland") out of the FAO Arable
# land target, on the premise that CBS 3002 is the temporary grassland FAO
# already counts inside arable land (#342). This script validates that premise
# against FAO's own direct measurement of the same thing: FAOSTAT Land Use (RL)
# item 6633, "Temporary meadows and pastures".
#
# ## Why the comparison needs a provenance filter
#
# FAOSTAT ships an observation-status flag with every value, and 6633 is mostly
# not observed: over 2001-2023 the pin carries 1100 official rows (flag "A")
# against 4001 imputed ones (flag "I"), i.e. only ~19% of the series is a
# reported figure. 900 of those imputed rows are zeros -- and FAO's own standard
# says that is exactly what an imputed zero means:
#
#   "In case of a missing value replaced by FAO with a 0 because the phenomenon
#    is assumed negligible for the considered unit, the flag to use is 'I'
#    (imputed) and NOT 'N - not significant'."
#      -- FAO, Statistical Standard Series: Observation Status Code List,
#         Version 4, endorsed by DCG-T on 10 July 2025, section on flag "I".
#
# Greece and Poland are imputed zeros for every year 2001-2023, while WHEP
# models 2.10 and 4.78 Mha of temporary grassland there. Scoring those pairs as
# disagreement measures FAO's gap-filling, not WHEP. So the judged comparison
# uses official rows only, and the other classes are reported beside it rather
# than mixed into it.
#
# The same rule decides what happens when several FAOSTAT reporting areas
# collapse into one WHEP polity: the polity-year is official only when every
# contributing raw row is, which is FAO's own composition rule ("the flag 'A'
# should be used only if the figures at numerator and denominator are both
# flagged as 'A'").
#
# ## Why two modelled concepts are compared, not one
#
# `cbs_3002` is what PR #349 nets. `green_fodder` is the whole green-fodder
# group WHEP books as harvested from arable land (CBS 3002 plus 2000 fodder
# cereals and grasses, 2001 fodder legumes, 2002 fodder vegetables and roots,
# 2003 fodder mix -- every item with `Cat_1 == "Fodder_green"`). It is also
# reported restricted to the CBS 3002 country-years (`green_on_3002`): the green
# group covers many more countries, so an unrestricted comparison would confound
# the concept difference with a coverage difference.
#
# Which of the two FAO 6633 corresponds to is country-dependent, and that is the
# finding this check exists to keep visible: for Ireland, Sweden, the United
# Kingdom, the Netherlands, Belgium, Luxembourg and Czechia, FAO 6633 equals
# WHEP's CBS 3002 to the digit; for Germany, Italy, Romania, Spain, Denmark,
# Austria and Bulgaria it is 3 to 40 times larger than CBS 3002 and lands close
# to the whole green-fodder group instead. So the aggregate CBS 3002 shortfall
# is a scope difference, not a reconstruction error, and picking the netting
# basis is a methodological decision (see the issue #354 discussion).
#
# Ireland is a third case again and worth stating separately: from 2007 onward
# modelled CBS 3002 equals FAO 6633 exactly, every year. Before 2007 the model
# carries 748-786 kha against FAO's 89-100 kha -- but every FAO value there is
# flagged "I", back-filled at the post-2007 level. The break is in FAOSTAT's own
# item 996, which WHEP passes through; it is not something the fodder
# reconstruction introduced.
#
# ## The coverage line is part of the finding
#
# Modelled CBS 3002 is not a global series. Inside 2001-2023 it exists for 26 EU
# polities and for the years 2001-2019 only -- FAOSTAT production item 996, its
# sole source in that window, stops at 2019. So PR #349's netting subtracts
# nothing at all from 2020 onward, and nothing anywhere outside the EU. The
# script prints that coverage every run, because a netting term that quietly
# switches off mid-panel is a discontinuity in published land footprints.
#
# ## Usage
#
#   Rscript validation/temp_grassland_6633.R
#   Rscript validation/temp_grassland_6633.R --record   # re-record baselines
#   Rscript validation/temp_grassland_6633.R --refresh  # rebuild the cache
#
#   VAL_TG_YEAR_MIN / VAL_TG_YEAR_MAX  comparison window (default 2001-2023, the
#                                      full span of FAOSTAT RL item 6633)
#   VAL_TG_PERTURB                     scale the modelled side by this factor,
#                                      to prove the check fires (see below)
#
# `VAL_TG_PERTURB=1.2` multiplies every modelled hectare by 1.2 without touching
# the recorded baseline, so a run under it MUST fail. That is how this check was
# shown to fire rather than merely to pass.
#
# Exits non-zero when any judged group fails, so CI can gate on it.

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
})

source("validation/validate.R")

tg_baseline <- "validation/gt_temp_grassland_6633.json"

# FAOSTAT Land Use item for "Temporary meadows and pastures".
.tg_fao_item <- 6633L

# Relative slack on a recorded ratio. The modelled side is a sum over a build
# whose row order is not guaranteed, so the last digits move; nothing larger is
# forgiven, and every recorded number is a measured state to be re-recorded when
# it changes, not a tolerance.
.tg_floor <- 1e-6

# Observation-status flags that mean "a figure was reported", per the FAO code
# list cited at the top. "A" official value is the only one that survives into
# the judged set; "B" (time series break) and "X" (from an external
# organization) are reported values too but carry a caveat, so they are named
# separately rather than folded into either side.
.tg_flag_class <- function(flags) {
  dplyr::case_when(
    is.na(flags) ~ "no_fao_row",
    flags == "A" ~ "official",
    grepl("I", flags, fixed = TRUE) ~ "imputed",
    TRUE ~ "reported_caveat"
  )
}

# FAO 6633 area in hectares per (year, polity area_code), carrying the set of
# observation-status flags behind it. Raw FAOSTAT reporting areas are collapsed
# to WHEP polities the same way `.grassland_occupation_faostat()` does it, so
# the two sides share one code space; unmapped statistical aggregates (FAOSTAT
# "China" 351, continents, "World") drop out there and cannot double-count.
tg_read_fao <- function(landuse = NULL) {
  if (is.null(landuse)) {
    landuse <- whep_read_file("faostat-landuse")
  }
  landuse |>
    dplyr::filter(
      .data[["Item Code"]] == .tg_fao_item,
      .data$Element == "Area"
    ) |>
    dplyr::transmute(
      year = as.integer(.data$Year),
      area_code = as.integer(.data[["Area Code"]]),
      fao_ha = .data$Value * 1000,
      flag = .data$Flag
    ) |>
    dplyr::filter(!is.na(.data$fao_ha)) |>
    add_polity_code(code_column = "area_code", year_column = "year") |>
    dplyr::filter(!is.na(.data$polity_code)) |>
    dplyr::summarise(
      fao_ha = sum(.data$fao_ha, na.rm = TRUE),
      n_raw = dplyr::n(),
      flags = paste(sort(unique(.data$flag)), collapse = ""),
      .by = c("year", "polity_area_code")
    ) |>
    dplyr::rename(area_code = "polity_area_code")
}

# The two modelled concepts, as CBS item-code sets. `green_fodder` is derived
# from `items_full` rather than hardcoded so a new Fodder_green item joins it.
tg_concepts <- function() {
  green <- whep::items_full |>
    dplyr::filter(.data$Cat_1 == "Fodder_green") |>
    dplyr::pull(.data$item_cbs_code) |>
    unique() |>
    as.integer()
  list(cbs_3002 = 3002L, green_fodder = green)
}

# Modelled harvested area (hectares) per (year, area_code) for one concept.
# `area_code` in the production output is already the polity code, so no further
# mapping is needed on this side.
tg_model_area <- function(prod_ha, item_codes, perturb = 1) {
  prod_ha |>
    dplyr::filter(.data$item_cbs_code %in% item_codes) |>
    dplyr::summarise(
      model_ha = sum(.data$value, na.rm = TRUE) * perturb,
      .by = c("year", "area_code")
    )
}

# Join one concept against FAO and label each pair by provenance class.
tg_pairs <- function(model, fao) {
  dplyr::inner_join(model, fao, by = c("year", "area_code")) |>
    dplyr::mutate(cls = .tg_flag_class(.data$flags))
}

# Per-class aggregates. `sum_ratio` is the aggregate the netting decision cares
# about; `median_ratio` is the typical country-year and is reported over rows
# with a positive FAO value only, because an imputed zero has no ratio.
tg_summarise <- function(pairs) {
  pairs |>
    dplyr::summarise(
      n_pairs = dplyr::n(),
      n_fao_zero = sum(.data$fao_ha == 0),
      model_Mha = sum(.data$model_ha) / 1e6,
      fao_Mha = sum(.data$fao_ha) / 1e6,
      sum_ratio = sum(.data$model_ha) / sum(.data$fao_ha),
      median_ratio = stats::median(
        (.data$model_ha / .data$fao_ha)[.data$fao_ha > 0]
      ),
      .by = "cls"
    ) |>
    dplyr::arrange(.data$cls)
}

# Judge the official class of each concept against its recorded state. A concept
# never recorded fails loudly the first time -- record it deliberately.
tg_verdict <- function(measured, baseline) {
  recorded <- baseline[[measured$concept]]
  if (is.null(recorded)) {
    return(dplyr::mutate(
      measured,
      recorded_sum_ratio = NA_real_,
      fail = TRUE,
      why = "not recorded"
    ))
  }
  drift <- function(field) {
    abs(measured[[field]] - as.numeric(recorded[[field]])) /
      pmax(abs(as.numeric(recorded[[field]])), .tg_floor)
  }
  measured |>
    dplyr::mutate(
      recorded_sum_ratio = as.numeric(recorded$sum_ratio),
      fail = .data$n_pairs != as.integer(recorded$n_pairs) |
        drift("sum_ratio") > .tg_floor |
        drift("median_ratio") > .tg_floor,
      why = dplyr::if_else(.data$fail, "moved against baseline", "")
    )
}

# --- Driver -------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
record <- "--record" %in% args
refresh <- "--refresh" %in% args
year_min <- as.integer(Sys.getenv("VAL_TG_YEAR_MIN", "2001"))
year_max <- as.integer(Sys.getenv("VAL_TG_YEAR_MAX", "2023"))
perturb <- as.numeric(Sys.getenv("VAL_TG_PERTURB", "1"))

cli::cli_h1("Temporary grassland vs FAO 6633: {year_min}-{year_max}")
if (perturb != 1) {
  cli::cli_alert_warning(
    "{.envvar VAL_TG_PERTURB}={perturb}: the modelled side is scaled, so every
     judged group is expected to FAIL."
  )
}

prod_cache <- sprintf(
  ".whep_cache/temp_grassland_ha_%d_%d.rds",
  year_min,
  year_max
)
prod_ha <- harness_build_or_cache(
  prod_cache,
  function() {
    build_primary_production(start_year = year_min, end_year = year_max) |>
      dplyr::filter(.data$unit == "ha") |>
      dplyr::select("year", "area_code", "item_cbs_code", "value")
  },
  refresh = refresh
)

fao <- tg_read_fao() |>
  dplyr::filter(.data$year >= year_min, .data$year <= year_max)
concepts <- tg_concepts()

pairs <- lapply(
  names(concepts),
  function(nm) {
    tg_pairs(tg_model_area(prod_ha, concepts[[nm]], perturb), fao) |>
      dplyr::mutate(concept = nm)
  }
)
names(pairs) <- names(concepts)

# The concept comparison, on one coverage. Without this the green group's extra
# countries would be read as a concept effect.
pairs$green_on_3002 <- pairs$green_fodder |>
  dplyr::semi_join(
    dplyr::distinct(pairs$cbs_3002, .data$year, .data$area_code),
    by = c("year", "area_code")
  ) |>
  dplyr::mutate(concept = "green_on_3002")

cli::cli_h2("Modelled CBS 3002 coverage in this window")
coverage <- prod_ha |>
  dplyr::filter(.data$item_cbs_code == 3002L, .data$value > 0) |>
  dplyr::summarise(
    n_polities = dplyr::n_distinct(.data$area_code),
    first_year = min(.data$year),
    last_year = max(.data$year),
    n_rows = dplyr::n()
  )
print(coverage)
if (coverage$last_year < year_max) {
  cli::cli_alert_warning(
    "Modelled CBS 3002 stops at {coverage$last_year}, so PR #349 nets nothing
     out of the arable target from {coverage$last_year + 1} to {year_max}."
  )
}

per_class <- dplyr::bind_rows(lapply(
  names(pairs),
  function(nm) dplyr::mutate(tg_summarise(pairs[[nm]]), concept = nm)
))

cli::cli_h2("Per provenance class (only {.val official} is judged)")
per_class |>
  dplyr::relocate("concept") |>
  print(n = Inf, width = Inf)

# The structural half of the check: nothing FAO imputed may reach the judged
# set. A refactor that let flag "I" back in would otherwise pass while measuring
# FAO's gap-filling.
official <- lapply(pairs, function(p) dplyr::filter(p, .data$cls == "official"))
leaked <- vapply(
  official,
  function(p) sum(grepl("I", p$flags, fixed = TRUE)),
  integer(1)
)
if (any(leaked > 0)) {
  cli::cli_abort(
    "{sum(leaked)} imputed FAO row{?s} reached the judged set; the provenance
     filter is broken."
  )
}

measured <- dplyr::bind_rows(lapply(
  names(official),
  function(nm) dplyr::mutate(tg_summarise(official[[nm]]), concept = nm)
)) |>
  dplyr::filter(.data$cls == "official") |>
  dplyr::select(
    "concept",
    "n_pairs",
    "model_Mha",
    "fao_Mha",
    "sum_ratio",
    "median_ratio"
  )

if (record) {
  baseline <- list(
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    window = list(year_min = year_min, year_max = year_max),
    note = paste(
      "Measured state of modelled temporary grassland against FAOSTAT RL 6633,",
      "official (flag A) rows only. Not a tolerance: every number is a",
      "measurement to be re-recorded when the reconstruction changes, and the",
      "gap between the two concepts is the open decision in issue #354."
    )
  )
  for (i in seq_len(nrow(measured))) {
    baseline[[measured$concept[[i]]]] <- as.list(
      dplyr::select(measured[i, ], -"concept")
    )
  }
  writeLines(
    jsonlite::toJSON(baseline, auto_unbox = TRUE, pretty = TRUE, digits = 17),
    tg_baseline
  )
  cli::cli_alert_success("Recorded into {.path {tg_baseline}}.")
}

baseline <- if (file.exists(tg_baseline)) {
  jsonlite::fromJSON(tg_baseline, simplifyVector = FALSE)
} else {
  list()
}
verdict <- dplyr::bind_rows(lapply(
  seq_len(nrow(measured)),
  function(i) tg_verdict(measured[i, ], baseline)
))

cli::cli_h2("Judged: official rows only")
verdict |> print(n = Inf, width = Inf)

# The per-country table is the substance of #354: it says which countries treat
# 6633 as CBS 3002 and which treat it as the whole green-fodder group.
by_country <- dplyr::inner_join(
  official$cbs_3002 |>
    dplyr::summarise(
      n = dplyr::n(),
      fao_Mha = sum(.data$fao_ha) / 1e6,
      ratio_3002 = sum(.data$model_ha) / sum(.data$fao_ha),
      .by = "area_code"
    ),
  official$green_on_3002 |>
    dplyr::summarise(
      ratio_green = sum(.data$model_ha) / sum(.data$fao_ha),
      .by = "area_code"
    ),
  by = "area_code"
) |>
  add_area_name(code_column = "area_code") |>
  dplyr::arrange(.data$ratio_3002)

cli::cli_h2("Per country, official rows only")
by_country |> print(n = Inf, width = Inf)

n_fail <- sum(verdict$fail)
cat(sprintf(
  paste0(
    "METRIC years=%d-%d last_modelled_3002=%d n_official_3002=%d ",
    "sum_ratio_3002=%.6f sum_ratio_green_on_3002=%.6f n_imputed_pairs=%d ",
    "n_fao_imputed_zero=%d n_failed=%d perturb=%s\n"
  ),
  year_min,
  year_max,
  coverage$last_year,
  measured$n_pairs[measured$concept == "cbs_3002"],
  measured$sum_ratio[measured$concept == "cbs_3002"],
  measured$sum_ratio[measured$concept == "green_on_3002"],
  nrow(dplyr::filter(pairs$cbs_3002, .data$cls == "imputed")),
  nrow(dplyr::filter(
    pairs$cbs_3002,
    .data$cls == "imputed",
    .data$fao_ha == 0
  )),
  n_fail,
  format(perturb)
))

if (n_fail > 0) {
  cli::cli_abort(
    "{n_fail} concept{?s} moved against {.path {tg_baseline}}; re-record with
     {.code --record} once the change is understood."
  )
}
cli::cli_alert_success(
  "All {nrow(verdict)} concepts match their recorded state."
)
