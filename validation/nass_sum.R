# Deterministic USA subnational extractor from the local USDA NASS QuickStats
# bulk CSV. Sums STATE-level production per (crop, year) into a national total
# in metric tonnes (rice on WHEP's milled-equivalent basis) and writes the
# per-country USA findings JSON the harness reads.
#
# The NASS bulk file is large and NOT committed. Its location is resolved from
# the WHEP_NASS_DIR env var, else the gitignored validation/cache/local_paths.json.
# The prefiltered production slice and the findings file are gitignored too.
#
# Usage: Rscript validation/nass_sum.R [years_csv]   (default 1990,2000,2010)

suppressPackageStartupMessages({
  library(data.table)
})

.resolve_nass_dir <- function() {
  d <- Sys.getenv("WHEP_NASS_DIR", "")
  if (nzchar(d) && dir.exists(d)) {
    return(d)
  }
  cfg <- "validation/cache/local_paths.json"
  if (file.exists(cfg)) {
    p <- jsonlite::fromJSON(cfg)[["WHEP_NASS_DIR"]]
    if (!is.null(p) && dir.exists(p)) {
      return(p)
    }
  }
  stop(
    "NASS dir not found. Set WHEP_NASS_DIR or add it to ",
    "validation/cache/local_paths.json"
  )
}

args <- commandArgs(trailingOnly = TRUE)
years_csv <- if (length(args) >= 1) args[[1]] else "1990,2000,2010"
years <- as.integer(strsplit(years_csv, ",")[[1]])

# crop -> NASS short_desc + unit conversion to tonnes (rice: rough cwt -> milled)
crops <- data.table(
  short_desc = c(
    "CORN, GRAIN - PRODUCTION, MEASURED IN BU",
    "WHEAT - PRODUCTION, MEASURED IN BU",
    "RICE - PRODUCTION, MEASURED IN CWT"
  ),
  crop = c("Maize (corn)", "Wheat", "Rice"),
  kg_per_unit = c(25.4012, 27.2155, 45.3592),
  milling = c(1, 1, 0.67),
  src_unit = c("bu", "bu", "cwt"),
  basis0 = c("grain", "all wheat, grain", "rough cwt x0.67 -> milled-equiv")
)

nass_dir <- .resolve_nass_dir()
slice <- "validation/cache/data/USA/nass_prod_slice.csv"

# One streaming grep pass over the multi-GB file, cached as a compact slice.
if (!file.exists(slice)) {
  dir.create(dirname(slice), recursive = TRUE, showWarnings = FALSE)
  big <- file.path(nass_dir, "crops.csv")
  patterns <- paste(sprintf("-e %s", shQuote(crops$short_desc)), collapse = " ")
  message("Prefiltering NASS crops.csv (one pass, cached after)...")
  system(sprintf("grep -F %s %s > %s", patterns, shQuote(big), shQuote(slice)))
}

dt <- fread(
  slice,
  header = FALSE,
  sep = ",",
  quote = "\"",
  showProgress = FALSE
)
# NASS columns: V10 short_desc, V13 agg_level, V16 state_alpha, V31 year,
# V32 freq, V38 Value.
dt <- dt[V13 == "STATE" & V32 == "ANNUAL" & as.integer(V31) %in% years]
dt[, value_raw := as.numeric(gsub(",", "", V38))]
dt <- dt[!is.na(value_raw)]
dt[, year := as.integer(V31)]

joined <- crops[dt, on = c(short_desc = "V10"), nomatch = 0]
agg <- joined[,
  .(src_total = sum(value_raw), n_states = uniqueN(V16)),
  by = .(crop, year, kg_per_unit, milling, src_unit, basis0)
]
agg[, value := round(src_total * kg_per_unit / 1000 * milling, 0)]

findings <- agg[, .(
  country_iso3 = "USA",
  crop = crop,
  year = year,
  value = value,
  unit = "tonnes",
  basis = sprintf(
    "%s; sum %s %s x %.4f kg/unit",
    basis0,
    format(src_total, big.mark = ","),
    src_unit,
    kg_per_unit
  ),
  n_subnational_units = n_states,
  source = "USDA NASS QuickStats bulk (local), STATE agg summed",
  url = "local:NASS/crops.csv",
  from_cache = TRUE,
  confidence = "high",
  notes = "deterministic state-sum; no web/PDF"
)]
data.table::setorder(findings, crop, year)

dir.create("validation/cache/findings", recursive = TRUE, showWarnings = FALSE)
writeLines(
  jsonlite::toJSON(
    findings,
    dataframe = "rows",
    auto_unbox = TRUE,
    pretty = TRUE
  ),
  "validation/cache/findings/USA.json"
)
cat(sprintf(
  "Wrote %d USA findings to validation/cache/findings/USA.json\n",
  nrow(findings)
))
print(findings[, .(crop, year, value, n_subnational_units)])
