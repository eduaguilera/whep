# OBSERVED cropping intensity from MapSPAM (harvested area / physical area per
# crop per country). This upgrades the cropping_intensity check from a GAEZ
# POTENTIAL *bound* to an OBSERVED *match* against an independent dataset.
#
# MapSPAM is download-gated (Harvard Dataverse guestbook for SPAM2020; the
# SPAM2010 S3 keys are not openly listable), so it is NOT auto-fetched. Obtain
# it ONCE - from https://www.mapspam.info/data/ (or the Dataverse guestbook) get
# the global PHYSICAL area and HARVESTED area CSVs and drop them in
# validation/cache/data/MapSPAM/. Then run this script; it writes
# cache/ground_truth/cropping_intensity_spam.json. If the CSVs are absent it
# exits cleanly.
#
# Usage: Rscript validation/spam_intensity.R

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(data.table)
})

dir <- "validation/cache/data/MapSPAM"
phys_f <- list.files(
  dir,
  pattern = "phys.*area.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)
harv_f <- list.files(
  dir,
  pattern = "harv.*area.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(phys_f) == 0 || length(harv_f) == 0) {
  cli::cli_alert_warning(
    "MapSPAM CSVs not found in {.path {dir}} - obtain from mapspam.info/data
    (global physical + harvested area). Skipping."
  )
  quit(save = "no", status = 0)
}

# crop column prefix -> item_cbs_code (+ readable crop)
crops <- data.table(
  prefix = c("whea", "rice", "maiz"),
  item_cbs_code = c(2511L, 2514L, 2807L)[c(1L, 3L, 2L)],
  crop = c("Wheat", "Rice", "Maize (corn)")
)

# Sum a crop's area over countries from a SPAM CSV. Handles both the SPAM2010
# layout (bare crop columns + a tech_type column; total = tech_type "A") and the
# SPAM2020 layout (crop columns suffixed "_a" for all-technologies).
.spam_country_area <- function(path) {
  dt <- fread(path[[1]], showProgress = FALSE)
  setnames(dt, tolower(names(dt)))
  iso_col <- intersect(c("iso3", "name_cntr", "cntry"), names(dt))[1]
  if (is.na(iso_col)) {
    cli::cli_abort("No ISO3 column in {.path {path}}.")
  }
  if ("tech_type" %in% names(dt)) {
    dt <- dt[toupper(tech_type) %in% c("A", "TA")]
  }
  out <- lapply(seq_len(nrow(crops)), function(i) {
    p <- crops$prefix[i]
    col <- intersect(c(paste0(p, "_a"), p), names(dt))[1]
    if (is.na(col)) {
      return(NULL)
    }
    g <- dt[,
      .(area = sum(as.numeric(get(col)), na.rm = TRUE)),
      by = c(iso_col)
    ]
    g[, `:=`(item_cbs_code = crops$item_cbs_code[i], crop = crops$crop[i])]
    setnames(g, iso_col, "iso3")
    g
  })
  data.table::rbindlist(out)
}

phys <- .spam_country_area(phys_f)
harv <- .spam_country_area(harv_f)
setnames(phys, "area", "physical_ha")
setnames(harv, "area", "harvested_ha")

ci <- merge(phys, harv, by = c("iso3", "item_cbs_code", "crop"))
ci <- ci[physical_ha > 0]
ci[, intensity := harvested_ha / physical_ha]

iso <- as.data.table(whep::regions_full)[,
  .(iso3 = toupper(iso3c), area_code = suppressWarnings(as.integer(code)))
]
iso <- iso[!is.na(area_code)][!duplicated(iso3)]
ci[, iso3 := toupper(iso3)]
out <- merge(ci, iso, by = "iso3")

gt <- out[, .(
  area_iso3 = iso3,
  area_code = area_code,
  item_cbs_code = item_cbs_code,
  crop = crop,
  year = 2010L,
  gt_value = round(intensity, 3),
  gt_unit = "ratio",
  source = "MapSPAM observed harvested/physical area",
  url = "https://www.mapspam.info/data/",
  basis = "observed multi-cropping ratio (harvested area / physical area)"
)]

dir.create(
  "validation/cache/ground_truth",
  showWarnings = FALSE,
  recursive = TRUE
)
writeLines(
  jsonlite::toJSON(gt, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE),
  "validation/cache/ground_truth/cropping_intensity_spam.json"
)
cat(sprintf("Wrote %d MapSPAM observed cropping-intensity rows.\n", nrow(gt)))
