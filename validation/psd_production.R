# USDA FAS PSD -> independent global production ground truth (per country, crop,
# year). PSD is compiled independently of FAOSTAT (which WHEP ingests), so it is
# a genuine external cross-check. PSD rice is reported on a MILLED basis and
# corn/wheat as grain - both match WHEP's bases. Writes the per-country PSD
# production ground truth into cache/ground_truth as JSON.
#
# Usage: Rscript validation/psd_production.R   (reads the bulk CSV in cache/data)

suppressPackageStartupMessages({
  library(data.table)
})

csv <- "validation/cache/data/USDA_PSD/psd_grains_pulses.csv"
dt <- fread(csv, showProgress = FALSE)

crop_map <- data.table(
  Commodity_Description = c("Wheat", "Corn", "Rice"),
  item_cbs_code = c(2511L, 2514L, 2807L),
  crop = c("Wheat", "Maize (corn)", "Rice")
)
# PSD country name -> ISO3 for the major producers (explicit = reliable).
iso_map <- data.table(
  Country_Name = c(
    "United States",
    "China",
    "India",
    "Brazil",
    "Russia",
    "France",
    "Indonesia",
    "Argentina",
    "Vietnam",
    "Bangladesh",
    "Ukraine",
    "Canada",
    "Australia",
    "Pakistan",
    "Germany",
    "Thailand",
    "Mexico",
    "Egypt",
    "Turkey",
    "Nigeria",
    "Myanmar",
    "Philippines"
  ),
  area_iso3 = c(
    "USA",
    "CHN",
    "IND",
    "BRA",
    "RUS",
    "FRA",
    "IDN",
    "ARG",
    "VNM",
    "BGD",
    "UKR",
    "CAN",
    "AUS",
    "PAK",
    "DEU",
    "THA",
    "MEX",
    "EGY",
    "TUR",
    "NGA",
    "MMR",
    "PHL"
  )
)
years <- c(1990L, 2000L, 2010L)

prod <- dt[Attribute_Description == "Production" & Market_Year %in% years]
prod <- crop_map[prod, on = "Commodity_Description", nomatch = 0]
prod <- iso_map[prod, on = "Country_Name", nomatch = 0]
prod[, value := as.numeric(Value) * 1000] # PSD Production is in 1000 MT
prod <- prod[is.finite(value) & value > 0]

gt <- prod[, .(
  area_iso3 = area_iso3,
  crop = crop,
  year = as.integer(Market_Year),
  gt_value = round(value),
  gt_unit = "tonnes",
  source = "USDA FAS PSD (grains_pulses)",
  url = "https://apps.fas.usda.gov/psdonline/",
  basis = "Production; rice=milled, corn/wheat=grain (matches WHEP bases)"
)]
setorder(gt, crop, area_iso3, year)

dir.create(
  "validation/cache/ground_truth",
  showWarnings = FALSE,
  recursive = TRUE
)
writeLines(
  jsonlite::toJSON(gt, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE),
  "validation/cache/ground_truth/production_psd.json"
)
cat(sprintf("Wrote %d PSD production GT rows.\n", nrow(gt)))
print(gt[area_iso3 %in% c("USA", "CHN", "IND") & year == 2000L])
