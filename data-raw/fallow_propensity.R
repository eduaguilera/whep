# Build inst/extdata/fallow_propensity.csv: relative propensity (0-1) for a
# crop's RAINFED area to sit in a fallow-rotation system, by agro-climatic ZONE
# (from GAEZ length of growing period + thermal climate; see .aez_zone()).
# Used by gridded_fallow_weights() to allocate FAOSTAT-reported fallow to crops.
#
# Long format: item_cbs_code x zone -> fallow_propensity. EXPERT, TUNABLE values
# grounded in the fallow-geography literature (Siebert et al. 2010; dryland
# cereal-fallow rotations; Gumma et al. 2016 rice-fallow). The irrigated share
# is removed upstream (rainfed-gated), so these only differentiate rainfed crops
# across climates: dryland cereals/pulses peak in arid/semi-arid; rainfed rice
# peaks in the humid tropics (monsoon rice-fallow); perennials ~0 everywhere.

library(dplyr)
library(tidyr)
library(readr)
devtools::load_all(".")

zones <- c("arid", "semiarid", "subhumid", "humid", "tropical_humid")

# class -> propensity by zone (order: arid, semiarid, subhumid, humid, trop_humid)
class_zone <- tibble::tribble(
  ~class, ~arid, ~semiarid, ~subhumid, ~humid, ~tropical_humid,
  "cereal_dryland", 1.00, 0.80, 0.30, 0.05, 0.10,
  "rice", 0.10, 0.20, 0.50, 0.30, 0.90,
  "maize", 0.40, 0.30, 0.10, 0.03, 0.10,
  "pulse", 1.00, 0.80, 0.40, 0.10, 0.30,
  "oilseed", 0.70, 0.60, 0.25, 0.05, 0.15,
  "roots", 0.30, 0.25, 0.15, 0.05, 0.10,
  "minor", 0.10, 0.10, 0.08, 0.03, 0.08,
  "perennial", 0.00, 0.00, 0.00, 0.00, 0.00
)

# fraction of the IRRIGATED area that also counts toward the fallow weight.
# Usually 0 (irrigated = continuously cropped), but rice-fallow is the rabi
# season left fallow after kharif rice whether or not the kharif was irrigated,
# so irrigated rice area in monsoon zones must be eligible for fallow.
class_irrig <- tibble::tribble(
  ~class, ~arid, ~semiarid, ~subhumid, ~humid, ~tropical_humid,
  "rice", 0.0, 0.0, 0.5, 0.3, 1.0
)

class_of <- function(code) {
  dplyr::case_when(
    code %in% c(2511, 2513, 2515, 2516, 2517, 2518, 2520) ~ "cereal_dryland",
    code == 2807 ~ "rice",
    code == 2514 ~ "maize",
    code %in% c(2546, 2547, 2549) ~ "pulse",
    code %in% c(2555, 2557, 2558, 2561, 2552, 2570, 772, 776) ~ "oilseed",
    code %in% c(2531, 2532, 2533, 2534, 2535) ~ "roots",
    code %in%
      c(
        248,
        2560,
        254,
        2562,
        2577,
        310,
        2563,
        2630,
        2633,
        2635,
        2672,
        2000,
        2001,
        2002,
        2003
      ) ~ "perennial",
    TRUE ~ "minor"
  )
}

crops <- whep::items_full |>
  dplyr::filter(group == "Primary crops") |>
  dplyr::distinct(item_cbs_code) |>
  dplyr::mutate(
    item_cbs_code = as.integer(item_cbs_code),
    class = class_of(item_cbs_code)
  )

prop_long <- crops |>
  dplyr::left_join(class_zone, by = "class") |>
  tidyr::pivot_longer(
    dplyr::all_of(zones),
    names_to = "zone",
    values_to = "fallow_propensity"
  )
irrig_long <- crops |>
  dplyr::left_join(class_irrig, by = "class") |>
  tidyr::pivot_longer(
    dplyr::all_of(zones),
    names_to = "zone",
    values_to = "irrig_share"
  ) |>
  dplyr::mutate(irrig_share = tidyr::replace_na(irrig_share, 0))

fallow_propensity <- prop_long |>
  dplyr::left_join(irrig_long, by = c("item_cbs_code", "class", "zone")) |>
  dplyr::transmute(item_cbs_code, zone, fallow_propensity, irrig_share) |>
  dplyr::arrange(item_cbs_code, zone)

write_csv(fallow_propensity, "inst/extdata/fallow_propensity.csv")
