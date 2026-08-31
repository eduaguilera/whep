# Builds the SJOS-N package data (Module 0):
#   n_boundary_params      - planetary reactive-nitrogen boundary parameters
#                            (Tg N/yr boundary range, per-capita cap,
#                            synthetic-to-total agricultural ratio, food share
#                            of agricultural N).
#   nourishment_thresholds - protein/energy floors + ceilings, the 1.35 waste-
#                            inequality factor, the normalised-score class
#                            cutoffs, and a provenance label per row.
#   sjos_levels            - the 2-way boundary axis crossed with the 3-way
#                            nourishment axis, plus plotting colours.
#   nourish_levels         - nourishment classification levels + colours.
#
# Constants transcribed from plans/2026-07-10-sjos-nitrogen.md (Module 0).
# Level labels and colours are ported BY VALUE from afsetools::load_vectors()
# (SJOS_levels/SJOS_colours and Nour_levels/Nourish_colours); the values are
# copied here so the package carries no afsetools dependency. Colours are R
# colour names, not hex. See R/datasets_sjos_n.R for the @source records.

n_boundary_params <- tibble::tribble(
  ~parameter,
  ~value,
  ~unit,
  ~description,
  "boundary_low",
  60,
  "Tg N/yr",
  "Lower estimate of the planetary boundary for anthropogenic reactive N.",
  "boundary_high",
  125,
  "Tg N/yr",
  "Upper estimate of the planetary boundary for anthropogenic reactive N.",
  "boundary_top",
  205,
  "Tg N/yr",
  "Top reactive-N boundary estimate bounding the SJOS-N boundary range.",
  "per_capita_cap",
  40,
  "kg N/cap/yr",
  "Per-capita anthropogenic reactive-nitrogen cap.",
  "syn_tot_agri_ratio",
  (109 + 33) / (0.85 * 109),
  "ratio",
  "Scaling from synthetic to total agricultural reactive N.",
  "food_agri_share",
  0.95,
  "fraction",
  "Food share of agricultural reactive nitrogen."
)

# `bound` says "ceiling", not "target": normalize_nourishment() uses the upper
# value as the top of the ADEQUATE band, above which a country is classified
# Over. Calling it a target read as something to aim at, which is the opposite
# of what the axis does with it (whep#753).
#
# `provenance` exists so no shipped number can look sourced when it is not.
# Only the protein floor has a citation; 63, 2300, 2900 and 1.35 are inherited
# from the Global SJOS-N analysis with no source anyone has been able to
# produce, and the author has confirmed 1.35 was a preliminary presentation
# figure (whep#753). They are labelled rather than removed because they are
# still what the shipped axis uses.
nourishment_thresholds <- tibble::tribble(
  ~metric,
  ~bound,
  ~value,
  ~unit,
  ~provenance,
  "protein_raw",
  "floor",
  46,
  "g/cap/day",
  "trs935_table46_55kg_safe_level",
  "protein_raw",
  "ceiling",
  63,
  "g/cap/day",
  "inherited_unsourced",
  "protein",
  "floor",
  46 * 1.35,
  "g/cap/day",
  "derived_raw_times_waste_inequality",
  "protein",
  "ceiling",
  63 * 1.35,
  "g/cap/day",
  "derived_raw_times_waste_inequality",
  "energy",
  "floor",
  2300,
  "kcal/cap/day",
  "inherited_unsourced",
  "energy",
  "ceiling",
  2900,
  "kcal/cap/day",
  "inherited_unsourced",
  "waste_inequality",
  "factor",
  1.35,
  "ratio",
  "inherited_unsourced",
  "class",
  "under",
  1,
  "score",
  "definition",
  "class",
  "over",
  2,
  "score",
  "definition"
)

# Boundary axis (Within_boundary/Exceedance) crossed with the nourishment axis
# (Under/Adequate/Over); colours from afsetools SJOS_colours. The order is the
# realised afsetools SJOS_levels order: load_vectors.R:684 wraps the vector in
# rev(), so element 1 is "Exceedance Over", and that is the factor-level order
# Global's figures stack and legend on (Global/R/SJOS_N_figs.R:188).
sjos_levels <- tibble::tribble(
  ~level,
  ~order,
  ~colour,
  "Exceedance Over",
  1L,
  "indianred3",
  "Exceedance Adequate",
  2L,
  "salmon1",
  "Exceedance Under",
  3L,
  "mediumpurple",
  "Within_boundary Over",
  4L,
  "burlywood3",
  "Within_boundary Adequate",
  5L,
  "lightgreen",
  "Within_boundary Under",
  6L,
  "lightseagreen"
)

# Nourishment levels in the afsetools Nour_levels order (Over/Adequate/Under);
# colours from afsetools Nourish_colours.
nourish_levels <- tibble::tribble(
  ~level,
  ~order,
  ~colour,
  "Over",
  1L,
  "red",
  "Adequate",
  2L,
  "green",
  "Under",
  3L,
  "blue"
)

usethis::use_data(
  n_boundary_params,
  nourishment_thresholds,
  sjos_levels,
  nourish_levels,
  overwrite = TRUE
)
