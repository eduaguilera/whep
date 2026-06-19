# redistribute_feed-based feed-intake path (migration in progress).
#
# Plan: plans/2026-06-19-wire-redistribute-feed-migration. Three engines:
#   Engine 1 DEMAND   = IPCC Tier-2 energy (6 covered species) + Krausmann
#                       per-head (non-IPCC species).
#   Engine 2 MIX      = Bouwman dm_share (within-species feed_type split).
#   Engine 3 ALLOCATE = redistribute_feed() at 0.5-degree cell grain
#                       (territory = country, sub_territory = cell), with the
#                       grass ceiling/deficit cascade + the distance-decay
#                       feed-access buffer on the roughage trade.
#
# Built incrementally; until the engines land this dispatcher errors so the
# package default (grain = "national", demand_tier = "fcr") keeps working.

.build_feed_intake_redistribute <- function(grain, demand_tier) {
  cli::cli_abort(c(
    "The {.val redistribute} feed-intake path is not yet implemented.",
    i = "Migration in progress: grain = {.val {grain}}, demand_tier =
      {.val {demand_tier}}.",
    i = "Use the default {.code grain = \"national\", demand_tier = \"fcr\"}
      for the current allocator."
  ))
}

# Curated live_anim_code -> livestock_category crosswalk: maps each live animal
# (animals_codes.csv) to its feed livestock_category (max_intake_share.csv),
# marks whether DEMAND comes from the IPCC Tier-2 energy model (the 6 covered
# species) or Krausmann per-head, and names the energy species each IPCC row
# draws maintenance from. Built + adversarially verified against the three
# source code systems.
.livestock_crosswalk <- function() {
  system.file(
    "extdata",
    "feed",
    "livestock_category_crosswalk.csv",
    package = "whep"
  ) |>
    data.table::fread(na.strings = "") |>
    tibble::as_tibble()
}
