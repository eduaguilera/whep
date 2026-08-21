# The reporting areas whose `polity_area_code` the shipped
# `polity_area_crosswalk` and the crosswalk the pipeline resolves through
# (`.polity_crosswalk()`, the one place `.unfold_rest_of_world()` is applied)
# disagree about.
#
# Under the whep#628 default (`whep.unfold_rest_of_world = "all"`) these are the
# Rest-of-World members promoted to their own bucket: the shipped object still
# says 999, the pipeline says the area's own code. Any helper building a
# `polity_area_code` bridge from the shipped object sums their data into a
# bucket nothing downstream keys on (whep#716), so a bridge is correct when it
# agrees with `.polity_crosswalk()` on this set.
.promoted_row_members <- function() {
  bucket <- function(cw, key) {
    cw <- as.data.frame(cw)
    cw <- cw[!is.na(cw[[key]]) & !is.na(cw$polity_area_code), ]
    cw <- cw[!duplicated(cw[[key]]), ]
    stats::setNames(as.integer(cw$polity_area_code), as.character(cw[[key]]))
  }
  shipped <- bucket(whep::polity_area_crosswalk, "area_code")
  pipeline <- bucket(whep:::.polity_crosswalk(), "area_code")
  keys <- intersect(names(shipped), names(pipeline))
  moved <- keys[shipped[keys] != pipeline[keys]]
  tibble::tibble(
    area_code = as.integer(moved),
    shipped_bucket = unname(shipped[moved]),
    area_code_expected = unname(pipeline[moved])
  ) |>
    dplyr::arrange(.data$area_code)
}

# The same set keyed by ISO3, for the bridges that join on `area_iso3c`.
.promoted_row_members_iso3 <- function() {
  cw <- as.data.frame(whep:::.polity_crosswalk())
  cw <- cw[cw$area_code %in% .promoted_row_members()$area_code, ]
  cw <- cw[!is.na(cw$area_iso3c), ]
  cw <- cw[!duplicated(cw$area_iso3c), ]
  tibble::tibble(
    area_iso3c = cw$area_iso3c,
    area_code_expected = as.integer(cw$polity_area_code)
  )
}
