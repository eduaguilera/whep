# Most of the crosswalk cannot be returned by add_polity_code(), and that is by design — but the
# proportion is large enough that "the crosswalk maps area X to polity Y" is not evidence any
# query yields Y.
#
# Measured: of 599 area-polity pairs, 274 are unreachable for every year 1961-2024. 273 of those
# belong to polities ending at or before 1961, which the `backcast_anchor = 1961L` clamp makes
# unselectable — the crosswalk carries whole polity chains while FAOSTAT-sourced resolution can
# only deliver each chain's 1961-onward part.
#
# The single exception is the interesting one. BLX-1921-1999 spans 1961 and is still never
# selected, because area 15 also maps to the overlapping BLX-1850-1999 and the resolver always
# picks that. It is not merely unused by the data — the alias map records 156,557 observed rows for
# BLX-1850-1999 and none for the twin — it is unselectable, so retiring it would change no output.
# That is the reachability half of the evidence on whep-polities#40.

testthat::test_that("unreachable crosswalk pairs are all explained", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[
    which(
      !is.na(cw$area_code) &
        !is.na(cw$polity_code) &
        !is.na(cw$polity_start_year) &
        !is.na(cw$polity_end_year)
    ),
  ]
  pairs <- unique(cw[, c("area_code", "polity_code", "polity_end_year")])
  testthat::expect_gt(nrow(pairs), 500L)

  grid <- expand.grid(
    area_code = sort(unique(cw$area_code)),
    year = 1961:2024
  )
  got <- as.data.frame(add_polity_code(grid))
  reachable <- unique(paste(got$area_code, got$polity_code))

  pairs$reachable <- paste(pairs$area_code, pairs$polity_code) %in% reachable
  unreachable <- pairs[!pairs$reachable, ]

  # Every unreachable pair must be explained: either its polity ended by the clamp year, or it is
  # the one known overlapping twin. An unreachable pair for any OTHER reason would mean a mapping
  # the resolver cannot use and nobody intended.
  unexplained <- unreachable[
    unreachable$polity_end_year > 1961L &
      unreachable$polity_code != "BLX-1921-1999",
  ]
  testthat::expect_equal(
    nrow(unexplained),
    0L,
    info = paste0(
      "these crosswalk mappings can never be returned, for no known reason: ",
      paste(
        utils::head(
          paste0(unexplained$area_code, "->", unexplained$polity_code),
          6
        ),
        collapse = ", "
      )
    )
  )

  # Bidirectional on the twin: if retiring it upstream makes area 15 resolve differently, this
  # fails and the explanation above needs revisiting.
  testthat::expect_true("BLX-1921-1999" %in% unreachable$polity_code)
})
