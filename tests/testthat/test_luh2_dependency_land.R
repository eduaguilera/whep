# Crown dependencies and overseas territories in LUH2 (#407).
#
# LUH2 reports land for JEY, GGY, IMN, ALA, BLM and SXM under their own ISO3.
# The crosswalk knows all six and names each one's sovereign polity, but gives
# them no FAOSTAT `area_code`, and `.current_area_lookup()` keeps only rows that
# have one. So `.read_land_areas()` could not see them and dropped their land
# under a warning that said they were "not found in polity_area_crosswalk" --
# which was the wrong diagnosis: the polity mapping exists, the numeric
# aggregation bucket does not. Measured on the shipped pin, 1850-2022, the six
# are 18.8 Mha-years of 2,409,085, i.e. 0.0008% of LUH2 land.
#
# Whether that land belongs to the sovereign is a series-definition decision
# (folding Jersey into `GBR` changes what `GBR` means in a land series), so the
# code now carries both answers and `dependency_land` picks one. These tests pin
# the plumbing on both sides of the switch: that the crosswalk can name a
# sovereign for every bucket-less ISO3, and that attribution is additive and
# collapses onto a single row per bucket rather than stacking a second one.

luh2_dependency_fixture <- function() {
  # Shaped like the `luh2-areas` pin: ISO3/Year/Land_Use/Area_Mha/C_stock_Tg.
  # `-99` is LUH2's own unassigned-land marker, kept here because it must stay
  # dropped whichever way the dependency switch is set.
  data.table::as.data.table(
    tibble::tribble(
      ~ISO3, ~Year, ~Land_Use, ~Area_Mha, ~C_stock_Tg,
      "GBR", 2000L, "c3ann", 4, 40,
      "GBR", 2000L, "pastr", 10, 100,
      "JEY", 2000L, "c3ann", 0.005, 0.05,
      "GGY", 2000L, "pastr", 0.003, 0.03,
      "-99", 2000L, "primf", 7, 70
    )
  )
}

test_that(".dependency_sovereign_iso3 names a sovereign for bucket-less ISO3s", {
  bridge <- whep:::.dependency_sovereign_iso3()

  # These are the six the crosswalk carries with a polity but no `area_code`.
  # Asserted by name rather than counted, so that a crosswalk refresh which
  # gives one of them its own bucket (option 3 in #407) shows up as a failure
  # here instead of silently changing what a land series contains.
  #
  # SXM reaches NLD by a different route than the other five since the #890
  # resync. It now has its own polity, `SXM-2010-2025`, which carries no
  # reporting area -- so there is no sovereign to read off the polity merge, and
  # `.dependency_sovereign_iso3()` falls back to the `legacy_polity_prefix` the
  # crosswalk row already names. Same answer, and this assertion is what proves
  # the fallback restored it: without it SXM leaves the bridge and the 3,876
  # SXM rows in `luh2-areas` are dropped as a territory with no area code.
  expect_equal(
    bridge[iso3c %in% c("JEY", "GGY", "IMN")]$sovereign_iso3c,
    rep("GBR", 3L)
  )
  expect_equal(bridge[iso3c == "ALA"]$sovereign_iso3c, "FIN")
  expect_equal(bridge[iso3c == "BLM"]$sovereign_iso3c, "FRA")
  expect_equal(bridge[iso3c == "SXM"]$sovereign_iso3c, "NLD")

  # A self-map would silently do nothing, and a dependency that already had a
  # bucket would be double-counted by the ordinary bridge as well as this one.
  expect_false(any(bridge$iso3c == bridge$sovereign_iso3c))
  with_code <- whep:::.current_area_lookup(include_unmapped = FALSE)$area_iso3c
  expect_length(intersect(bridge$iso3c, with_code), 0L)
})

# `.read_land_areas()` renames the pin's `ISO3`/`Year` before it attributes, so
# the direct-call tests below have to hand over the renamed shape.
luh2_dependency_renamed <- function() {
  dt <- luh2_dependency_fixture()
  data.table::setnames(dt, c("ISO3", "Year"), c("iso3c", "year"))
  dt
}

test_that(".attribute_dependency_land sums a dependency into its sovereign", {
  known <- whep:::.current_area_lookup(include_unmapped = FALSE)$area_iso3c
  out <- whep:::.attribute_dependency_land(
    luh2_dependency_renamed(),
    known_iso3 = known
  )

  expect_false(any(c("JEY", "GGY") %in% out$iso3c))
  expect_equal(out[iso3c == "GBR" & Land_Use == "c3ann"]$Area_Mha, 4.005)
  expect_equal(out[iso3c == "GBR" & Land_Use == "pastr"]$Area_Mha, 10.003)
  # Carbon has to travel with the area; the same rows carry both.
  expect_equal(out[iso3c == "GBR" & Land_Use == "c3ann"]$C_stock_Tg, 40.05)

  # One row per (ISO3, Year, Land_Use), not the sovereign's row plus a second
  # dependency row: `.fix_luh2_crop_collapse()` assigns an interpolated value
  # into every row matching a bucket/year/land-use key, so a stacked second row
  # would be repaired to twice the intended area.
  expect_equal(nrow(out[iso3c == "GBR"]), 2L)

  # LUH2's own unassigned marker is not a territory and must be untouched.
  expect_equal(out[iso3c == "-99"]$Area_Mha, 7)
})

test_that(".attribute_dependency_land leaves unresolvable sovereigns alone", {
  # If the sovereign itself is missing from the area bridge, relabelling would
  # move the loss rather than remove it, and would report the drop under the
  # sovereign's ISO3, which reads as a far bigger gap than it is.
  fixture <- luh2_dependency_renamed()
  out <- whep:::.attribute_dependency_land(
    luh2_dependency_renamed(),
    known_iso3 = c("ESP", "FRA")
  )

  expect_equal(sort(unique(out$iso3c)), sort(unique(fixture$iso3c)))
  expect_equal(nrow(out), nrow(fixture))
})

test_that(".read_land_areas honours the dependency_land switch", {
  # Driven off a fixture rather than the pinned LUH2 file: the pin is 515k rows
  # and the point being tested is which ISO3s survive the bridge, not the data.
  local_mocked_bindings(
    .read_input = function(name, years = NULL, year_col = NULL, ...) {
      luh2_dependency_fixture()
    }
  )
  gbr_code <- whep:::.current_area_lookup(include_unmapped = FALSE)[
    area_iso3c == "GBR",
    polity_area_code
  ]

  dropped <- character()
  default <- withCallingHandlers(
    whep:::.read_land_areas(years = 2000L),
    warning = function(w) {
      dropped <<- c(dropped, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Before the switch existed this was the only behaviour: Jersey and Guernsey
  # were reported as dropped alongside the sentinel, and their 0.008 Mha never
  # reached any polity.
  expect_true(any(grepl("JEY", dropped)))
  expect_equal(sum(default$Area_Mha), 14)
  expect_equal(unique(default$area_code), gbr_code)

  attributed <- withCallingHandlers(
    whep:::.read_land_areas(years = 2000L, dependency_land = "sovereign"),
    warning = function(w) {
      dropped <<- c(dropped, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Attribution is additive because LUH2 partitions land: summing every ISO3
  # and land use for 2000 on the real pin gives 13,589 Mha, global ice-free
  # land, so `GBR` excludes Jersey by construction and adding it cannot
  # double-count.
  expect_equal(sum(attributed$Area_Mha), 14.008)
  expect_equal(unique(attributed$area_code), gbr_code)
  expect_equal(nrow(attributed), 2L)
  # The dependency rows take the sovereign's label as well as its bucket,
  # otherwise one bucket would carry two `area` names and split every
  # downstream group-by.
  expect_length(unique(attributed$area), 1L)
})
