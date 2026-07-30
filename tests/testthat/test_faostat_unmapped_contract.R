# The two facts about which FAOSTAT areas resolve to nothing on purpose are UPSTREAM's, published in the
# manifest as `faostat_unmapped_areas`, and embedded here by data-raw/constants.R because the manifest is
# not a package resource.
#
# They were hardcoded in this package until upstream published them: the 5000 group threshold measured
# against real production — 34 of 34 unmapped codes are >= 5000, a number upstream already knew — and
# "deliberate" inferred from crosswalk membership. Inference cannot distinguish a decision from an
# absence, which is how a warning here came to report FAOSTAT 351 "China" as an area code nobody knows.
#
# Embedding reintroduces the drift risk that copying always does, so this compares the copy against the
# manifest whenever one is reachable, and asserts the embedded values are usable when it is not.

testthat::test_that("the embedded FAOSTAT constants are present and sane", {
  # Runs everywhere, including CI, because it needs nothing outside the package.
  testthat::expect_true(is.numeric(whep:::faostat_group_code_min))
  testthat::expect_length(whep:::faostat_group_code_min, 1L)
  testthat::expect_gt(whep:::faostat_group_code_min, 1000L)

  testthat::expect_true(is.numeric(whep:::faostat_deliberate_area_codes))
  testthat::expect_gte(length(whep:::faostat_deliberate_area_codes), 1L)
  # 351 China is the case the whole mechanism exists for; losing it would be silent.
  testthat::expect_true(351L %in% whep:::faostat_deliberate_area_codes)

  # No modelled area may sit at or above the threshold, or a real territory would be dismissed as an
  # aggregate. Asserted here too, since this is now where the threshold enters the package.
  modelled <- unique(stats::na.omit(
    as.data.frame(whep::polity_area_crosswalk)$area_code
  ))
  testthat::expect_false(any(modelled >= whep:::faostat_group_code_min))
})

testthat::test_that("the embedded constants match the upstream manifest", {
  path <- Sys.getenv(
    "WHEP_POLITIES_MANIFEST",
    unset = path.expand("~/whep-polities/data/final/polities_manifest.json")
  )
  testthat::skip_if_not(
    file.exists(path),
    paste0("upstream manifest not found at ", path)
  )
  manifest <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  published <- manifest$faostat_unmapped_areas
  testthat::skip_if(
    is.null(published$group_code_min),
    "manifest predates faostat_unmapped_areas"
  )

  testthat::expect_equal(
    as.integer(whep:::faostat_group_code_min),
    as.integer(published$group_code_min),
    info = "R/sysdata.rda is stale — rerun data-raw/constants.R"
  )
  testthat::expect_setequal(
    as.integer(whep:::faostat_deliberate_area_codes),
    as.integer(published$deliberate_area_codes)
  )
})

# A THRESHOLD IS A RULE OF THUMB, AND FAOSTAT BREAKS IT. `code >= group_code_min`
# covers the main regional groups -- World 5000, the continents, the income bands -- but
# is not exhaustive: the emissions domains carry aggregates inside the country range.
#
# 420 "Sub-Saharan Africa" is 14,427 rows of faostat-emissions-livestock, 0.6% of that
# pin, and it was classified as "an area code this project does not know" on every real
# build. That is the WARNING bucket, and the point of having three buckets is that this
# one means something is wrong.
#
# Found by running a build, not by reading code, and only visible because the unknown
# bucket warns while the other two inform. The full sweep of that pin: 39 unmapped codes,
# 38 proper groups at or above the threshold, and exactly this one below it.
#
# Published separately from `deliberate_area_codes` because the two are different facts
# that happen to share an effect. 351 China is a DECISION -- it is reported alongside its
# own components, so routing it anywhere would double-count. 420 is simply a group with a
# low code. A consumer cannot tell them apart from the numbers, which is why upstream
# names both.
testthat::test_that("subthreshold FAOSTAT groups are classified as groups, not unknowns", {
  sub_groups <- whep:::faostat_subthreshold_groups
  testthat::expect_true(is.numeric(sub_groups))
  # The full class, enumerated by sweeping nine pins rather than by collecting one per
  # smoke run: 420 is a regional group, and the five "(excluding intra-trade)" codes are
  # multi-territory trade totals. Pinned by identity because the point is WHICH codes are
  # exempt, and a count would let a real country join the list unnoticed.
  #
  #   261 European Union (12) (excluding intra-trade)
  #   265 China (excluding intra-trade)
  #   266 European Union (15) (excluding intra-trade)
  #   268 European Union (25) (excluding intra-trade)
  #   269 European Union (27) (excluding Croatia) (excluding intra-trade)
  #   420 Sub-Saharan Africa
  testthat::expect_setequal(
    sort(as.integer(sub_groups)),
    c(261L, 265L, 266L, 268L, 269L, 420L)
  )

  # Must be below the threshold, or it would need no exception and the list would be
  # silently redundant.
  testthat::expect_true(all(sub_groups < whep:::faostat_group_code_min))

  # And disjoint from the deliberate non-mappings: a code in both would mean upstream
  # is asserting two different reasons for the same drop.
  testthat::expect_equal(
    length(intersect(sub_groups, whep:::faostat_deliberate_area_codes)),
    0L
  )
})

testthat::test_that("the embedded subthreshold list matches the manifest", {
  path <- Sys.getenv(
    "WHEP_POLITIES_MANIFEST",
    unset = path.expand("~/whep-polities/data/final/polities_manifest.json")
  )
  testthat::skip_if_not(
    file.exists(path),
    "upstream manifest unavailable; cannot check the embedded copy for drift"
  )
  mf <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  upstream <- sort(as.integer(
    mf$faostat_unmapped_areas$subthreshold_group_codes
  ))
  testthat::expect_equal(
    sort(as.integer(whep:::faostat_subthreshold_groups)),
    upstream,
    info = "rerun data-raw/constants.R and commit R/sysdata.rda"
  )
})

testthat::test_that("the drop classifier puts a subthreshold group in the group bucket", {
  # Exercised through the real function rather than by reimplementing its arithmetic,
  # since the defect was in which bucket a code lands in, not in the buckets themselves.
  dt <- data.table::data.table(
    area_code = c(420L, 5000L, 351L, 421L, 68L),
    polity_area_code = c(NA, NA, NA, NA, 68L)
  )
  msgs <- character(0)
  warns <- character(0)
  withCallingHandlers(
    whep:::.warn_unmapped_codes(dt, "polity_area_code", "area_code", "test"),
    message = function(cond) {
      msgs <<- c(msgs, conditionMessage(cond))
      invokeRestart("muffleMessage")
    },
    warning = function(cond) {
      warns <<- c(warns, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  all_msgs <- paste(msgs, collapse = " ")
  all_warns <- paste(warns, collapse = " ")

  # 420 must be reported as a group, and NOT as an unknown. Named explicitly in the
  # message because the count alone used to carry the text "(>= 5000)", which stopped
  # being true the moment a below-threshold code joined the bucket.
  testthat::expect_true(grepl("420", all_msgs))
  testthat::expect_false(grepl("420", all_warns))
  testthat::expect_true(grepl("below 5000", all_msgs))
  # 421 is below the threshold and in no upstream list, so it must STILL warn -- the
  # fix had to correct the bucket, not silence it. Chosen deliberately over a code like
  # 999999, which is above the threshold and would have been classed a group by the
  # rule itself, making the assertion vacuous.
  testthat::expect_true(grepl("421", all_warns))
})

# The standing version of the sweep that found these. Two of the six were discovered one
# at a time by running real builds -- 420 in a production build, 265 in a CBS build -- and
# each cost a trace through the call stack to attribute, because neither appears in the
# pins the unit tests cover. Finding the rest that way would have taken one build per code.
#
# So instead: sweep every readable pin for unmapped area codes below the threshold, and
# require that the set is exactly what upstream publishes. A new one fails here, named,
# instead of surfacing as a warning in someone's build months later.
#
# Needs the real pins, so it skips on CI, where they are not available. That skip is
# reported in the local gate's inventory rather than being silent -- an unrun check is
# indistinguishable from a passing one, which cost twelve stale baseline entries earlier
# in this branch.
testthat::test_that("no pin carries an unmapped sub-threshold code outside the published list", {
  testthat::skip_on_ci()
  pins <- c(
    "faostat-production",
    "faostat-production-old",
    "faostat-trade-totals",
    "faostat-emissions-livestock",
    "faostat-cbs-new",
    "faostat-cbs-old-crops",
    "faostat-cbs-old-animal",
    "international-yields",
    "fishstat-trade"
  )
  cw <- as.data.frame(whep::polity_area_crosswalk)
  mapped <- unique(as.integer(stats::na.omit(cw$area_code)))
  known <- c(
    as.integer(whep:::faostat_subthreshold_groups),
    as.integer(whep:::faostat_deliberate_area_codes)
  )
  threshold <- as.integer(whep:::faostat_group_code_min)

  found <- integer(0)
  checked <- 0L
  for (pin in pins) {
    d <- tryCatch(whep:::whep_read_file(pin), error = function(e) NULL)
    if (is.null(d)) {
      next
    }
    nms <- names(d)
    col <- nms[grepl("^(Area Code|AreaCode|area_code)$", nms)][1]
    if (is.na(col)) {
      next
    }
    checked <- checked + 1L
    v <- suppressWarnings(as.integer(d[[col]]))
    v <- unique(v[!is.na(v)])
    found <- c(found, v[!v %in% mapped & v < threshold & !v %in% known])
  }
  testthat::skip_if(checked == 0L, "no pins reachable")
  testthat::expect_equal(
    sort(unique(found)),
    integer(0),
    info = paste0(
      "area codes below the group threshold that no polity claims and upstream does ",
      "not list, so a real build reports them as unknown: ",
      paste(sort(unique(found)), collapse = ", ")
    )
  )
})
