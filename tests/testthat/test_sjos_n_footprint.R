# A minimal 2-region x 2-item IO fixture, hand-built in the shape of
# build_io_model() output (build_io_model has no example = TRUE; this follows the
# build_footprint() docstring example and io_model.R's list-column contract).
# Z is zero (no intermediate flows), so the Leontief inverse is the identity and
# every footprint flow is hand-checkable: X = rowSums(Y). Area 1 exports part of
# its item-10 output to area 2's food demand (Y[1, 3] = 40), the only cross-area
# flow; everything else is consumed domestically.
.sjos_fp_io <- function() {
  labels <- tibble::tibble(
    index = 1:4,
    area_code = c(1L, 1L, 2L, 2L),
    item_cbs_code = c(10L, 20L, 10L, 20L)
  )
  fd_labels <- tibble::tibble(
    area_code = c(1L, 1L, 2L, 2L),
    fd_col = c("food", "other_uses", "food", "other_uses")
  )
  # Columns (fd): area1-food, area1-other, area2-food, area2-other.
  # Rows (sectors): area1-item10, area1-item20, area2-item10, area2-item20.
  y_mat <- matrix(
    c(
      60,
      0,
      0,
      0,
      0,
      50,
      0,
      0,
      40,
      0,
      80,
      30,
      0,
      0,
      0,
      0
    ),
    nrow = 4,
    ncol = 4
  )
  tibble::tibble(
    year = 2000L,
    Z = list(matrix(0, nrow = 4, ncol = 4)),
    X = list(c(100, 50, 80, 30)),
    Y = list(y_mat),
    labels = list(labels),
    fd_labels = list(fd_labels)
  )
}

# The matching per-crop exceedance input: impact_u for the "exceedance" category
# is exceedance_n_t, giving per-sector nitrogen (100, 20, 40, 15) t N in label
# order, so the aligned extension totals 175 t N.
.sjos_fp_exceedance <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~exceedance_n_t,
    ~within_boundary_n_t,
    ~actual_n_t,
    ~production_n_t,
    2000L, 1L, 10L, 100, 40, 140, 150,
    2000L, 1L, 20L, 20, 10, 30, 35,
    2000L, 2L, 10L, 40, 20, 60, 70,
    2000L, 2L, 20L, 15, 5, 20, 25
  )
}

.sjos_fp_run <- function(category = "exceedance") {
  suppressMessages(
    whep::build_sjos_n_footprint(
      .sjos_fp_exceedance(),
      io = .sjos_fp_io(),
      category = category
    )
  )
}

testthat::test_that("the domestic vs traded split follows origin vs target", {
  out <- .sjos_fp_run()
  testthat::expect_setequal(
    out$fp_all$origin,
    c("Domestic consumption", "Traded")
  )
  traded <- dplyr::filter(out$fp_all, origin == "Traded")
  # The only cross-area flow: area 1's item-10 nitrogen consumed in area 2.
  testthat::expect_equal(nrow(traded), 1L)
  testthat::expect_equal(traded$target_area, 2L)
  testthat::expect_equal(traded$item_cbs_code, 10L)
  testthat::expect_equal(traded$impact_u, 40)
  domestic <- out$fp_all |>
    dplyr::filter(origin == "Domestic consumption") |>
    dplyr::pull(impact_u) |>
    sum()
  testthat::expect_equal(domestic, 135)
})

testthat::test_that("fp_food is the target_fd == food subset of fp_all", {
  out <- .sjos_fp_run()
  # The one non-food flow is area 1's item-20 other-uses consumption (20 t N).
  testthat::expect_equal(sum(out$fp_all$impact_u), 175)
  testthat::expect_equal(sum(out$fp_food$impact_u), 155)
  testthat::expect_false(
    any(out$fp_food$target_area == 1L & out$fp_food$item_cbs_code == 20L)
  )
})

testthat::test_that("per-item_cbs granularity is retained", {
  out <- .sjos_fp_run()
  testthat::expect_setequal(out$fp_all$item_cbs_code, c(10L, 20L))
  by_item <- out$fp_all |>
    dplyr::summarise(total = sum(impact_u), .by = item_cbs_code)
  testthat::expect_equal(
    by_item$total[by_item$item_cbs_code == 10L],
    140
  )
  testthat::expect_equal(
    by_item$total[by_item$item_cbs_code == 20L],
    35
  )
})

testthat::test_that("total embodied N conserves the extension total", {
  out <- .sjos_fp_run()
  extension_total <- whep::build_n_exceedance_extension(
    .sjos_fp_exceedance(),
    category = "exceedance"
  ) |>
    dplyr::pull(impact_u) |>
    sum()
  testthat::expect_equal(sum(out$fp_all$impact_u), extension_total)
})

testthat::test_that("signed crop attribution is traced linearly and conserves", {
  signed <- .sjos_fp_exceedance() |>
    dplyr::mutate(
      exceedance_n_t = dplyr::if_else(
        .data$area_code == 1L & .data$item_cbs_code == 20L,
        -20,
        .data$exceedance_n_t
      ),
      attribution_method = "signed_crop_surplus_share"
    )
  out <- suppressMessages(
    whep::build_sjos_n_footprint(
      signed,
      io = .sjos_fp_io(),
      category = "exceedance"
    )
  )
  extension_total <- sum(signed$exceedance_n_t)
  testthat::expect_equal(sum(out$fp_all$impact_u), extension_total)
  testthat::expect_true(any(out$fp_all$impact_u < 0))
})

testthat::test_that("the category stamp is carried and dispatched", {
  out <- .sjos_fp_run(category = "within_boundary")
  testthat::expect_true(all(out$fp_all$category == "within_boundary"))
  testthat::expect_true(all(out$fp_food$category == "within_boundary"))
  # within_boundary_n_t sums to 40 + 10 + 20 + 5 = 75 t N.
  testthat::expect_equal(sum(out$fp_all$impact_u), 75)
})

testthat::test_that("an unknown category is rejected", {
  testthat::expect_error(
    whep::build_sjos_n_footprint(
      .sjos_fp_exceedance(),
      io = .sjos_fp_io(),
      category = "surplus"
    )
  )
})

testthat::test_that("data$fp_flows injects pre-traced flows", {
  flows <- tibble::tribble(
    ~year,
    ~origin_area,
    ~origin_item,
    ~target_area,
    ~target_item,
    ~target_fd,
    ~value,
    2000L, 1L, 10L, 1L, 10L, "food", 60,
    2000L, 1L, 10L, 2L, 10L, "food", 40,
    2000L, 2L, 20L, 2L, 20L, "other_uses", 15
  )
  out <- whep::build_sjos_n_footprint(
    category = "exceedance",
    data = list(fp_flows = flows)
  )
  testthat::expect_equal(sum(out$fp_all$impact_u), 115)
  testthat::expect_equal(sum(out$fp_food$impact_u), 100)
  testthat::expect_setequal(
    out$fp_all$origin,
    c("Domestic consumption", "Traded")
  )
})

testthat::test_that("the example fixture returns fp_all and fp_food", {
  out <- whep::build_sjos_n_footprint(example = TRUE)
  testthat::expect_named(out, c("fp_all", "fp_food"))
  testthat::expect_equal(sum(out$fp_all$impact_u), 175)
  testthat::expect_equal(sum(out$fp_food$impact_u), 155)
  pointblank::expect_col_exists(
    out$fp_all,
    c(
      "year",
      "origin_area",
      "origin_item",
      "target_area",
      "target_fd",
      "origin",
      "item_cbs_code",
      "impact_u",
      "category"
    )
  )
})

testthat::test_that("producer classes are retained on footprint flows", {
  classes <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~nourish,
    ~boundary_side,
    2000L, 1L, 10L, "Adequate", "Exceedance",
    2000L, 1L, 20L, "Adequate", "Exceedance",
    2000L, 2L, 10L, "Under", "Within_boundary",
    2000L, 2L, 20L, "Under", "Exceedance"
  )
  out <- suppressMessages(
    whep::build_sjos_n_footprint(
      .sjos_fp_exceedance(),
      io = .sjos_fp_io(),
      data = list(origin_classes = classes)
    )
  )
  traded <- dplyr::filter(out$fp_all, .data$origin == "Traded")
  testthat::expect_equal(traded$origin_area, 1L)
  testthat::expect_equal(traded$origin_item, 10L)
  testthat::expect_equal(traded$nourish, "Adequate")
  testthat::expect_equal(traded$boundary_side, "Exceedance")
})

# Consumer-side country-year classes for the two fixture areas. Area 2 is the
# importer of the one traded flow (area 1's item 10).
.sjos_fp_target_classes <- function() {
  tibble::tribble(
    ~year, ~area_code, ~nourish,
    2000L, 1L, "Adequate",
    2000L, 2L, "Under"
  )
}

.sjos_fp_run_target <- function(target_classes, origin_classes = NULL) {
  suppressMessages(
    whep::build_sjos_n_footprint(
      .sjos_fp_exceedance(),
      io = .sjos_fp_io(),
      data = purrr::compact(list(
        origin_classes = origin_classes,
        target_classes = target_classes
      ))
    )
  )
}

testthat::test_that("consumer nourishment classes join on target_area and year", {
  out <- .sjos_fp_run_target(.sjos_fp_target_classes())
  testthat::expect_named(out, c("fp_all", "fp_food", "target_class_diag"))
  traded <- dplyr::filter(out$fp_all, .data$origin == "Traded")
  testthat::expect_equal(traded$origin_area, 1L)
  testthat::expect_equal(traded$target_area, 2L)
  testthat::expect_equal(traded$target_nourish, "Under")
  expected <- c(`1` = "Adequate", `2` = "Under")
  testthat::expect_equal(
    out$fp_all$target_nourish,
    unname(expected[as.character(out$fp_all$target_area)])
  )
  testthat::expect_false(anyNA(out$fp_all$target_nourish))
  testthat::expect_equal(sum(out$target_class_diag$n_flows_unclassified), 0L)
})

testthat::test_that("the consumer cross-tab sums to the footprint total", {
  base <- .sjos_fp_run()
  out <- .sjos_fp_run_target(.sjos_fp_target_classes())
  for (tbl in c("fp_all", "fp_food")) {
    testthat::expect_equal(nrow(out[[tbl]]), nrow(base[[tbl]]))
    testthat::expect_equal(sum(out[[tbl]]$impact_u), sum(base[[tbl]]$impact_u))
    for (key in c("origin_area", "target_area", "origin")) {
      crosstab <- out[[tbl]] |>
        dplyr::summarise(
          impact_u = sum(.data$impact_u),
          .by = dplyr::all_of(c(key, "target_nourish"))
        ) |>
        dplyr::summarise(
          impact_u = sum(.data$impact_u),
          .by = dplyr::all_of(key)
        )
      total <- base[[tbl]] |>
        dplyr::summarise(
          impact_u = sum(.data$impact_u),
          .by = dplyr::all_of(key)
        )
      testthat::expect_equal(
        dplyr::arrange(crosstab, .data[[key]]),
        dplyr::arrange(total, .data[[key]])
      )
    }
  }
  # Area 2 consumes 40 t traded from area 1 plus 55 t of its own.
  by_band <- out$fp_all |>
    dplyr::summarise(impact_u = sum(.data$impact_u), .by = "target_nourish")
  testthat::expect_equal(
    by_band$impact_u[by_band$target_nourish == "Under"],
    95
  )
  testthat::expect_equal(
    by_band$impact_u[by_band$target_nourish == "Adequate"],
    80
  )
})

testthat::test_that("producer-side output is unchanged without target_classes", {
  origin <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~nourish, ~boundary_side,
    2000L, 1L, 10L, "Adequate", "Exceedance",
    2000L, 1L, 20L, "Adequate", "Exceedance",
    2000L, 2L, 10L, "Under", "Within_boundary",
    2000L, 2L, 20L, "Under", "Exceedance"
  )
  without <- .sjos_fp_run_target(NULL, origin_classes = origin)
  with <- .sjos_fp_run_target(
    .sjos_fp_target_classes(),
    origin_classes = origin
  )
  testthat::expect_named(without, c("fp_all", "fp_food"))
  for (tbl in c("fp_all", "fp_food")) {
    testthat::expect_false(any(startsWith(names(without[[tbl]]), "target_n")))
    testthat::expect_identical(
      dplyr::select(with[[tbl]], -"target_nourish"),
      without[[tbl]]
    )
  }
  # The pre-change golden: the example fixture is the no-classes output.
  plain <- .sjos_fp_run()
  golden <- whep::build_sjos_n_footprint(example = TRUE)
  for (tbl in c("fp_all", "fp_food")) {
    testthat::expect_equal(
      dplyr::arrange(
        plain[[tbl]],
        .data$origin_area,
        .data$origin_item,
        .data$target_area,
        .data$target_fd
      ),
      dplyr::arrange(
        golden[[tbl]],
        .data$origin_area,
        .data$origin_item,
        .data$target_area,
        .data$target_fd
      ) |>
        dplyr::select(dplyr::all_of(names(plain[[tbl]])))
    )
  }
})

testthat::test_that("unclassified consumer country-years stay NA and are reported", {
  partial <- tibble::tribble(
    ~year, ~area_code, ~nourish,
    2000L, 1L, "Adequate"
  )
  testthat::expect_warning(
    out <- .sjos_fp_run_target(partial),
    class = "whep_sjos_fp_unclassified_target"
  )
  base <- .sjos_fp_run()
  testthat::expect_equal(nrow(out$fp_all), nrow(base$fp_all))
  testthat::expect_equal(sum(out$fp_all$impact_u), 175)
  to_area_2 <- dplyr::filter(out$fp_all, .data$target_area == 2L)
  testthat::expect_true(all(is.na(to_area_2$target_nourish)))
  diag <- out$target_class_diag
  testthat::expect_setequal(diag$table, c("fp_all", "fp_food"))
  all_row <- dplyr::filter(diag, .data$table == "fp_all")
  # Flows into area 2: 40 t traded from area 1, 40 + 15 t domestic.
  testthat::expect_equal(all_row$n_flows, 5L)
  testthat::expect_equal(all_row$n_flows_unclassified, 3L)
  testthat::expect_equal(all_row$n_target_areas, 2L)
  testthat::expect_equal(all_row$n_target_areas_unclassified, 1L)
  testthat::expect_equal(all_row$impact_u, 175)
  testthat::expect_equal(all_row$impact_u_unclassified, 95)
  food_row <- dplyr::filter(diag, .data$table == "fp_food")
  testthat::expect_equal(food_row$impact_u, 155)
  testthat::expect_equal(food_row$impact_u_unclassified, 95)

  # A country-year present with an NA class counts the same as an absent one.
  na_class <- dplyr::bind_rows(
    partial,
    tibble::tibble(year = 2000L, area_code = 2L, nourish = NA_character_)
  )
  testthat::expect_warning(
    out_na <- .sjos_fp_run_target(na_class),
    class = "whep_sjos_fp_unclassified_target"
  )
  testthat::expect_equal(out_na$target_class_diag, diag)
})

testthat::test_that("a country-year joint class is carried when supplied", {
  joint <- .sjos_fp_target_classes() |>
    dplyr::mutate(
      boundary_side = c("Exceedance", "Within_boundary"),
      sjos_class = factor(
        paste(.data$boundary_side, .data$nourish),
        levels = whep::sjos_levels$level
      )
    )
  out <- .sjos_fp_run_target(joint)
  traded <- dplyr::filter(out$fp_all, .data$origin == "Traded")
  testthat::expect_equal(traded$target_boundary_side, "Within_boundary")
  testthat::expect_equal(
    as.character(traded$target_sjos_class),
    "Within_boundary Under"
  )
  testthat::expect_s3_class(out$fp_all$target_sjos_class, "factor")
})

testthat::test_that("a crop-level consumer table is refused, not duplicated", {
  crop_level <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~nourish, ~boundary_side,
    2000L, 1L, 10L, "Adequate", "Exceedance",
    2000L, 1L, 20L, "Adequate", "Within_boundary",
    2000L, 2L, 10L, "Under", "Within_boundary"
  )
  testthat::expect_error(
    .sjos_fp_run_target(crop_level),
    "one class per country-year"
  )
  # The same table reduced to nourish is one class per country-year and joins.
  out <- .sjos_fp_run_target(
    dplyr::select(crop_level, "year", "area_code", "nourish")
  )
  testthat::expect_false(anyNA(out$fp_all$target_nourish))
})

testthat::test_that("target_classes without nourish aborts", {
  testthat::expect_error(
    .sjos_fp_run_target(dplyr::select(.sjos_fp_target_classes(), -"nourish")),
    "nourish"
  )
})
