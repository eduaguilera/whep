# Tests for the ranked grassland reclassification engine,
# .classify_grassland_cells(). All fixtures are hand-built; every expected
# class is derived in a comment from the prefix areas.

.gi_ratios <- function(cells, ratio, years = 2000L) {
  tidyr::expand_grid(cell_id = cells$cell_id, year = years) |>
    dplyr::mutate(density_ratio = ratio)
}

.gi_classes <- function(out, yr = 2000L) {
  out |>
    dplyr::filter(.data$year == yr) |>
    dplyr::arrange(.data$cell_id) |>
    dplyr::pull(grassland_class)
}

testthat::test_that("ratio 1 everywhere reproduces the IMAGE 2010 map", {
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       50,
    2L,       10L,           20,        "intensive",       40,
    3L,       10L,           30,        "extensive",       5,
    4L,       10L,           40,        "extensive",       10,
    5L,       20L,           7,         "intensive",       12,
    6L,       20L,           13,        "extensive",       3
  )
  ratios <- .gi_ratios(cells, 1, years = c(1990L, 2010L))

  out <- whep:::.classify_grassland_cells(cells, ratios)

  testthat::expect_equal(nrow(out), 12L)
  testthat::expect_named(
    out,
    c(
      "cell_id",
      "year",
      "country_2010",
      "image_class_2010",
      "grassland_class",
      "density_ratio",
      "target_share",
      "method_grassland_split"
    )
  )
  testthat::expect_equal(out$grassland_class, out$image_class_2010)
  testthat::expect_true(all(
    out$method_grassland_split == "image2010_density_rank"
  ))
  # Country 10: 30 of 100 ha intensive; country 20: 7 of 20 ha.
  shares <- out |>
    dplyr::distinct(.data$country_2010, .data$target_share) |>
    dplyr::arrange(.data$country_2010)
  testthat::expect_equal(shares$target_share, c(0.3, 0.35))
})

testthat::test_that("demotion removes the lowest-manure intensive cells", {
  # Intensive area 30 of 100 -> s(2010) = 0.3. Ratio 0.7 -> T = 21.
  # Keep order by manure desc: id 1 (50), id 3 (35), id 2 (20).
  # Prefix areas 0, 10, 20, 30 -> |A - 21| = 21, 11, 1, 9 -> keep 2 cells.
  # So id 2, the lowest-manure intensive cell, is demoted.
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       50,
    2L,       10L,           10,        "intensive",       20,
    3L,       10L,           10,        "intensive",       35,
    4L,       10L,           70,        "extensive",       5
  )

  out <- whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 0.7))

  testthat::expect_equal(
    .gi_classes(out),
    c("intensive", "extensive", "intensive", "extensive")
  )
  testthat::expect_equal(out$target_share, rep(0.21, 4))
})

testthat::test_that("promotion adds the highest-manure extensive cells", {
  # Intensive 10 of 100 -> s(2010) = 0.1. Ratio 3 -> T = 30.
  # Promotion order by manure desc: id 3 (15), id 4 (8), id 2 (5), id 5 (1).
  # Areas 10 + cumsum = 10, 20, 30, 40, 100 -> exactly 30 at 2 cells.
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       30,
    2L,       10L,           10,        "extensive",       5,
    3L,       10L,           10,        "extensive",       15,
    4L,       10L,           10,        "extensive",       8,
    5L,       10L,           60,        "extensive",       1
  )

  out <- whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 3))

  testthat::expect_equal(
    .gi_classes(out),
    c("intensive", "extensive", "intensive", "intensive", "extensive")
  )
})

testthat::test_that("demotion keeps the prefix nearest to the target", {
  # Intensive 20 of 100 -> s(2010) = 0.2. Ratio 0.6 -> T = 12.
  # Keep order: id 1 (4 ha), id 2 (10 ha), id 3 (6 ha).
  # Prefix areas 0, 4, 14, 20 -> |A - 12| = 12, 8, 2, 8 -> keep 2 cells
  # (14 ha, overshooting T), not the largest area below T (4 ha).
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           4,         "intensive",       50,
    2L,       10L,           10,        "intensive",       40,
    3L,       10L,           6,         "intensive",       30,
    4L,       10L,           80,        "extensive",       2
  )

  out <- whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 0.6))

  testthat::expect_equal(
    .gi_classes(out),
    c("intensive", "intensive", "extensive", "extensive")
  )
})

testthat::test_that("promotion adds the prefix nearest to the target", {
  # Intensive 10 of 100 -> s(2010) = 0.1. Ratio 1.9 -> T = 19.
  # Promotion order: id 2 (4 ha), id 3 (12 ha), id 4 (74 ha).
  # Areas 10, 14, 26, 100 -> |A - 19| = 9, 5, 7, 81 -> promote 1 cell.
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       30,
    2L,       10L,           4,         "extensive",       9,
    3L,       10L,           12,        "extensive",       8,
    4L,       10L,           74,        "extensive",       1
  )

  out <- whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 1.9))

  testthat::expect_equal(
    .gi_classes(out),
    c("intensive", "intensive", "extensive", "extensive")
  )
})

testthat::test_that("demotion ties keep the longer prefix", {
  # Intensive 30 of 100 -> s(2010) = 0.3. Ratio 0.5 -> T = 15.
  # Keep order: id 1 (50), id 2 (40), id 3 (30), 10 ha each.
  # Prefix areas 0, 10, 20, 30 -> |A - 15| = 15, 5, 5, 15 -> tie between
  # 10 and 20; the longer prefix (fewer changes from 2010) wins: keep 2.
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       50,
    2L,       10L,           10,        "intensive",       40,
    3L,       10L,           10,        "intensive",       30,
    4L,       10L,           70,        "extensive",       2
  )

  out <- whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 0.5))

  testthat::expect_equal(
    .gi_classes(out),
    c("intensive", "intensive", "extensive", "extensive")
  )
})

testthat::test_that("promotion ties add the shorter prefix", {
  # Intensive 10 of 100 -> s(2010) = 0.1. Ratio 2.5 -> T = 25.
  # Promotion order: id 2 (9), id 3 (8), id 4 (1).
  # Areas 10, 20, 30, 100 -> |A - 25| = 15, 5, 5, 75 -> tie between 20 and
  # 30; the shorter prefix (fewer changes from 2010) wins: promote 1.
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       30,
    2L,       10L,           10,        "extensive",       9,
    3L,       10L,           10,        "extensive",       8,
    4L,       10L,           70,        "extensive",       1
  )

  out <- whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 2.5))

  testthat::expect_equal(
    .gi_classes(out),
    c("intensive", "intensive", "extensive", "extensive")
  )
})

testthat::test_that("target share is the area-weighted mean of finite ratios", {
  # s(2010) = 10 / 100 = 0.1. Finite ratios: id 1 (10 ha, 2), id 2
  # (30 ha, 0.5); id 3 is NA and drops out of the mean only.
  # m = (10 * 2 + 30 * 0.5) / 40 = 0.875 -> s(t) = 0.0875, T = 8.75.
  # Demotion: prefix areas 0, 10 -> |A - 8.75| = 8.75, 1.25 -> keep id 1.
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       30,
    2L,       10L,           30,        "extensive",       9,
    3L,       10L,           60,        "extensive",       8
  )
  ratios <- tibble::tribble(
    ~cell_id, ~year, ~density_ratio,
    1L,       2000L, 2,
    2L,       2000L, 0.5,
    3L,       2000L, NA
  )

  out <- whep:::.classify_grassland_cells(cells, ratios)

  testthat::expect_equal(out$target_share, rep(0.0875, 3))
  testthat::expect_equal(out$density_ratio, c(2, 0.5, NA))
  testthat::expect_equal(
    .gi_classes(out),
    c("intensive", "extensive", "extensive")
  )
  testthat::expect_true(all(
    out$method_grassland_split == "image2010_density_rank"
  ))
})

testthat::test_that("the target share is clamped to [0, 1]", {
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       50,
    2L,       10L,           20,        "intensive",       40,
    3L,       10L,           30,        "extensive",       5,
    4L,       10L,           40,        "extensive",       10
  )

  # 0.3 * 100 = 30 > 1 -> s = 1 -> T = 100 -> every cell intensive.
  high <- whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 100))
  testthat::expect_equal(high$target_share, rep(1, 4))
  testthat::expect_true(all(high$grassland_class == "intensive"))

  # 0.3 * 0 = 0 -> T = 0 -> the empty prefix -> every cell extensive.
  zero <- whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 0))
  testthat::expect_equal(zero$target_share, rep(0, 4))
  testthat::expect_true(all(zero$grassland_class == "extensive"))
})

testthat::test_that("equal manure ties break on ascending cell_id", {
  # Demotion: 4 intensive cells x 10 ha, manure all 20, plus 60 ha
  # extensive. s(2010) = 0.4, ratio 0.5 -> T = 20 -> keep 2 cells.
  # Keep order on the tie is cell_id ascending: 1, 3, 6, 8 -> keep 1, 3.
  demote <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    8L,       10L,           10,        "intensive",       20,
    3L,       10L,           10,        "intensive",       20,
    6L,       10L,           10,        "intensive",       20,
    1L,       10L,           10,        "intensive",       20,
    9L,       10L,           60,        "extensive",       1
  )
  out_d <- whep:::.classify_grassland_cells(demote, .gi_ratios(demote, 0.5))
  kept <- out_d |>
    dplyr::filter(.data$grassland_class == "intensive") |>
    dplyr::pull(cell_id) |>
    sort()
  testthat::expect_equal(kept, c(1L, 3L))

  # Promotion: 10 ha intensive, three 10-ha extensive cells with manure 5,
  # one 60-ha extensive cell with manure 1. s(2010) = 0.1, ratio 3 ->
  # T = 30 -> promote 2 -> on the tie, cell_id ascending: 2, 7.
  promote <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    4L,       10L,           10,        "intensive",       30,
    9L,       10L,           10,        "extensive",       5,
    2L,       10L,           10,        "extensive",       5,
    7L,       10L,           10,        "extensive",       5,
    11L,      10L,           60,        "extensive",       1
  )
  out_p <- whep:::.classify_grassland_cells(promote, .gi_ratios(promote, 3))
  promoted <- out_p |>
    dplyr::filter(
      .data$grassland_class == "intensive",
      .data$image_class_2010 == "extensive"
    ) |>
    dplyr::pull(cell_id) |>
    sort()
  testthat::expect_equal(promoted, c(2L, 7L))
})

testthat::test_that("an undefined country-year keeps IMAGE classes", {
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       50,
    2L,       10L,           90,        "extensive",       5,
    3L,       20L,           10,        "intensive",       50,
    4L,       20L,           90,        "extensive",       5,
    5L,       30L,           40,        "intensive",       50,
    6L,       30L,           60,        "extensive",       5
  )
  # Country 10: no finite ratio in 2000 (NA and Inf). Country 30: no
  # finite ratio in 1990. Country 20 is defined throughout; ratio 0 ->
  # all extensive, which would also demote countries 10 and 30 were they
  # defined.
  ratios <- tidyr::expand_grid(
    cell_id = 1:6,
    year = c(1990L, 2000L)
  ) |>
    dplyr::mutate(
      density_ratio = dplyr::case_when(
        .data$cell_id == 1L & .data$year == 2000L ~ NA_real_,
        .data$cell_id == 2L & .data$year == 2000L ~ Inf,
        .data$cell_id %in% 5:6 & .data$year == 1990L ~ NaN,
        .default = 0
      )
    )

  testthat::expect_message(
    out <- whep:::.classify_grassland_cells(cells, ratios),
    class = "whep_grassland_no_density"
  )
  msgs <- testthat::capture_messages(
    whep:::.classify_grassland_cells(cells, ratios)
  )
  testthat::expect_length(msgs, 1L)
  testthat::expect_match(msgs, "10")
  testthat::expect_match(msgs, "30")

  undefined <- out |>
    dplyr::filter(
      (.data$country_2010 == 10L & .data$year == 2000L) |
        (.data$country_2010 == 30L & .data$year == 1990L)
    )
  testthat::expect_equal(nrow(undefined), 4L)
  testthat::expect_equal(
    undefined$grassland_class,
    undefined$image_class_2010
  )
  testthat::expect_true(all(is.na(undefined$target_share)))
  testthat::expect_true(all(
    undefined$method_grassland_split == "image2010_fixed_no_density"
  ))

  defined <- dplyr::anti_join(
    out,
    undefined,
    by = c("cell_id", "year")
  )
  testthat::expect_equal(nrow(defined), 8L)
  testthat::expect_true(all(defined$grassland_class == "extensive"))
  testthat::expect_true(all(
    defined$method_grassland_split == "image2010_density_rank"
  ))
})

testthat::test_that("no inform is emitted when every country-year is defined", {
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       50,
    2L,       10L,           90,        "extensive",       5
  )
  testthat::expect_no_message(
    whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 1.2)),
    class = "whep_grassland_no_density"
  )
})

testthat::test_that("an all-extensive country stays extensive", {
  # s(2010) = 0 -> s(t) = 0 whatever the ratio -> no promotion.
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           30,        "extensive",       9,
    2L,       10L,           70,        "extensive",       2
  )

  out <- whep:::.classify_grassland_cells(cells, .gi_ratios(cells, 5))

  testthat::expect_true(all(out$grassland_class == "extensive"))
  testthat::expect_equal(out$target_share, c(0, 0))
})

testthat::test_that("invalid inputs abort", {
  cells <- tibble::tribble(
    ~cell_id, ~country_2010, ~grass_ha, ~image_class_2010, ~manure_n_kg_ha,
    1L,       10L,           10,        "intensive",       50,
    2L,       10L,           90,        "extensive",       5
  )
  ratios <- .gi_ratios(cells, 1)
  classify <- whep:::.classify_grassland_cells

  testthat::expect_error(
    classify(dplyr::select(cells, -"manure_n_kg_ha"), ratios),
    "manure_n_kg_ha"
  )
  testthat::expect_error(
    classify(cells, dplyr::select(ratios, -"density_ratio")),
    "density_ratio"
  )
  testthat::expect_error(
    classify(dplyr::mutate(cells, image_class_2010 = "mixed"), ratios),
    class = "whep_grassland_input"
  )
  testthat::expect_error(
    classify(dplyr::mutate(cells, grass_ha = c(10, 0)), ratios),
    class = "whep_grassland_input"
  )
  testthat::expect_error(
    classify(dplyr::mutate(cells, grass_ha = c(10, NA)), ratios),
    class = "whep_grassland_input"
  )
  testthat::expect_error(
    classify(dplyr::mutate(cells, manure_n_kg_ha = c(NA, 5)), ratios),
    class = "whep_grassland_input"
  )
  testthat::expect_error(
    classify(dplyr::mutate(cells, cell_id = 1L), ratios),
    class = "whep_grassland_input"
  )
  testthat::expect_error(
    classify(cells, dplyr::bind_rows(ratios, ratios[1, ])),
    class = "whep_grassland_input"
  )
  testthat::expect_error(
    classify(cells[1, ], ratios),
    "1 ratio row"
  )
  testthat::expect_error(
    classify(cells, ratios[1, ]),
    class = "whep_grassland_input"
  )
  testthat::expect_error(
    classify(cells, dplyr::mutate(ratios, density_ratio = -1)),
    class = "whep_grassland_input"
  )
})

# Brute-force reference: scan every prefix length for the nearest area,
# with the demotion (longer) and promotion (shorter) tie rules. One
# country, one year, one ratio shared by every cell.
.gi_reference <- function(cells, ratio) {
  int <- cells[cells$image_class_2010 == "intensive", ]
  ext <- cells[cells$image_class_2010 == "extensive", ]
  int <- int[order(-int$manure_n_kg_ha, int$cell_id), ]
  ext <- ext[order(-ext$manure_n_kg_ha, ext$cell_id), ]
  int_ha <- sum(int$grass_ha)
  target <- min(sum(cells$grass_ha), max(0, int_ha * ratio))
  if (target <= int_ha) {
    gap <- abs(c(0, cumsum(int$grass_ha)) - target)
    k <- max(which(gap == min(gap))) - 1
    intensive_ids <- int$cell_id[seq_len(k)]
  } else {
    gap <- abs(int_ha + c(0, cumsum(ext$grass_ha)) - target)
    k <- min(which(gap == min(gap))) - 1
    intensive_ids <- c(int$cell_id, ext$cell_id[seq_len(k)])
  }
  sort(intensive_ids)
}

.gi_random_case <- function(seed) {
  withr::local_seed(seed)
  n <- sample(2:8, 1)
  cells <- tibble::tibble(
    cell_id = sample(1:50, n),
    country_2010 = 1L,
    grass_ha = sample(1:4, n, replace = TRUE),
    image_class_2010 = sample(c("intensive", "extensive"), n, replace = TRUE),
    manure_n_kg_ha = sample(1:3, n, replace = TRUE)
  )
  list(cells = cells, ratio = sample(c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 3), 1))
}

testthat::test_that("the step rule matches a brute-force prefix scan", {
  cases <- purrr::map(1:300, .gi_random_case)
  agree <- purrr::map_lgl(cases, \(case) {
    out <- whep:::.classify_grassland_cells(
      case$cells,
      .gi_ratios(case$cells, case$ratio)
    )
    got <- sort(out$cell_id[out$grassland_class == "intensive"])
    identical(got, .gi_reference(case$cells, case$ratio))
  })
  testthat::expect_true(all(agree))
})

testthat::test_that("non-grazer items and non-LU units are excluded", {
  stock_lu <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit,   ~value,
    2010L, 10L,        961L,            "LU",    50,
    2010L, 10L,        1049L,           "LU",    30,
    2010L, 10L,        961L,            "heads", 500
  )
  grassland_ha <- tibble::tribble(
    ~area_code, ~year, ~grassland_ha,
    10L,        2010L, 100
  )

  out <- whep:::.grazing_density(stock_lu, grassland_ha)

  testthat::expect_named(
    out,
    c("area_code", "year", "grazing_lu", "grassland_ha", "grazing_density")
  )
  testthat::expect_equal(out$grazing_lu, 50)
  testthat::expect_equal(out$grazing_density, 0.5)
})

testthat::test_that("two grazer items sum per country-year", {
  stock_lu <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2010L, 10L,        961L,            "LU",  40,
    2010L, 10L,        976L,            "LU",  10
  )
  grassland_ha <- tibble::tribble(
    ~area_code, ~year, ~grassland_ha,
    10L,        2010L, 50
  )

  out <- whep:::.grazing_density(stock_lu, grassland_ha)

  testthat::expect_equal(out$grazing_lu, 50)
  testthat::expect_equal(out$grazing_density, 1)
})

testthat::test_that("zero grassland gives NA density", {
  stock_lu <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2010L, 10L,        961L,            "LU",  20
  )
  grassland_ha <- tibble::tribble(
    ~area_code, ~year, ~grassland_ha,
    10L,        2010L, 0
  )

  out <- whep:::.grazing_density(stock_lu, grassland_ha)

  testthat::expect_equal(out$grazing_lu, 20)
  testthat::expect_true(is.na(out$grazing_density))
})

testthat::test_that("grassland with no grazer rows gives NA lu and density", {
  stock_lu <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2010L, 20L,        961L,            "LU",  20
  )
  grassland_ha <- tibble::tribble(
    ~area_code, ~year, ~grassland_ha,
    10L,        2010L, 100
  )

  out <- whep:::.grazing_density(stock_lu, grassland_ha)
  row10 <- dplyr::filter(out, .data$area_code == 10L)

  testthat::expect_equal(row10$grassland_ha, 100)
  testthat::expect_true(is.na(row10$grazing_lu))
  testthat::expect_true(is.na(row10$grazing_density))
})

testthat::test_that("grazer rows summing to zero give an undefined density", {
  stock_lu <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2010L, 10L,        961L,            "LU",  0,
    2010L, 10L,        976L,            "LU",  0
  )
  grassland_ha <- tibble::tribble(
    ~area_code, ~year, ~grassland_ha,
    10L,        2010L, 100
  )

  out <- whep:::.grazing_density(stock_lu, grassland_ha)

  testthat::expect_equal(out$grazing_lu, 0)
  testthat::expect_true(is.na(out$grazing_density))
})

testthat::test_that("a duplicated grassland_ha key aborts", {
  stock_lu <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2010L, 10L,        961L,            "LU",  20
  )
  grassland_ha <- tibble::tribble(
    ~area_code, ~year, ~grassland_ha,
    10L,        2010L, 100,
    10L,        2010L, 90
  )

  testthat::expect_error(
    whep:::.grazing_density(stock_lu, grassland_ha),
    class = "whep_grazing_density_input"
  )
})

testthat::test_that(".grazer_item_codes() covers grazers, not others", {
  codes <- whep:::.grazer_item_codes()

  testthat::expect_true(960L %in% codes)
  testthat::expect_true(946L %in% codes)
  testthat::expect_false(1049L %in% codes)
  testthat::expect_false(1190L %in% codes)
})

# Tests for .nearest_class_rate(): nearest-cell rate transfer for
# grassland allowance classes lacking their own rate (maintainer
# decision 2026-09-24, issue #1285).

testthat::test_that("a cell's own finite rate is kept as archive", {
  cells <- tibble::tribble(
    ~cell_id, ~lon, ~lat, ~country, ~image_region,
    ~managed_rate, ~extensive_rate,
    1L,       10,   10,   100L,     1L,
    25.5,          NA_real_
  )

  out <- whep:::.nearest_class_rate(cells)

  testthat::expect_equal(out$managed_rate, 25.5)
  testthat::expect_equal(out$managed_method, "archive")
})

testthat::test_that("same-country donor wins over a nearer region donor", {
  # Cell 1 (country 10) has no managed_rate. Cell 2 is farther away
  # (lon 5) but shares country 10. Cell 3 is nearer (lon 1) and shares
  # cell 1's region (1), but is in a different country (20). The
  # same-country donor must win regardless of distance.
  cells <- tibble::tribble(
    ~cell_id, ~lon, ~lat, ~country, ~image_region,
    ~managed_rate, ~extensive_rate,
    1L,       0,    0,    10L,      1L,
    NA_real_,      NA_real_,
    2L,       5,    0,    10L,      2L,
    40,            NA_real_,
    3L,       1,    0,    20L,      1L,
    99,            NA_real_
  )

  out <- whep:::.nearest_class_rate(cells)
  row <- dplyr::filter(out, .data$cell_id == 1L)

  testthat::expect_equal(row$managed_rate, 40)
  testthat::expect_equal(row$managed_method, "nearest_country")
})

testthat::test_that("falls back to the nearest same-region donor", {
  # Cell 1 (country 10) has no managed_rate and no donor shares its
  # country. Cell 2 shares cell 1's region (1) and is nearer (lon 2).
  # Cell 3 has a different region (2) and is farther (lon 9).
  cells <- tibble::tribble(
    ~cell_id, ~lon, ~lat, ~country, ~image_region,
    ~managed_rate, ~extensive_rate,
    1L,       0,    0,    10L,      1L,
    NA_real_,      NA_real_,
    2L,       2,    0,    20L,      1L,
    15,            NA_real_,
    3L,       9,    0,    30L,      2L,
    999,           NA_real_
  )

  out <- whep:::.nearest_class_rate(cells)
  row <- dplyr::filter(out, .data$cell_id == 1L)

  testthat::expect_equal(row$managed_rate, 15)
  testthat::expect_equal(row$managed_method, "nearest_region")
})

testthat::test_that("no donor anywhere gives none", {
  cells <- tibble::tribble(
    ~cell_id, ~lon, ~lat, ~country,     ~image_region,
    ~managed_rate, ~extensive_rate,
    1L,       0,    0,    10L,          1L,
    NA_real_,      NA_real_,
    2L,       5,    5,    NA_integer_,  NA_integer_,
    NA_real_,      NA_real_
  )

  out <- whep:::.nearest_class_rate(cells)

  testthat::expect_true(all(is.na(out$managed_rate)))
  testthat::expect_true(all(out$managed_method == "none"))
})

testthat::test_that("an exact distance tie picks the smaller donor cell_id", {
  # Donors 5 (lon 2) and 3 (lon -2) are equidistant from cell 1 (lon 0,
  # same latitude): the haversine formula is symmetric in |dlon|. The
  # tie must resolve to the smaller cell_id, 3.
  cells <- tibble::tribble(
    ~cell_id, ~lon, ~lat, ~country, ~image_region,
    ~managed_rate, ~extensive_rate,
    1L,       0,    0,    10L,      1L,
    NA_real_,      NA_real_,
    5L,       2,    0,    10L,      1L,
    70,            NA_real_,
    3L,       -2,   0,    10L,      1L,
    30,            NA_real_
  )

  out <- whep:::.nearest_class_rate(cells)
  row <- dplyr::filter(out, .data$cell_id == 1L)

  testthat::expect_equal(row$managed_rate, 30)
  testthat::expect_equal(row$managed_method, "nearest_country")
})

testthat::test_that("the two classes are resolved independently", {
  # Cell 1 has its own managed_rate (archive) but no extensive_rate;
  # cell 2 (same country) has an extensive_rate but no managed_rate.
  # Cell 1 must end up "archive" for managed and "nearest_country" for
  # extensive at once.
  cells <- tibble::tribble(
    ~cell_id, ~lon, ~lat, ~country, ~image_region,
    ~managed_rate, ~extensive_rate,
    1L,       0,    0,    10L,      1L,
    12,            NA_real_,
    2L,       1,    0,    10L,      1L,
    NA_real_,      8
  )

  out <- whep:::.nearest_class_rate(cells)
  row <- dplyr::filter(out, .data$cell_id == 1L)

  testthat::expect_equal(row$managed_method, "archive")
  testthat::expect_equal(row$managed_rate, 12)
  testthat::expect_equal(row$extensive_method, "nearest_country")
  testthat::expect_equal(row$extensive_rate, 8)
})

testthat::test_that("a duplicated cell_id aborts", {
  cells <- tibble::tribble(
    ~cell_id, ~lon, ~lat, ~country, ~image_region,
    ~managed_rate, ~extensive_rate,
    1L,       0,    0,    10L,      1L,
    12,            NA_real_,
    1L,       1,    0,    10L,      1L,
    14,            NA_real_
  )

  testthat::expect_error(
    whep:::.nearest_class_rate(cells),
    class = "whep_nearest_rate_input"
  )
})

testthat::test_that("chunking above 1000 recipients matches a full scan", {
  withr::local_seed(42)
  n_donor <- 40
  n_recipient <- 1200
  donors <- tibble::tibble(
    cell_id = seq_len(n_donor),
    lon = stats::runif(n_donor, -30, 30),
    lat = stats::runif(n_donor, -30, 30),
    country = 1L,
    image_region = 1L,
    managed_rate = stats::runif(n_donor, 1, 100),
    extensive_rate = NA_real_
  )
  recipients <- tibble::tibble(
    cell_id = seq(n_donor + 1, n_donor + n_recipient),
    lon = stats::runif(n_recipient, -30, 30),
    lat = stats::runif(n_recipient, -30, 30),
    country = 1L,
    image_region = 1L,
    managed_rate = NA_real_,
    extensive_rate = NA_real_
  )
  cells <- dplyr::bind_rows(donors, recipients)

  t0 <- Sys.time()
  out <- whep:::.nearest_class_rate(cells)
  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  testthat::expect_lt(elapsed, 5)

  # Brute-force reference: one recipient x donor distance matrix built
  # with the package's own haversine helper (same formula the chunked
  # path uses), and the same smallest-cell_id tie rule. Donors are
  # already in ascending cell_id order by construction (seq_len), so
  # `nearest` indexes directly into `donors$managed_rate`.
  d <- whep:::.haversine_km_matrix(
    recipients$lon,
    recipients$lat,
    donors$lon,
    donors$lat
  )
  nearest <- max.col(-d, ties.method = "first")
  expected_rate <- donors$managed_rate[nearest]

  got <- out |>
    dplyr::filter(.data$cell_id %in% recipients$cell_id) |>
    dplyr::arrange(.data$cell_id)
  testthat::expect_equal(got$managed_rate, expected_rate)
  testthat::expect_true(all(got$managed_method == "nearest_country"))
})

testthat::test_that("the antimeridian is treated as near, not far", {
  # Cell 1 (lon 179.75) has no donor. Cell 2 (lon -179.75) is its true
  # near neighbour across the antimeridian (about 0.5 degrees away).
  # Cell 3 (lon 0) is nearly antipodal (about 179.75 degrees away). The
  # nearest donor must be cell 2, not cell 3.
  cells <- tibble::tribble(
    ~cell_id, ~lon,     ~lat, ~country, ~image_region,
    ~managed_rate, ~extensive_rate,
    1L,       179.75,   0,    10L,      1L,
    NA_real_,      NA_real_,
    2L,       -179.75,  0,    10L,      1L,
    55,            NA_real_,
    3L,       0,        0,    10L,      1L,
    5,             NA_real_
  )

  out <- whep:::.nearest_class_rate(cells)
  row <- dplyr::filter(out, .data$cell_id == 1L)

  testthat::expect_equal(row$managed_rate, 55)
  testthat::expect_equal(row$managed_method, "nearest_country")
})

# ---- build_grassland_intensity_classes() ------------------------------------
#
# Three present-day countries on real area codes, so the historical-polity
# resolution runs against the shipped polity tables:
# - Russia (185), reported as the USSR (228) in 1961 and 1991 and on its own
#   from 1992. Its grassland is 400 ha in every year; grazer LU is 60 (1961)
#   and 100 (1991) under the USSR, 80 (1992) and 100 (2010) under Russia. So
#   D_USSR = 0.15 and 0.25, D_Russia = 0.2 and 0.25, and the chained 1961
#   density is 0.2 * 0.15 / 0.25 = 0.12: ratio 0.12 / 0.25 = 0.48. IMAGE
#   intensive area 200 ha (cells 50121, 50122) -> 1961 target 96 ha: keeping
#   50121 (100 ha, gap 4) is nearer than keeping none (gap 96) or both (gap
#   104), so 50122, the lower-manure intensive cell, is demoted.
# - Argentina (9): LU and grassland unchanged, ratio 1. Cell 179522 carries
#   WHEP grassland but no IMAGE grassland.
# - Brazil (21): zero grazer LU in 2010, so every ratio is undefined and its
#   cells keep the IMAGE class.

.gic_fx_layers <- function() {
  tibble::tribble(
    ~cell_id, ~lon,   ~lat,   ~a_crop_ha, ~a_gr_int_ha, ~a_gr_ext_ha,
    ~manure_int_n_kg, ~manure_ext_n_kg, ~image_class_2010, ~image_region,
    50121L,   40.25,  55.25,  30,         100,          0,
    5000,             NA_real_,         "intensive",       15L,
    50122L,   40.75,  55.25,  10,         100,          0,
    2000,             NA_real_,         "intensive",       15L,
    50123L,   41.25,  55.25,  0,          0,            200,
    NA_real_,         1000,             "extensive",       15L,
    179520L,  -60.25, -34.75, 20,         100,          0,
    3000,             NA_real_,         "intensive",       5L,
    179521L,  -59.75, -34.75, 0,          0,            100,
    NA_real_,         500,              "extensive",       5L,
    179522L,  -59.25, -34.75, 40,         0,            0,
    NA_real_,         NA_real_,         NA_character_,     5L,
    144260L,  -50.25, -10.25, 0,          80,           0,
    2400,             NA_real_,         "intensive",       4L,
    144261L,  -49.75, -10.25, 0,          0,            120,
    NA_real_,         600,              "extensive",       4L
  )
}

.gic_fx_cell_polity <- function() {
  tibble::tribble(
    ~lon,   ~lat,   ~area_code, ~polity_frac,
    40.25,  55.25,  185L,       1,
    40.75,  55.25,  185L,       1,
    41.25,  55.25,  185L,       1,
    -60.25, -34.75, 9L,         1,
    -59.75, -34.75, 9L,         1,
    -59.25, -34.75, 9L,         1,
    -50.25, -10.25, 21L,        1,
    -49.75, -10.25, 21L,        1
  )
}

.gic_fx_pasture <- function() {
  cells <- tibble::tribble(
    ~lon,   ~lat,   ~pasture_ha, ~rangeland_ha,
    40.25,  55.25,  60,          40,
    40.75,  55.25,  100,         0,
    41.25,  55.25,  50,          150,
    -60.25, -34.75, 100,         0,
    -59.75, -34.75, 70,          30,
    -59.25, -34.75, 50,          0,
    -50.25, -10.25, 80,          0,
    -49.75, -10.25, 20,          100
  )
  tidyr::expand_grid(cells, year = c(1961L, 1991L, 1992L, 2010L))
}

.gic_fx_stock_lu <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit,   ~value,
    1961L, 228L,       961L,            "LU",    60,
    1991L, 228L,       961L,            "LU",    100,
    1992L, 185L,       961L,            "LU",    80,
    1961L, 9L,         961L,            "LU",    100,
    1961L, 21L,        961L,            "LU",    50,
    2010L, 185L,       961L,            "LU",    100,
    2010L, 9L,         961L,            "LU",    100,
    2010L, 21L,        961L,            "LU",    0,
    # A non-grazer row and a head count, both excluded.
    2010L, 185L,       1049L,           "LU",    500,
    1961L, 228L,       961L,            "heads", 9999
  )
}

.gic_fx_data <- function() {
  list(
    stock_lu = .gic_fx_stock_lu(),
    gridded_pasture = .gic_fx_pasture(),
    cell_polity = .gic_fx_cell_polity(),
    grassland_layers = .gic_fx_layers()
  )
}

.gic_fx_run <- function(years = c(1961L, 2010L), data = .gic_fx_data()) {
  suppressMessages(
    whep::build_grassland_intensity_classes(years = years, data = data)
  )
}

.gic_row <- function(out, cell, yr) {
  dplyr::filter(out, .data$cell_id == cell, .data$year == yr)
}

testthat::test_that("the builder returns the documented columns", {
  out <- .gic_fx_run()
  testthat::expect_named(
    out,
    names(whep::build_grassland_intensity_classes(example = TRUE))
  )
  testthat::expect_type(out$cell_id, "integer")
  testthat::expect_type(out$year, "integer")
  testthat::expect_type(out$country_2010, "integer")
  testthat::expect_type(out$image_region, "integer")
})

testthat::test_that("2010 classes equal the IMAGE map", {
  out <- .gic_fx_run() |>
    dplyr::filter(.data$year == 2010L, !is.na(.data$image_class_2010))
  testthat::expect_equal(out$grassland_class, out$image_class_2010)
  ranked <- dplyr::filter(out, .data$country_2010 != 21L)
  testthat::expect_equal(ranked$density_ratio, rep(1, nrow(ranked)))
})

testthat::test_that("a USSR-reported year uses the USSR's density", {
  out <- .gic_fx_run()
  russia <- dplyr::filter(out, .data$country_2010 == 185L, .data$year == 1961L)
  # Russia's own level (0.2 in 1992) moved by the USSR's trend
  # (0.15 / 0.25), over Russia's 2010 density (0.25).
  testthat::expect_equal(russia$density_ratio, rep(0.48, 3))
  testthat::expect_equal(russia$target_share, rep(96 / 400, 3))
  testthat::expect_equal(
    russia$density_basis,
    rep("chained_predecessor_trend", 3)
  )
  russia_2010 <- dplyr::filter(
    out,
    .data$country_2010 == 185L,
    .data$year == 2010L
  )
  testthat::expect_equal(russia_2010$density_basis, rep("own", 3))
})

testthat::test_that("a lower earlier density demotes the lowest-manure cell", {
  out <- .gic_fx_run()
  testthat::expect_equal(
    .gic_row(out, 50121L, 1961L)$grassland_class,
    "intensive"
  )
  testthat::expect_equal(
    .gic_row(out, 50122L, 1961L)$grassland_class,
    "extensive"
  )
  testthat::expect_equal(
    .gic_row(out, 50123L, 1961L)$grassland_class,
    "extensive"
  )
  testthat::expect_equal(
    .gic_row(out, 50122L, 1961L)$method_grassland_split,
    "image2010_density_rank"
  )
})

testthat::test_that("WHEP grassland without IMAGE grassland is extensive", {
  row <- .gic_row(.gic_fx_run(), 179522L, 1961L)
  testthat::expect_equal(row$grassland_class, "extensive")
  testthat::expect_equal(row$method_grassland_split, "no_image_grassland")
  testthat::expect_true(is.na(row$image_class_2010))
  testthat::expect_equal(row$grass_ha_image, 0)
  testthat::expect_equal(row$whep_grass_ha, 50)
  testthat::expect_equal(row$a_crop_ha, 40)
  testthat::expect_true(is.na(row$target_share))
})

testthat::test_that("every in-scope cell appears once per year", {
  out <- .gic_fx_run()
  testthat::expect_equal(nrow(out), 8L * 2L)
  testthat::expect_false(anyDuplicated(out[c("cell_id", "year")]) > 0)
  testthat::expect_setequal(out$cell_id, .gic_fx_layers()$cell_id)
  one_year <- .gic_fx_run(years = 1961L)
  testthat::expect_equal(unique(one_year$year), 1961L)
  testthat::expect_equal(nrow(one_year), 8L)
})

testthat::test_that("a cell with no grassland anywhere is out of scope", {
  data <- .gic_fx_data()
  data$gridded_pasture <- data$gridded_pasture |>
    dplyr::mutate(
      pasture_ha = dplyr::if_else(.data$lon == -59.25, 0, .data$pasture_ha)
    )
  out <- .gic_fx_run(data = data)
  testthat::expect_false(179522L %in% out$cell_id)
})

testthat::test_that("zero 2010 grazing keeps the IMAGE class", {
  out <- .gic_fx_run()
  brazil <- dplyr::filter(out, .data$country_2010 == 21L)
  testthat::expect_true(all(is.na(brazil$density_ratio)))
  testthat::expect_equal(brazil$grassland_class, brazil$image_class_2010)
  testthat::expect_true(
    all(brazil$method_grassland_split == "image2010_fixed_no_density")
  )
  testthat::expect_message(
    whep::build_grassland_intensity_classes(
      years = c(1961L, 2010L),
      data = .gic_fx_data()
    ),
    class = "whep_grassland_no_density"
  )
})

testthat::test_that("an IMAGE cell with no crosswalk country keeps its class", {
  data <- .gic_fx_data()
  data$cell_polity <- dplyr::filter(data$cell_polity, .data$lon != 40.75)
  # Brazil's undefined density informs too; only the crosswalk gap is
  # under test here.
  testthat::expect_message(
    out <- withCallingHandlers(
      whep::build_grassland_intensity_classes(years = 1961L, data = data),
      whep_grassland_no_density = function(m) invokeRestart("muffleMessage")
    ),
    class = "whep_grassland_no_country"
  )
  row <- .gic_row(out, 50122L, 1961L)
  testthat::expect_true(is.na(row$country_2010))
  testthat::expect_equal(row$grassland_class, "intensive")
  testthat::expect_equal(
    row$method_grassland_split,
    "image2010_fixed_no_density"
  )
})

testthat::test_that("a missing year aborts instead of reading as no grazing", {
  data <- .gic_fx_data()
  data$stock_lu <- dplyr::filter(data$stock_lu, .data$year != 1961L)
  testthat::expect_error(
    .gic_fx_run(data = data),
    class = "whep_grassland_year_coverage"
  )
})

testthat::test_that("an all-zero grazer LU table is refused", {
  data <- .gic_fx_data()
  data$stock_lu <- dplyr::mutate(data$stock_lu, value = 0)
  rows <- dplyr::filter(
    data$stock_lu,
    .data$area_code == 9L,
    .data$year == 2010L,
    .data$item_prod_code %in% whep:::.grazer_item_codes()
  )
  dens <- whep:::.grazing_density(
    rows,
    tibble::tibble(area_code = 9L, year = 2010L, grassland_ha = 250)
  )
  expect_supplied_guard(
    # The national LU is the sum of its item rows: 0 = 0 on the vacuous
    # table (zero LU gives no density, so density x area cannot be used).
    identity = isTRUE(all.equal(sum(rows$value), sum(dens$grazing_lu))),
    guard = .gic_fx_run(data = data)
  )
})

testthat::test_that("an all-zero grassland surface is refused", {
  data <- .gic_fx_data()
  data$gridded_pasture <- dplyr::mutate(
    data$gridded_pasture,
    pasture_ha = 0,
    rangeland_ha = 0
  )
  grass <- data$gridded_pasture
  expect_supplied_guard(
    # The national total is the sum of its cells: 0 = 0.
    identity = sum(grass$pasture_ha + grass$rangeland_ha) ==
      sum(grass$pasture_ha) + sum(grass$rangeland_ha),
    guard = .gic_fx_run(data = data)
  )
})

testthat::test_that("all-zero IMAGE grassland layers are refused", {
  data <- .gic_fx_data()
  data$grassland_layers <- dplyr::mutate(
    data$grassland_layers,
    a_gr_int_ha = 0,
    a_gr_ext_ha = 0
  )
  layers <- data$grassland_layers
  expect_supplied_guard(
    # Intensive + extensive = grassland still holds: 0 + 0 = 0.
    identity = all(
      layers$a_gr_int_ha + layers$a_gr_ext_ha ==
        pmax(layers$a_gr_int_ha, layers$a_gr_ext_ha)
    ),
    guard = .gic_fx_run(data = data)
  )
})

testthat::test_that("absent IMAGE manure layers are refused", {
  data <- .gic_fx_data()
  data$grassland_layers <- dplyr::mutate(
    data$grassland_layers,
    manure_int_n_kg = NA_real_,
    manure_ext_n_kg = NA_real_
  )
  testthat::expect_error(
    .gic_fx_run(data = data),
    class = "whep_absent_input"
  )
})

testthat::test_that("grazer rows that match nothing are refused", {
  data <- .gic_fx_data()
  data$stock_lu <- dplyr::mutate(data$stock_lu, item_prod_code = 1049L)
  testthat::expect_error(
    .gic_fx_run(data = data),
    class = "whep_absent_input"
  )
})

testthat::test_that("successors and aggregates reach their reporting polity", {
  grazers <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    1961L, 228L,       961L,            "LU",  10,
    1961L, 15L,        961L,            "LU",  10,
    1961L, 150L,       961L,            "LU",  10
  )
  map <- whep:::.gic_reporting_map(
    c(63L, 150L, 185L, 255L, 256L),
    1961L,
    grazers
  )$map |>
    dplyr::arrange(.data$area_code)
  # Russia walks its predecessor edge to the USSR. Estonia's predecessor
  # chain is its own pre-1940 polities, and Belgium's reaches the
  # Netherlands' family; both are placed by the successor walk instead:
  # Estonia on the USSR, Belgium and Luxembourg on Belgium-Luxembourg.
  testthat::expect_equal(
    map$polity_code,
    c(
      "F228-1945-1991",
      "NLD-1830-2025",
      "F228-1945-1991",
      "BLX-1850-1999",
      "BLX-1850-1999"
    )
  )
  testthat::expect_equal(
    map$method_polity_lineage,
    c(
      "successor_walk",
      "anchor",
      "predecessor",
      "successor_walk",
      "successor_walk"
    )
  )
})

testthat::test_that("a sibling interval anchored by another code is refused", {
  map <- tibble::tribble(
    ~area_code, ~year, ~polity_code,    ~method_polity_lineage,
    150L,       1961L, "NLD-1830-2025", "anchor",
    255L,       1961L, "NLD-1830-2025", "sibling_interval",
    272L,       1961L, "F248-1947-1991", "sibling_interval"
  )
  out <- whep:::.gic_drop_sibling_collisions(map)
  testthat::expect_equal(
    out$polity_code,
    c("NLD-1830-2025", NA, "F248-1947-1991")
  )
  testthat::expect_equal(
    out$method_polity_lineage,
    c("anchor", "unresolved", "sibling_interval")
  )
})

testthat::test_that("the example fixture carries the output contract", {
  ex <- whep::build_grassland_intensity_classes(example = TRUE)
  testthat::expect_equal(
    names(ex),
    c(
      "cell_id",
      "lon",
      "lat",
      "year",
      "country_2010",
      "image_region",
      "a_crop_ha",
      "grass_ha_image",
      "whep_grass_ha",
      "image_class_2010",
      "grassland_class",
      "density_ratio",
      "target_share",
      "method_grassland_split",
      "density_basis"
    )
  )
  # The fixture is self-consistent: the engine reproduces its classes.
  ranked <- dplyr::filter(ex, !is.na(.data$image_class_2010))
  cells <- ranked |>
    dplyr::distinct(.data$cell_id, .keep_all = TRUE) |>
    dplyr::transmute(
      .data$cell_id,
      .data$country_2010,
      grass_ha = .data$grass_ha_image,
      .data$image_class_2010,
      manure_n_kg_ha = c(40, 30, 50, 20, 10)
    )
  got <- whep:::.classify_grassland_cells(
    cells,
    dplyr::select(ranked, "cell_id", "year", "density_ratio")
  )
  testthat::expect_equal(got$grassland_class, ranked$grassland_class)
  testthat::expect_equal(got$target_share, ranked$target_share)
})

testthat::test_that("codes folded into a reporting bucket use the bucket", {
  # From 2012 Sudan (276) and South Sudan (277) report folded into area 206;
  # in 2010 area 206 is the pre-2011 Sudan both descend from.
  grazers <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2010L, 206L,       961L,            "LU",  10,
    2020L, 206L,       961L,            "LU",  10
  )
  map <- whep:::.gic_reporting_map(
    c(276L, 277L),
    c(2010L, 2020L),
    grazers
  )$map |>
    dplyr::arrange(.data$year, .data$area_code)
  testthat::expect_equal(
    map$method_polity_lineage,
    c("predecessor", "predecessor", "bucket_fold", "bucket_fold")
  )
  testthat::expect_equal(map$polity_code[1:2], rep("SUD-1956-2011", 2))
  testthat::expect_true(all(startsWith(map$polity_code[3:4], "F206-")))
})

testthat::test_that("equidistant donors in a grid row tie to the smaller id", {
  # Donors 0.5 degrees either side of the recipient differ by ~1e-13 km in
  # floating point; the documented rule gives the tie to cell_id 2.
  cells <- tibble::tribble(
    ~cell_id, ~lon,  ~lat,  ~country, ~image_region,
    ~managed_rate, ~extensive_rate,
    1L,       -7.75, 40.25, 10L,      1L,
    NA_real_,      NA_real_,
    2L,       -8.25, 40.25, 10L,      1L,
    20,            NA_real_,
    3L,       -7.25, 40.25, 10L,      1L,
    30,            NA_real_
  )
  row <- whep:::.nearest_class_rate(cells) |>
    dplyr::filter(.data$cell_id == 1L)
  testthat::expect_equal(row$managed_rate, 20)
  testthat::expect_equal(row$managed_method, "nearest_country")
})

# ---- density chaining (maintainer decision 2026-09-24) ----------------------

.gic_chain_ratio <- function(map, density, code) {
  code_density <- whep:::.gic_code_density(map, density)
  cells <- tibble::tibble(cell_id = 1L, country_2010 = code)
  list(
    density = code_density,
    ratio = whep:::.gic_cell_ratios(cells, code_density, 1961L)
  )
}

testthat::test_that("a successor keeps its level and borrows the trend", {
  # Kazakhstan-like: the USSR at 0.3 (1961) and 0.25 (1991); Kazakhstan on
  # its own at 0.04 (1992) and 0.045 (2010).
  map <- tibble::tribble(
    ~area_code, ~year, ~polity_code,     ~method_polity_lineage,
    108L,       1961L, "F228-1945-1991", "predecessor",
    108L,       1991L, "F228-1945-1991", "predecessor",
    108L,       1992L, "KAZ-1991-2025",  "anchor",
    108L,       2010L, "KAZ-1991-2025",  "anchor"
  )
  density <- tibble::tribble(
    ~polity_code,     ~year, ~grazing_density,
    "F228-1945-1991", 1961L, 0.3,
    "F228-1945-1991", 1991L, 0.25,
    "KAZ-1991-2025",  1992L, 0.04,
    "KAZ-1991-2025",  2010L, 0.045
  )
  got <- .gic_chain_ratio(map, density, 108L)
  d <- dplyr::arrange(got$density, .data$year)
  testthat::expect_equal(d$code_density, c(0.048, 0.04, 0.04, 0.045))
  testthat::expect_equal(
    d$density_basis,
    c(rep("chained_predecessor_trend", 2), "own", "own")
  )
  testthat::expect_equal(got$ratio$density_ratio, 0.048 / 0.045)
  testthat::expect_equal(got$ratio$density_basis, "chained_predecessor_trend")
})

testthat::test_that("a two-link chain is linked from the present back", {
  # Serbia: Yugoslavia (1961, 1990), then Serbia and Montenegro (1995,
  # 2005), then Serbia on its own (2006, 2010). The newer link first:
  # D(1995) = 1.2 * 0.8 / 1.0 = 0.96, D(2005) = 1.2; then the older link
  # onto it: D(1961) = 0.96 * 1.2 / 1.0 = 1.152, D(1990) = 0.96.
  map <- tibble::tribble(
    ~area_code, ~year, ~polity_code,     ~method_polity_lineage,
    272L,       1961L, "F248-1947-1991", "sibling_interval",
    272L,       1990L, "F248-1947-1991", "sibling_interval",
    272L,       1995L, "SCG-1992-2006",  "predecessor",
    272L,       2005L, "SCG-1992-2006",  "predecessor",
    272L,       2006L, "SRB-2006-2008",  "anchor",
    272L,       2010L, "SRB-2008-2025",  "anchor"
  )
  density <- tibble::tribble(
    ~polity_code,     ~year, ~grazing_density,
    "F248-1947-1991", 1961L, 1.2,
    "F248-1947-1991", 1990L, 1.0,
    "SCG-1992-2006",  1995L, 0.8,
    "SCG-1992-2006",  2005L, 1.0,
    "SRB-2006-2008",  2006L, 1.2,
    "SRB-2008-2025",  2010L, 1.5
  )
  got <- .gic_chain_ratio(map, density, 272L)
  d <- dplyr::arrange(got$density, .data$year)
  testthat::expect_equal(
    d$code_density,
    c(1.152, 0.96, 0.96, 1.2, 1.2, 1.5)
  )
  testthat::expect_equal(
    d$density_basis,
    c(rep("chained_predecessor_trend", 4), "own", "own")
  )
  testthat::expect_equal(got$ratio$density_ratio, 1.152 / 1.5)
})

testthat::test_that("an undefined link leaves the chained density NA", {
  map <- tibble::tribble(
    ~area_code, ~year, ~polity_code,     ~method_polity_lineage,
    108L,       1961L, "F228-1945-1991", "predecessor",
    108L,       1991L, "F228-1945-1991", "predecessor",
    108L,       1992L, "KAZ-1991-2025",  "anchor",
    108L,       2010L, "KAZ-1991-2025",  "anchor"
  )
  no_pred <- tibble::tribble(
    ~polity_code,     ~year, ~grazing_density,
    "F228-1945-1991", 1961L, NA_real_,
    "F228-1945-1991", 1991L, NA_real_,
    "KAZ-1991-2025",  1992L, 0.04,
    "KAZ-1991-2025",  2010L, 0.045
  )
  got <- .gic_chain_ratio(map, no_pred, 108L)
  testthat::expect_true(is.na(got$ratio$density_ratio))
  testthat::expect_equal(got$ratio$density_basis, "chained_predecessor_trend")

  # No later year with an own density: t0 is undefined.
  no_own <- dplyr::mutate(
    no_pred,
    grazing_density = c(0.3, 0.25, NA, NA)
  )
  got <- .gic_chain_ratio(map, no_own, 108L)
  testthat::expect_true(all(is.na(got$density$code_density)))
  testthat::expect_true(is.na(got$ratio$density_ratio))
})

testthat::test_that("a shared bucket keeps the bucket density", {
  # Sudan (276): the pre-2011 Sudan in 2010, folded into area 206 from 2012.
  # The bucket years are not chained; 2010 is chained onto the bucket's
  # first year: 0.7 * 0.6 / 0.65.
  map <- tibble::tribble(
    ~area_code, ~year, ~polity_code,     ~method_polity_lineage,
    276L,       2010L, "SUD-1956-2011",  "predecessor",
    276L,       2011L, "SUD-1956-2011",  "predecessor",
    276L,       2012L, "F206-2011-2025", "bucket_fold",
    276L,       2020L, "F206-2011-2025", "bucket_fold"
  )
  density <- tibble::tribble(
    ~polity_code,     ~year, ~grazing_density,
    "SUD-1956-2011",  2010L, 0.6,
    "SUD-1956-2011",  2011L, 0.65,
    "F206-2011-2025", 2012L, 0.7,
    "F206-2011-2025", 2020L, 0.9
  )
  d <- whep:::.gic_code_density(map, density) |>
    dplyr::arrange(.data$year)
  testthat::expect_equal(
    d$code_density,
    c(0.7 * 0.6 / 0.65, 0.7, 0.7, 0.9)
  )
  testthat::expect_equal(
    d$density_basis,
    c(rep("chained_predecessor_trend", 2), "bucket", "bucket")
  )
})
