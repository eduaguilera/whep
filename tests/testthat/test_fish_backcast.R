# -- The pre-1961 fish production back-cast (whep#1543) ------------------------

# One area, two fish items, two years. 1961 is the FAOSTAT anchor and 1960 the
# year the back-cast fills. Pelagic Fish is a net exporter in 1961 (production
# 120 against 100 of use); Demersal Fish is produced nowhere in the area and is
# all imported. Population grows 90 -> 100 and agricultural land is flat, so
# 1960 food is 0.9 of 1961 and feed is unchanged.
.fish_backcast_frame <- function() {
  observed <- tibble::tribble(
    ~year, ~item_cbs,        ~item_cbs_code, ~element,          ~value,
    1961L, "Pelagic Fish",   2763L,          "production",      120,
    1961L, "Pelagic Fish",   2763L,          "import",          10,
    1961L, "Pelagic Fish",   2763L,          "export",          30,
    1961L, "Pelagic Fish",   2763L,          "domestic_supply", 100,
    1961L, "Pelagic Fish",   2763L,          "food",            70,
    1961L, "Pelagic Fish",   2763L,          "feed",            30,
    1961L, "Demersal Fish",  2762L,          "production",      0,
    1961L, "Demersal Fish",  2762L,          "import",          50,
    1961L, "Demersal Fish",  2762L,          "export",          0,
    1961L, "Demersal Fish",  2762L,          "domestic_supply", 50,
    1961L, "Demersal Fish",  2762L,          "food",            50
  ) |>
    dplyr::mutate(area = "Algeria", area_code = 4L)
  whep:::.cbs_complete_year_nesting_dt(
    observed,
    id_cols = c("area", "area_code", "item_cbs", "item_cbs_code", "element"),
    years = 1960:1961
  ) |>
    tibble::as_tibble()
}

.fish_backcast_args <- function() {
  list(
    primary_area = tibble::tibble(
      year = integer(),
      area_code = integer(),
      item_cbs = character(),
      item_cbs_code = integer(),
      area_ha = double()
    ),
    gdp_pop = tibble::tibble(
      year = c(1960L, 1961L),
      area_code = "DZA",
      pop = c(90, 100)
    ),
    land_wide = tibble::tibble(
      year = c(1960L, 1961L),
      area_code = 4L,
      Cropland = 10,
      Pasture = 90,
      agriland = 100
    )
  )
}

.run_fish_backcast <- function(..., frame = .fish_backcast_frame()) {
  a <- .fish_backcast_args()
  whep:::.fill_historical_destinies(
    frame,
    a$primary_area,
    a$gdp_pop,
    a$land_wide,
    whep::items_full,
    ...
  ) |>
    suppressMessages()
}

# The row's balance residual, as `.reestimate_domestic_supply()` computes the
# stock term: supply less trade less the uses the back-cast grew.
.fish_balance_at <- function(result, yr, code) {
  wide <- result |>
    dplyr::filter(.data$year == yr, .data$item_cbs_code == code) |>
    dplyr::select("element", "value") |>
    tidyr::pivot_wider(names_from = "element", values_from = "value")
  value_of <- \(el) if (el %in% names(wide)) wide[[el]] else 0
  uses <- c("food", "feed", "other_uses", "processing", "seed") |>
    purrr::map_dbl(value_of) |>
    sum()
  list(
    production = value_of("production"),
    import = value_of("import"),
    export = value_of("export"),
    uses = uses,
    stock = value_of("production") +
      value_of("import") -
      value_of("export") -
      uses
  )
}

test_that("pre-1961 fish use has production behind it", {
  # whep#1543: before 1961 the fish items had uses and no production or
  # trade, so the whole use closed through `stock_variation` -- about 37 Mt a
  # year of stock draw on stocks that never existed. Asserted on the balance
  # residual, because the balance identity itself holds on the broken rows
  # too: the stock term absorbs the missing supply exactly.
  result <- .run_fish_backcast()
  pelagic <- .fish_balance_at(result, 1960L, 2763L)

  expect_gt(pelagic$production, 0)
  expect_equal(pelagic$stock, 0)
})

test_that("supply_ratio holds the 1961 self-sufficiency of each item", {
  # 1960 uses: food 70 * 0.9 = 63, feed 30 -> 93. Production 120 / 100 of use
  # in 1961, so 1.2 * 93 = 111.6, and the 18.6 surplus is exported.
  pelagic <- .run_fish_backcast(fish_backcast = "supply_ratio") |>
    .fish_balance_at(1960L, 2763L)

  expect_equal(pelagic$uses, 93)
  expect_equal(pelagic$production, 111.6)
  expect_equal(pelagic$export, 18.6)
  expect_equal(pelagic$import, 0)
})

test_that("supply_ratio books an item the area never produced as imports", {
  # Demersal Fish: no 1961 production, so none before it either, and the 45 t
  # of 1960 food arrives as imports instead of as a stock draw.
  demersal <- .run_fish_backcast(fish_backcast = "supply_ratio") |>
    .fish_balance_at(1960L, 2762L)

  expect_equal(demersal$production, 0)
  expect_equal(demersal$import, 45)
  expect_equal(demersal$stock, 0)
})

test_that("closed_economy books every pre-1961 use as production", {
  result <- .run_fish_backcast(fish_backcast = "closed_economy")
  pelagic <- .fish_balance_at(result, 1960L, 2763L)
  demersal <- .fish_balance_at(result, 1960L, 2762L)

  expect_equal(pelagic$production, 93)
  expect_equal(pelagic$export, 0)
  expect_equal(demersal$production, 45)
  expect_equal(demersal$import, 0)
  expect_equal(demersal$stock, 0)
})

test_that("the fish back-cast leaves the 1961 anchor as observed", {
  result <- .run_fish_backcast()
  pelagic <- .fish_balance_at(result, 1961L, 2763L)

  expect_equal(pelagic$production, 120)
  expect_equal(pelagic$import, 10)
  expect_equal(pelagic$export, 30)
})

test_that("the historical extension labels the fish supply with its method", {
  a <- .fish_backcast_args()
  inputs <- list(
    primary_cbs_area = a$primary_area,
    gdp_pop = a$gdp_pop,
    land_areas_wide = a$land_wide
  )
  raw <- .fish_backcast_frame() |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::mutate(source = "FAOSTAT_FBS_Old")
  result <- whep:::.cbs_extend_historical(
    raw,
    inputs,
    1960:1961,
    fish_backcast = "closed_economy"
  ) |>
    suppressMessages() |>
    dplyr::filter(.data$year == 1960L, .data$item_cbs_code == 2763L)
  source_of <- \(el) result$source[result$element == el]

  expect_equal(source_of("production"), "historical_fish_closed_economy")
  expect_equal(source_of("import"), "historical_fish_closed_economy")
  expect_equal(source_of("food"), "historical_fill")
})

test_that("the fish back-cast stamps the rows it estimated", {
  result <- .run_fish_backcast()
  stamped <- result |>
    dplyr::filter(.data$fish_backcast) |>
    dplyr::distinct(.data$year, .data$item_cbs_code)

  expect_equal(unique(stamped$year), 1960L)
  expect_setequal(stamped$item_cbs_code, c(2762L, 2763L))
  expect_equal(
    whep:::.fish_backcast_source("supply_ratio"),
    "historical_fish_supply_ratio"
  )
})

test_that("the fish back-cast does not touch non-fish items", {
  a <- .fish_backcast_args()
  wheat <- .fish_backcast_frame() |>
    dplyr::filter(.data$item_cbs_code == 2763L) |>
    dplyr::mutate(item_cbs = "Wheat", item_cbs_code = 2511L)
  result <- whep:::.fill_historical_destinies(
    wheat,
    a$primary_area,
    a$gdp_pop,
    a$land_wide,
    whep::items_full
  ) |>
    suppressMessages() |>
    dplyr::filter(.data$year == 1960L)

  expect_false(any(result$fish_backcast))
  expect_false("import" %in% result$element)
})

test_that("a missing anchor production is read off the balance", {
  # The food balances omit a zero production row: Austria's 1961 Pelagic Fish
  # is supply 141 kt, import 142 kt, export 1 kt and no production row. The
  # balance itself says production is zero, so the item is imported before
  # 1961 rather than left as a stock draw.
  frame <- .fish_backcast_frame() |>
    dplyr::filter(
      !(.data$element == "production" & .data$item_cbs_code == 2762L)
    )
  demersal <- .run_fish_backcast(frame = frame) |>
    .fish_balance_at(1960L, 2762L)

  expect_equal(demersal$production, 0)
  expect_equal(demersal$import, 45)
  expect_equal(demersal$stock, 0)
})

test_that("a fish key with no production anchor is reported, not filled", {
  # With neither a production nor a supply observed there is no ratio to
  # hold, so the key keeps the generic fill and its use stays a stock draw.
  # That must be said.
  frame <- .fish_backcast_frame() |>
    dplyr::filter(
      !(.data$element %in%
        c("production", "domestic_supply") &
        .data$year == 1961L)
    )
  a <- .fish_backcast_args()

  expect_message(
    whep:::.fill_historical_destinies(
      frame,
      a$primary_area,
      a$gdp_pop,
      a$land_wide,
      whep::items_full
    ),
    class = "whep_fish_backcast"
  )
})

test_that("build_commodity_balances validates fish_backcast", {
  expect_error(
    build_commodity_balances(example = TRUE, fish_backcast = "fishstat"),
    class = "rlang_error"
  )
})
