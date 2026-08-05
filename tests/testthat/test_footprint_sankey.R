testthat::test_that("plot_footprint_sankey writes browser viewer", {
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("jsonlite")

  footprints <- tibble::tribble(
    ~origin_area, ~origin_item, ~target_item, ~target_area, ~value,
    "Brazil", "Soybeans", "Pigmeat", "China", 10,
    "Brazil", "Soybeans", "Milk", "China", 4,
    "Brazil", "Soybeans", "Soybean Oil", "France", 3
  )
  file <- tempfile(fileext = ".html")

  result <- plot_footprint_sankey(
    footprints,
    file = file,
    title = "Test Sankey",
    value_label = "ha"
  )

  testthat::expect_equal(result, file)
  testthat::expect_true(file.exists(file))

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  testthat::expect_match(html, "whep-sankey", fixed = TRUE)
  testthat::expect_match(html, "Test Sankey", fixed = TRUE)
  testthat::expect_match(html, "Hide paths smaller than", fixed = TRUE)
  testthat::expect_match(
    html,
    "Max ' + esc(stage.label) + ' nodes",
    fixed = TRUE
  )
  testthat::expect_match(html, "data-max-stage", fixed = TRUE)
  testthat::expect_match(html, "Hidden by threshold", fixed = TRUE)
  testthat::expect_match(html, "Soybeans", fixed = TRUE)
})

testthat::test_that("plot_footprint_sankey uses labels and groups small nodes", {
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("jsonlite")

  footprints <- tibble::tribble(
    ~origin_area, ~origin_area_name, ~origin_item, ~target_item, ~target_area, ~value,
    1L, "Brazil", "Soybeans", "Pigmeat", "China", 10,
    1L, "Brazil", "Maize", "Milk", "China", 4,
    2L, "France", "Wheat", "Bread", "Brazil", 3
  )
  file <- tempfile(fileext = ".html")

  plot_footprint_sankey(
    footprints,
    max_nodes = 1,
    file = file
  )

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  testthat::expect_match(html, "Brazil", fixed = TRUE)
  testthat::expect_match(html, "France", fixed = TRUE)
  testthat::expect_match(html, "Other", fixed = TRUE)
})

testthat::test_that("plot_footprint_sankey supports stage-specific grouping", {
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("jsonlite")

  footprints <- tibble::tribble(
    ~origin_area, ~origin_item, ~product, ~target_area, ~value,
    "Brazil", "Soybeans", "Pigmeat (China)", "China", 10,
    "Brazil", "Soybeans", "Milk (China)", "China", 8,
    "Brazil", "Soybeans", "Eggs (China)", "China", 6
  )
  file <- tempfile(fileext = ".html")

  plot_footprint_sankey(
    footprints,
    stages = c("origin_area", "origin_item", "product", "target_area"),
    stage_max_nodes = c(product = 1),
    stage_other_labels = c(product = "Other products"),
    file = file
  )

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  testthat::expect_match(html, "Other products", fixed = TRUE)
  testthat::expect_match(html, "\"max_nodes\":1", fixed = TRUE)
})

testthat::test_that("plot_footprint_sankey hard-limits embedded nodes", {
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("jsonlite")

  footprints <- tibble::tribble(
    ~origin_area, ~origin_item, ~target_item, ~target_area, ~value,
    "Alpha", "Soybeans", "Pigmeat", "China", 10,
    "Beta", "Soybeans", "Pigmeat", "China", 8,
    "Gamma", "Soybeans", "Pigmeat", "China", 1
  )
  file <- tempfile(fileext = ".html")

  plot_footprint_sankey(
    footprints,
    max_nodes = 1,
    embed_max_nodes = 2,
    stage_other_labels = c(origin_area = "Other origins"),
    file = file
  )

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  testthat::expect_match(html, "Other origins", fixed = TRUE)
  testthat::expect_no_match(html, "Gamma", fixed = TRUE)
  testthat::expect_match(html, "\"max_nodes\":1", fixed = TRUE)
  testthat::expect_match(html, "\"embed_max_nodes\":2", fixed = TRUE)
  testthat::expect_match(html, "input.max = String(hard)", fixed = TRUE)
})

testthat::test_that("plot_footprint_sankey rejects invalid NA options", {
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("jsonlite")

  footprints <- tibble::tribble(
    ~origin_area, ~origin_item, ~target_item, ~target_area, ~value,
    "Brazil", "Soybeans", "Pigmeat", "China", 10
  )

  testthat::expect_error(
    plot_footprint_sankey(footprints, max_nodes = NA_real_),
    "max_nodes"
  )
  testthat::expect_error(
    plot_footprint_sankey(footprints, min_share = NA_real_),
    "min_share"
  )
  testthat::expect_error(
    plot_footprint_sankey(footprints, embed_max_nodes = NA_real_),
    "embed_max_nodes"
  )
  testthat::expect_error(
    plot_footprint_sankey(
      footprints,
      stage_embed_max_nodes = c(origin_area = NA_real_)
    ),
    "stage_embed_max_nodes"
  )
  testthat::expect_error(
    plot_footprint_sankey(
      footprints,
      stage_other_labels = c(origin_area = NA_character_)
    ),
    "stage_other_labels"
  )
  testthat::expect_error(
    plot_footprint_sankey(footprints, file = NA_character_),
    "file"
  )
})
