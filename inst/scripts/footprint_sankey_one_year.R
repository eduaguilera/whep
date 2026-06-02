suppressPackageStartupMessages({
  devtools::load_all(".")
})

# Usage:
#   Rscript inst/scripts/footprint_sankey_one_year.R [year] [output_file]
#
# Optional environment variables:
#   WHEP_FOOTPRINT_YEAR         Year to run when no CLI year is supplied.
#   WHEP_FOOTPRINT_SANKEY_FILE  Output HTML path when no CLI file is supplied.
#   WHEP_FOOTPRINT_SANKEY_OPEN  Set to "true" to open the HTML viewer.
#   WHEP_FOOTPRINT_OUTPUT_TOL   Minimum sector output used in intensities.
#   WHEP_FOOTPRINT_VALUE_ADDED_FLOOR
#                                  Minimum non-intermediate leakage share.
#   WHEP_FOOTPRINT_MAX_RATIO    Maximum footprint/source-extension ratio.
#   WHEP_FOOTPRINT_SANKEY_MIN_SHARE
#                                  Initial browser path cutoff in percent.
#   WHEP_FOOTPRINT_SANKEY_MODE  "product" for FABIO-like product phase
#                                  or "basic" for origin -> target item.
#   WHEP_FOOTPRINT_PRODUCT_AREAS
#                                  Supplier/product areas kept separately.
#   WHEP_FOOTPRINT_SANKEY_PATH_MIN_SHARE
#                                  Drop embedded paths below this total share.

args <- commandArgs(trailingOnly = TRUE)

year <- if (length(args) >= 1) {
  as.integer(args[[1]])
} else {
  as.integer(Sys.getenv("WHEP_FOOTPRINT_YEAR", "1986"))
}

if (is.na(year)) {
  stop("`year` must be an integer.", call. = FALSE)
}

output_file <- if (length(args) >= 2) {
  args[[2]]
} else {
  Sys.getenv(
    "WHEP_FOOTPRINT_SANKEY_FILE",
    file.path(getwd(), paste0("footprint-sankey-", year, ".html"))
  )
}
output_dir <- dirname(output_file)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

open_viewer <- identical(
  tolower(Sys.getenv("WHEP_FOOTPRINT_SANKEY_OPEN", "false")),
  "true"
) || interactive()
output_tol <- as.numeric(Sys.getenv("WHEP_FOOTPRINT_OUTPUT_TOL", "1e-8"))
value_added_floor <- as.numeric(
  Sys.getenv("WHEP_FOOTPRINT_VALUE_ADDED_FLOOR", "1e-2")
)
max_ratio <- as.numeric(Sys.getenv("WHEP_FOOTPRINT_MAX_RATIO", "1.05"))
sankey_min_share <- as.numeric(
  Sys.getenv("WHEP_FOOTPRINT_SANKEY_MIN_SHARE", "0")
)
sankey_mode <- tolower(Sys.getenv("WHEP_FOOTPRINT_SANKEY_MODE", "product"))
product_areas <- as.numeric(Sys.getenv("WHEP_FOOTPRINT_PRODUCT_AREAS", "5"))
path_min_share <- as.numeric(
  Sys.getenv("WHEP_FOOTPRINT_SANKEY_PATH_MIN_SHARE", "1e-5")
)

if (is.na(output_tol) || output_tol < 0) {
  stop("`WHEP_FOOTPRINT_OUTPUT_TOL` must be non-negative.", call. = FALSE)
}
if (
  is.na(value_added_floor) ||
    !is.finite(value_added_floor) ||
    value_added_floor < 0 ||
    value_added_floor >= 1
) {
  stop(
    "`WHEP_FOOTPRINT_VALUE_ADDED_FLOOR` must be in [0, 1).",
    call. = FALSE
  )
}
if (is.na(max_ratio) || max_ratio <= 0) {
  stop("`WHEP_FOOTPRINT_MAX_RATIO` must be positive.", call. = FALSE)
}
if (is.na(sankey_min_share) || sankey_min_share < 0) {
  stop(
    "`WHEP_FOOTPRINT_SANKEY_MIN_SHARE` must be non-negative.",
    call. = FALSE
  )
}
if (!sankey_mode %in% c("product", "basic")) {
  stop(
    "`WHEP_FOOTPRINT_SANKEY_MODE` must be \"product\" or \"basic\".",
    call. = FALSE
  )
}
if (is.na(product_areas) || product_areas < 1 || !is.finite(product_areas)) {
  stop(
    "`WHEP_FOOTPRINT_PRODUCT_AREAS` must be a positive number.",
    call. = FALSE
  )
}
if (is.na(path_min_share) || path_min_share < 0) {
  stop(
    "`WHEP_FOOTPRINT_SANKEY_PATH_MIN_SHARE` must be non-negative.",
    call. = FALSE
  )
}

message("Building IO model for ", year, ".")
io <- build_io_model(years = year)
land_use <- get_land_fp_production()

labels <- io$labels[[1]]

extensions <- land_use |>
  dplyr::filter(year == !!year) |>
  dplyr::right_join(labels, by = c("area_code", "item_cbs_code")) |>
  tidyr::replace_na(list(impact_u = 0)) |>
  dplyr::arrange(index) |>
  dplyr::pull(impact_u)

message("Computing land footprint for ", year, ".")
footprints <- compute_footprint(
  z_mat = io$Z[[1]],
  x_vec = io$X[[1]],
  y_mat = io$Y[[1]],
  extensions = extensions,
  labels = labels,
  fd_labels = io$fd_labels[[1]],
  output_tol = output_tol,
  value_added_floor = value_added_floor
) |>
  dplyr::mutate(year = year) |>
  add_area_name(
    name_column = "origin_area_name",
    code_column = "origin_area"
  ) |>
  add_area_name(
    name_column = "target_area_name",
    code_column = "target_area"
  ) |>
  add_item_cbs_name(
    name_column = "origin_item_name",
    code_column = "origin_item"
  ) |>
  add_item_cbs_name(
    name_column = "target_item_name",
    code_column = "target_item"
  )

source_total <- sum(extensions[io$X[[1]] > output_tol], na.rm = TRUE)
footprint_total <- sum(footprints$value, na.rm = TRUE)
if (
  is.finite(source_total) &&
    source_total > 0 &&
    footprint_total > source_total * max_ratio
) {
  stop(
    paste(
      "Footprint result is numerically unstable:",
      paste0("source land extensions = ", signif(source_total, 4)),
      paste0("computed footprint = ", signif(footprint_total, 4)),
      paste0("ratio = ", signif(footprint_total / source_total, 4)),
      "The IO system likely contains near-singular sectors",
      "(technical coefficient columns with sums close to 1).",
      "Not writing a Sankey viewer because it would be misleading.",
      "If the output file already exists, treat it as stale.",
      sep = "\n"
    ),
    call. = FALSE
  )
}

message("Writing Sankey viewer to ", output_file, ".")
if (identical(sankey_mode, "product")) {
  message("Adding FABIO-like product-area stage.")
  footprints <- add_footprint_product_stage(
    footprints,
    y_mat = io$Y[[1]],
    labels = labels,
    fd_labels = io$fd_labels[[1]],
    max_product_areas = product_areas,
    other_area_name = "Other product areas",
    min_share = path_min_share
  ) |>
    add_item_cbs_name(
      name_column = "product_item_name",
      code_column = "product_item"
    ) |>
    dplyr::mutate(
      product_area_label = .data$product_area_name,
      product = paste0(
        .data$product_item_name,
        " (",
        .data$product_area_name,
        ")"
      )
    )
  stages <- c(
    "origin_area",
    "origin_item",
    "product_area_label",
    "product",
    "target_area",
    "target_fd"
  )
  subtitle <- paste0(
    "Origin areas and items traced through supplied products ",
    "to final-demand areas."
  )
  stage_max_nodes <- c(
    origin_area = 8,
    origin_item = 8,
    product_area_label = 12,
    product = 75,
    target_area = 12,
    target_fd = 3
  )
  stage_other_labels <- c(
    origin_area = "Other origin areas",
    origin_item = "Other origin items",
    product_area_label = "Other product areas",
    product = "Other products",
    target_area = "Other target areas",
    target_fd = "Other final demand"
  )
} else {
  stages <- NULL
  subtitle <- "Origin areas and items traced to final-demand areas and items."
  stage_max_nodes <- NULL
  stage_other_labels <- NULL
}

plot_footprint_sankey(
  footprints,
  stages = stages,
  file = output_file,
  open = open_viewer,
  max_nodes = 8,
  stage_max_nodes = stage_max_nodes,
  stage_other_labels = stage_other_labels,
  min_share = sankey_min_share,
  title = paste("Land Footprint Sankey", year),
  subtitle = subtitle,
  value_label = "ha"
)

message("Done.")
