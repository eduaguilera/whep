suppressPackageStartupMessages({
  devtools::load_all(".")
})

# Usage:
#   Rscript inst/scripts/footprint_sankey_first_use_one_origin.R \
#     [year] [origin_area] [origin_item] [output_file]
#
# Examples:
#   Rscript inst/scripts/footprint_sankey_first_use_one_origin.R \
#     2000 Brazil Soyabeans footprint-sankey-2000-brazil-soyabeans-first-use.html
#
# Optional environment variables:
#   WHEP_FOOTPRINT_YEAR
#   WHEP_FOOTPRINT_ORIGIN_AREA
#   WHEP_FOOTPRINT_ORIGIN_ITEM
#   WHEP_FOOTPRINT_SANKEY_FILE
#   WHEP_FOOTPRINT_SANKEY_OPEN
#   WHEP_FOOTPRINT_OUTPUT_TOL
#   WHEP_FOOTPRINT_VALUE_ADDED_FLOOR
#   WHEP_FOOTPRINT_SANKEY_MIN_SHARE
#   WHEP_FOOTPRINT_SANKEY_MAX_NODES

.resolve_sankey_area <- function(value) {
  if (grepl("^[0-9]+$", value)) {
    return(as.integer(value))
  }
  lookup <- whep::regions_full |>
    dplyr::select(area_code = code, area_name = name)
  matched <- lookup |>
    dplyr::filter(tolower(.data$area_name) == tolower(value))
  if (nrow(matched) != 1) {
    stop("Could not resolve origin area `", value, "`.", call. = FALSE)
  }
  as.integer(matched$area_code[[1]])
}

.resolve_sankey_item <- function(value) {
  if (grepl("^[0-9]+$", value)) {
    return(as.integer(value))
  }
  matched <- whep::items_cbs |>
    dplyr::filter(tolower(.data$item_cbs_name) == tolower(value))
  if (nrow(matched) != 1) {
    stop("Could not resolve origin item `", value, "`.", call. = FALSE)
  }
  as.integer(matched$item_cbs_code[[1]])
}

.sankey_area_name <- function(area_code) {
  value <- whep::regions_full |>
    dplyr::filter(.data$code == area_code) |>
    dplyr::pull(.data$name)
  value[1]
}

.sankey_item_name <- function(item_code) {
  value <- whep::items_cbs |>
    dplyr::filter(.data$item_cbs_code == item_code) |>
    dplyr::pull(.data$item_cbs_name)
  value[1]
}

.sankey_slug <- function(value) {
  value |>
    tolower() |>
    stringr::str_replace_all("[^a-z0-9]+", "-") |>
    stringr::str_replace_all("(^-|-$)", "")
}

args <- commandArgs(trailingOnly = TRUE)

year <- if (length(args) >= 1) {
  as.integer(args[[1]])
} else {
  as.integer(Sys.getenv("WHEP_FOOTPRINT_YEAR", "2000"))
}
if (is.na(year)) {
  stop("`year` must be an integer.", call. = FALSE)
}

origin_area_arg <- if (length(args) >= 2) {
  args[[2]]
} else {
  Sys.getenv("WHEP_FOOTPRINT_ORIGIN_AREA", "Brazil")
}
origin_item_arg <- if (length(args) >= 3) {
  args[[3]]
} else {
  Sys.getenv("WHEP_FOOTPRINT_ORIGIN_ITEM", "Soyabeans")
}

origin_area <- .resolve_sankey_area(origin_area_arg)
origin_item <- .resolve_sankey_item(origin_item_arg)
origin_area_name <- .sankey_area_name(origin_area)
origin_item_name <- .sankey_item_name(origin_item)

default_file <- file.path(
  getwd(),
  paste0(
    "footprint-sankey-",
    year,
    "-",
    .sankey_slug(origin_area_name),
    "-",
    .sankey_slug(origin_item_name),
    "-first-use.html"
  )
)
output_file <- if (length(args) >= 4) {
  args[[4]]
} else {
  Sys.getenv("WHEP_FOOTPRINT_SANKEY_FILE", default_file)
}
output_dir <- dirname(output_file)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

open_viewer <- identical(
  tolower(Sys.getenv("WHEP_FOOTPRINT_SANKEY_OPEN", "false")),
  "true"
) ||
  interactive()
output_tol <- as.numeric(Sys.getenv("WHEP_FOOTPRINT_OUTPUT_TOL", "1e-8"))
value_added_floor <- as.numeric(
  Sys.getenv("WHEP_FOOTPRINT_VALUE_ADDED_FLOOR", "1e-2")
)
sankey_min_share <- as.numeric(
  Sys.getenv("WHEP_FOOTPRINT_SANKEY_MIN_SHARE", "0")
)
sankey_max_nodes <- as.numeric(
  Sys.getenv("WHEP_FOOTPRINT_SANKEY_MAX_NODES", "10")
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
if (is.na(sankey_min_share) || sankey_min_share < 0) {
  stop(
    "`WHEP_FOOTPRINT_SANKEY_MIN_SHARE` must be non-negative.",
    call. = FALSE
  )
}
if (
  is.na(sankey_max_nodes) ||
    sankey_max_nodes < 1 ||
    !is.finite(sankey_max_nodes)
) {
  stop(
    "`WHEP_FOOTPRINT_SANKEY_MAX_NODES` must be a positive number.",
    call. = FALSE
  )
}

message(
  "Building IO model for ",
  year,
  " and first-use paths for ",
  origin_item_name,
  " from ",
  origin_area_name,
  "."
)
io <- build_io_model(years = year)
land_use <- get_land_fp_production()
labels <- io$labels[[1]]

extensions <- land_use |>
  dplyr::filter(year == !!year) |>
  dplyr::right_join(labels, by = c("area_code", "item_cbs_code")) |>
  tidyr::replace_na(list(impact_u = 0)) |>
  dplyr::arrange(index) |>
  dplyr::pull(impact_u)

paths <- compute_footprint_paths(
  z_mat = io$Z[[1]],
  x_vec = io$X[[1]],
  y_mat = io$Y[[1]],
  extensions = extensions,
  labels = labels,
  fd_labels = io$fd_labels[[1]],
  origin_area = origin_area,
  origin_item = origin_item,
  output_tol = output_tol,
  value_added_floor = value_added_floor
) |>
  dplyr::mutate(year = year) |>
  add_area_name(
    name_column = "origin_area_name",
    code_column = "origin_area"
  ) |>
  add_area_name(
    name_column = "use_area_name",
    code_column = "use_area"
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
    name_column = "use_item_name",
    code_column = "use_item"
  ) |>
  add_item_cbs_name(
    name_column = "target_item_name",
    code_column = "target_item"
  ) |>
  dplyr::mutate(
    origin = paste0(.data$origin_item_name, " (", .data$origin_area_name, ")"),
    first_use_area = .data$use_area_name,
    first_use_item = paste0(
      .data$use_item_name,
      " (",
      .data$use_area_name,
      ")"
    ),
    final_demand_area = .data$target_area_name
  )

if (nrow(paths) == 0) {
  stop("No positive first-use footprint paths matched the requested origin.")
}

message("Writing first-use Sankey viewer to ", output_file, ".")
plot_footprint_sankey(
  paths,
  stages = c(
    "origin",
    "first_use_area",
    "first_use_item",
    "final_demand_area"
  ),
  file = output_file,
  open = open_viewer,
  max_nodes = sankey_max_nodes,
  min_share = sankey_min_share,
  title = paste("Land Footprint First-use Sankey", year),
  subtitle = paste0(
    origin_item_name,
    " from ",
    origin_area_name,
    " traced through first direct use to final-demand areas."
  ),
  value_label = "ha"
)

message("Done.")
