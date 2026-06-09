suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
  library(Matrix)
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
})

# Reproduce aggregate FABIO land footprints for a few benchmark years and
# compare them with WHEP footprints. FABIO input files are downloaded to
# .fabio_compare if missing.

fabio_dir <- Sys.getenv("FABIO_DIR", ".fabio_compare")
years <- strsplit(
  Sys.getenv("FABIO_COMPARE_YEARS", "1986,2000,2013"),
  ",",
  fixed = TRUE
)[[1]] |>
  as.integer()
grassland_metric <- tolower(Sys.getenv("WHEP_GRASSLAND_METRIC", "occupation"))
usable_grass_yield_dm_t_ha <- as.numeric(
  Sys.getenv("WHEP_USABLE_GRASS_YIELD_DM_T_HA", "2.06")
)

if (!grassland_metric %in% c("occupation", "active_grazing", "both")) {
  stop(
    "`WHEP_GRASSLAND_METRIC` must be \"occupation\", \"active_grazing\", or \"both\".",
    call. = FALSE
  )
}
if (is.na(usable_grass_yield_dm_t_ha) || usable_grass_yield_dm_t_ha <= 0) {
  stop(
    "`WHEP_USABLE_GRASS_YIELD_DM_T_HA` must be a positive number.",
    call. = FALSE
  )
}

dir.create(fabio_dir, showWarnings = FALSE, recursive = TRUE)

fabio_base_url <- "https://zenodo.org/records/2577067/files"
fabio_files <- c(
  "regions.csv",
  "items.csv",
  "X.rds",
  "Y.rds",
  "E.rds",
  paste0(years, "_L_mass.rds")
)

download_fabio_file <- function(file) {
  path <- file.path(fabio_dir, file)
  if (file.exists(path) && file.info(path)$size > 0) {
    return(path)
  }
  url <- paste0(fabio_base_url, "/", file, "?download=1")
  message("Downloading ", url)
  download.file(url, path, mode = "wb", quiet = FALSE)
  path
}

invisible(vapply(fabio_files, download_fabio_file, character(1)))

eu28_iso3 <- c(
  "AUT",
  "BEL",
  "BGR",
  "HRV",
  "CYP",
  "CZE",
  "DNK",
  "EST",
  "FIN",
  "FRA",
  "DEU",
  "GRC",
  "HUN",
  "IRL",
  "ITA",
  "LVA",
  "LTU",
  "LUX",
  "MLT",
  "NLD",
  "POL",
  "PRT",
  "ROU",
  "SVK",
  "SVN",
  "ESP",
  "SWE",
  "GBR"
)

targets <- tibble(
  target = c("CHN", "USA", "EU28"),
  iso3c = list("CHN", "USA", eu28_iso3)
)

fd_levels <- c("food", "other")
product_classes <- c("plant", "animal")

class_from_group <- function(group) {
  case_when(
    group %in% c("Livestock", "Livestock products", "Fish") ~ "animal",
    group %in%
      c(
        "Primary crops",
        "Crop products",
        "Crop residues",
        "Grass",
        "Forestry"
      ) ~ "plant",
    TRUE ~ "other"
  )
}

fabio_regions <- read_csv(
  file.path(fabio_dir, "regions.csv"),
  show_col_types = FALSE
)
fabio_items <- read_csv(
  file.path(fabio_dir, "items.csv"),
  show_col_types = FALSE
)
fabio_x <- readRDS(file.path(fabio_dir, "X.rds"))
fabio_y <- readRDS(file.path(fabio_dir, "Y.rds"))
fabio_e <- readRDS(file.path(fabio_dir, "E.rds"))

n_reg <- nrow(fabio_regions)
n_item <- nrow(fabio_items)
fabio_index <- tibble(
  sector = seq_len(n_reg * n_item),
  area_code = rep(fabio_regions$code, each = n_item),
  iso3c = rep(fabio_regions$iso3c, each = n_item),
  item_code = rep(fabio_items$item_code, n_reg),
  item = rep(fabio_items$item, n_reg),
  group = rep(fabio_items$group, n_reg),
  comm_group = rep(fabio_items$comm_group, n_reg),
  product_class = class_from_group(group)
)

target_area_codes <- function(target_iso, regions = fabio_regions) {
  regions |>
    filter(.data$iso3c %in% target_iso) |>
    pull(.data$code)
}

make_fabio_demand <- function(y_mat, area_codes, fd, product_class) {
  cols <- paste0(area_codes, "_", fd)
  cols <- intersect(cols, colnames(y_mat))
  if (length(cols) == 0L) {
    return(rep(0, nrow(y_mat)))
  }

  demand <- Matrix::rowSums(y_mat[, cols, drop = FALSE])
  demand[fabio_index$product_class != product_class] <- 0
  as.numeric(demand)
}

calc_fabio_year <- function(year) {
  message("FABIO footprints: ", year)
  x_vec <- as.numeric(fabio_x[, as.character(year)])
  y_mat <- fabio_y[[as.character(year)]]
  e_year <- fabio_e[[as.character(year)]]
  l_mat <- readRDS(file.path(fabio_dir, paste0(year, "_L_mass.rds")))

  ext <- as.numeric(e_year$landuse) / x_vec
  ext[!is.finite(ext)] <- 0

  demands <- tidyr::expand_grid(
    targets,
    fd = fd_levels,
    product_class = product_classes
  ) |>
    mutate(
      area_codes = map(.data$iso3c, target_area_codes),
      demand = pmap(
        list(.data$area_codes, .data$fd, .data$product_class),
        ~ make_fabio_demand(y_mat, ..1, ..2, ..3)
      )
    )

  demand_mat <- do.call(cbind, demands$demand)
  required <- l_mat %*% demand_mat

  result <- map_dfr(seq_len(ncol(required)), function(j) {
    fp <- ext * as.numeric(required[, j])
    target_iso <- demands$iso3c[[j]]
    tibble(
      model = "FABIO",
      grassland_metric = NA_character_,
      extension_scope = "official_landuse",
      year = year,
      target = demands$target[[j]],
      fd = demands$fd[[j]],
      product_class = demands$product_class[[j]],
      target_area_count = length(demands$area_codes[[j]]),
      value = sum(fp, na.rm = TRUE),
      positive_value = sum(pmax(fp, 0), na.rm = TRUE),
      import_share = sum(
        fp[!fabio_index$iso3c %in% target_iso],
        na.rm = TRUE
      ) /
        sum(fp, na.rm = TRUE)
    )
  })

  rm(l_mat, required)
  gc()
  result
}

fabio_results <- map_dfr(years, calc_fabio_year)

land_use <- get_land_fp_production(
  grassland_metric = grassland_metric,
  usable_grass_yield_dm_t_ha = usable_grass_yield_dm_t_ha
)
land_use_scope_names <- unique(land_use$extension_scope)

whep_item_groups <- whep::items_full |>
  distinct(.data$item_cbs_code, .data$group)

whep_area_codes <- function(target_iso) {
  whep::polities |>
    filter(.data$iso3c %in% target_iso) |>
    pull(.data$area_code)
}

make_whep_demand <- function(
  y_mat,
  fd_labels,
  labels,
  area_codes,
  fd,
  product_class
) {
  whep_fd <- if (identical(fd, "other")) "other_uses" else fd
  cols <- which(
    fd_labels$area_code %in% area_codes & fd_labels$fd_col == whep_fd
  )
  if (length(cols) == 0L) {
    return(rep(0, nrow(y_mat)))
  }

  demand <- Matrix::rowSums(y_mat[, cols, drop = FALSE])
  demand[labels$product_class != product_class] <- 0
  as.numeric(demand)
}

whep_extension_scopes <- function(extensions, labels) {
  list(
    current_all_land_fp = extensions,
    no_livestock_products = ifelse(
      labels$group == "Livestock products",
      0,
      extensions
    ),
    primary_crops_grass = ifelse(
      labels$group %in% c("Primary crops", "Grass"),
      extensions,
      0
    ),
    crop_and_grass_direct = ifelse(
      labels$group %in%
        c(
          "Primary crops",
          "Crop products",
          "Crop residues",
          "Grass"
        ),
      extensions,
      0
    ),
    crops_only = ifelse(
      labels$group %in% c("Primary crops", "Crop products", "Crop residues"),
      extensions,
      0
    )
  )
}

calc_whep_year <- function(io_row) {
  year <- io_row$year
  message("WHEP footprints: ", year)

  z_mat <- io_row$Z[[1]]
  x_vec <- io_row$X[[1]]
  y_mat <- io_row$Y[[1]]
  fd_labels <- io_row$fd_labels[[1]]
  labels <- io_row$labels[[1]] |>
    left_join(whep_item_groups, by = "item_cbs_code") |>
    mutate(product_class = class_from_group(.data$group))

  a_mat <- whep:::.technical_coefficients(
    z_mat,
    x_vec,
    value_added_floor = 1e-3
  )
  ia <- Matrix::Diagonal(length(x_vec)) - a_mat
  lu_fact <- whep:::.factor_ia(ia)

  demands <- tidyr::expand_grid(
    targets,
    fd = fd_levels,
    product_class = product_classes
  ) |>
    mutate(
      area_codes = map(.data$iso3c, whep_area_codes),
      area_codes = map(.data$area_codes, ~ intersect(.x, fd_labels$area_code)),
      demand = pmap(
        list(.data$area_codes, .data$fd, .data$product_class),
        ~ make_whep_demand(y_mat, fd_labels, labels, ..1, ..2, ..3)
      )
    )

  demand_mat <- do.call(cbind, demands$demand)
  required <- Matrix::solve(lu_fact, demand_mat)

  result <- map_dfr(land_use_scope_names, function(land_metric) {
    extensions <- land_use |>
      filter(
        .data$year == !!year,
        .data$extension_scope == !!land_metric
      ) |>
      right_join(
        labels,
        by = c("area_code", "item_cbs_code")
      ) |>
      arrange(.data$index) |>
      mutate(impact_u = tidyr::replace_na(.data$impact_u, 0)) |>
      pull(.data$impact_u)

    scopes <- whep_extension_scopes(extensions, labels)

    imap_dfr(scopes, function(scope_ext, scope_name) {
      intensity <- ifelse(x_vec <= 1e-8, 0, scope_ext / x_vec)
      intensity[!is.finite(intensity)] <- 0

      map_dfr(seq_len(ncol(required)), function(j) {
        fp <- intensity * as.numeric(required[, j])
        target_codes <- demands$area_codes[[j]]
        tibble(
          model = "WHEP",
          grassland_metric = land_metric,
          extension_scope = scope_name,
          year = year,
          target = demands$target[[j]],
          fd = demands$fd[[j]],
          product_class = demands$product_class[[j]],
          target_area_count = length(target_codes),
          value = sum(fp, na.rm = TRUE),
          positive_value = sum(pmax(fp, 0), na.rm = TRUE),
          import_share = sum(
            fp[!labels$area_code %in% target_codes],
            na.rm = TRUE
          ) /
            sum(fp, na.rm = TRUE)
        )
      })
    })
  })

  rm(lu_fact, required)
  gc()
  result
}

io <- build_io_model(years = years)
whep_results <- map_dfr(seq_len(nrow(io)), ~ calc_whep_year(io[.x, ]))

results <- bind_rows(fabio_results, whep_results)

comparison <- whep_results |>
  left_join(
    fabio_results |>
      select(
        "year",
        "target",
        "fd",
        "product_class",
        fabio_value = "value",
        fabio_positive_value = "positive_value",
        fabio_import_share = "import_share"
      ),
    by = c("year", "target", "fd", "product_class")
  ) |>
  mutate(
    ratio = .data$value / .data$fabio_value,
    positive_ratio = .data$positive_value / .data$fabio_positive_value,
    import_share_diff = .data$import_share - .data$fabio_import_share
  )

fabio_extension_totals <- map_dfr(years, function(year) {
  e_year <- fabio_e[[as.character(year)]]
  e_year |>
    as_tibble() |>
    summarise(
      value = sum(.data$landuse, na.rm = TRUE),
      .by = c("group", "comm_group")
    ) |>
    mutate(
      model = "FABIO",
      grassland_metric = NA_character_,
      year = year,
      source_column = "landuse"
    )
})

whep_extension_totals <- land_use |>
  filter(.data$year %in% years) |>
  summarise(
    value = sum(.data$impact_u, na.rm = TRUE),
    .by = c("extension_scope", "year", "group")
  ) |>
  rename(grassland_metric = "extension_scope") |>
  mutate(
    model = "WHEP",
    comm_group = NA_character_,
    source_column = "impact_u"
  )

extension_totals <- bind_rows(
  fabio_extension_totals,
  whep_extension_totals
) |>
  select(
    "model",
    "grassland_metric",
    "year",
    "group",
    "comm_group",
    "source_column",
    "value"
  )

write_csv(results, file.path(fabio_dir, "footprint_aggregates.csv"))
write_csv(comparison, file.path(fabio_dir, "footprint_comparison.csv"))
write_csv(extension_totals, file.path(fabio_dir, "extension_totals.csv"))

message("\nComparison summary by WHEP extension scope:")
comparison |>
  summarise(
    median_ratio = median(.data$ratio, na.rm = TRUE),
    min_ratio = min(.data$ratio, na.rm = TRUE),
    max_ratio = max(.data$ratio, na.rm = TRUE),
    median_import_share_diff = median(.data$import_share_diff, na.rm = TRUE),
    .by = c("grassland_metric", "extension_scope")
  ) |>
  arrange(.data$grassland_metric, .data$median_ratio) |>
  print(n = Inf)

message("\nLargest current WHEP/FABIO ratios:")
comparison |>
  filter(.data$extension_scope == "current_all_land_fp") |>
  arrange(desc(.data$ratio)) |>
  select(
    "grassland_metric",
    "year",
    "target",
    "fd",
    "product_class",
    "value",
    "fabio_value",
    "ratio",
    "import_share",
    "fabio_import_share"
  ) |>
  print(n = 20)

message("\nWrote:")
message("  ", file.path(fabio_dir, "footprint_aggregates.csv"))
message("  ", file.path(fabio_dir, "footprint_comparison.csv"))
message("  ", file.path(fabio_dir, "extension_totals.csv"))
