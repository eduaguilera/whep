# Compare WHEP io_model output against official FABIO matrices
#
# Official FABIO data (Bruckner et al., 2019) is available on Zenodo.
#
# Data structure of the FABIO RDS files
# --------------------------------------
# X.rds  — matrix [24000 rows × 28 years], one column per year (1986-2013).
#           Rows are area-blocked: 192 areas × 125 commodities, in the order
#           defined by io_codes.csv (area 1 items c001-c125, area 2 items
#           c001-c125, …).  Row names repeat c001-c125 for every area.
#
# Z.rds  — list of 28 sparse matrices, one per year, or a single 24000×24000
#           matrix for a specific year (format may vary by Zenodo version).
#
# Y.rds  — similar to Z, but columns are final demand categories.
#
# Sector labels come from io_codes.csv (NOT items_full.csv — the latter has a
# systematic off-by-one shift in item_code from c066 onwards and should not
# be used for the comm_code → item_cbs_code mapping).
#
# Usage
# -----
#   1. Build your io_model:  io <- build_io_model(years = YEAR)
#   2. Load FABIO X:         X  <- readRDS("~/Downloads/X.rds")
#   3. Set YEAR and paths below, then source this script.

library(dplyr)
library(Matrix)


# --------------------------------------------------------------------------- #
# Configuration — edit these                                                    #
# --------------------------------------------------------------------------- #

YEAR <- 2010

io <- build_io_model(years = YEAR)
# https://zenodo.org/records/2577067/files/X.rds?download=1
X <- readRDS("~/Downloads/X.rds")

# Path to io_codes.csv (from the FABIO Zenodo repository)
# https://zenodo.org/records/2577067/files/io_codes.csv?download=1
IO_CODES_PATH <- "~/Downloads/io_codes.csv"

# Optional: per-year Z and Y matrices. Set to NULL to skip those sections.
# These are typically large — start with just X.
FABIO_Z_PATH <- NULL # e.g. paste0("~/Downloads/", YEAR, "_Z.rds")
FABIO_Y_PATH <- NULL # e.g. paste0("~/Downloads/", YEAR, "_Y.rds")

# --------------------------------------------------------------------------- #
# Helpers                                                                       #
# --------------------------------------------------------------------------- #

# WHEP deliberately splits or renames some commodities that FABIO keeps as one
# row. Compare after rolling WHEP sectors up to FABIO commodity definitions.
.build_whep_fabio_bridge <- function() {
  tibble::tribble(
    ~item_cbs_code, ~fabio_item_code,
    960L, 866L,   # Cattle, dairy -> Cattle
    961L, 866L,   # Cattle, non-dairy -> Cattle
    1049L, 1034L, # Pigs -> Pigs
    1052L, 2029L, # Chickens, layers -> Poultry Birds
    1053L, 2029L, # Chickens, broilers -> Poultry Birds
    1068L, 2029L, # Ducks -> Poultry Birds
    1072L, 2029L, # Geese -> Poultry Birds
    1079L, 2029L, # Turkeys -> Poultry Birds
    2001L, 2000L, # Fodder legumes -> Fodder crops
    2002L, 2000L, # Fodder vegetables and roots -> Fodder crops
    2003L, 2000L, # Fodder mix -> Fodder crops
    5003L, 2000L, # Other fodder -> Fodder crops
    3000L, 2001L, # Grassland -> Grazing
    3002L, 2001L, # Temporary grassland -> Grazing
    248L, 2560L,  # Coconuts -> Coconuts - Incl Copra
    2112L, 2749L, # Meat Meal -> Meat Meal
    2542L, 2818L, # Sugar (Raw Equivalent) -> Sugar, Refined Equiv
    2552L, 2556L, # Groundnuts -> Groundnuts (Shelled Eq)
    2761L, 2960L, # Freshwater Fish -> Fish, Seafood
    2762L, 2960L, # Demersal Fish -> Fish, Seafood
    2763L, 2960L, # Pelagic Fish -> Fish, Seafood
    2764L, 2960L, # Marine Fish, Other -> Fish, Seafood
    2765L, 2960L, # Crustaceans -> Fish, Seafood
    2766L, 2960L, # Cephalopods -> Fish, Seafood
    2767L, 2960L, # Molluscs, Other -> Fish, Seafood
    2769L, 2960L, # Aquatic Animals, Others -> Fish, Seafood
    2775L, 2960L, # Aquatic Plants -> Fish, Seafood
    2781L, 2960L, # Fish, Body Oil -> Fish, Seafood
    2782L, 2960L, # Fish, Liver Oil -> Fish, Seafood
    2790L, 2960L, # Fish meal -> Fish, Seafood
    2807L, 2805L  # Rice and products -> Rice (Milled Equivalent)
  )
}

# Extract the single-year slice from the WHEP io tibble.
.slice_year <- function(io, year) {
  row <- io[io$year == year, ]
  if (nrow(row) != 1L) {
    stop("Year ", year, " not found in io model output.")
  }
  list(
    Z = row$Z[[1L]],
    Y = row$Y[[1L]],
    X = row$X[[1L]],
    labels = row$labels[[1L]]
  )
}

# Build the FABIO sector label table from io_codes.csv.
# Returns a data frame with one row per FABIO matrix row:
#   area_code, fabio_item_code, fabio_item_name, comm_code
.build_fabio_labels <- function(io_codes_path) {
  io <- read.csv(io_codes_path, stringsAsFactors = FALSE)
  # io_codes rows are in the same order as the X matrix rows.
  # item_code in io_codes is the official FABIO comparison code.
  data.frame(
    area_code = as.integer(io$area_code),
    fabio_item_code = as.integer(io$item_code),
    fabio_item_name = io$item,
    comm_code = io$comm_code,
    stringsAsFactors = FALSE
  )
}

.map_whep_labels_to_fabio <- function(labels, bridge, fabio_items) {
  labels |>
    left_join(bridge, by = "item_cbs_code") |>
    mutate(
      fabio_item_code = coalesce(
        .data$fabio_item_code,
        as.integer(.data$item_cbs_code)
      )
    ) |>
    left_join(fabio_items, by = "fabio_item_code")
}

.build_primary_double_systems <- function(fabio_items) {
  item_lookup <- whep::items_prod_full |>
    transmute(
      item_prod_code_chr = as.character(.data$item_prod_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      item_cbs_name = .data$item_cbs
    )

  primary_double <- whep::primary_double |>
    mutate(
      item_prod_code_chr = as.character(.data$item_prod_code),
      primary_double_family = coalesce(.data$Item_area, .data$item_prod),
      primary_double_role = if_else(
        grepl("_area$", .data$Multi_type),
        "area",
        "product"
      )
    ) |>
    left_join(item_lookup, by = "item_prod_code_chr")

  parents <- primary_double |>
    filter(.data$primary_double_role == "area") |>
    transmute(
      primary_double_family = .data$primary_double_family,
      parent_item_code = .data$item_cbs_code,
      parent_item_name = .data$item_cbs_name,
      parent_multi_type = .data$Multi_type
    )

  products <- primary_double |>
    filter(.data$primary_double_role == "product") |>
    transmute(
      primary_double_family = .data$primary_double_family,
      product_item_code = .data$item_cbs_code,
      product_item_name = .data$item_cbs_name,
      product_multi_type = .data$Multi_type
    )

  systems <- inner_join(products, parents, by = "primary_double_family")
  fabio_codes <- fabio_items$fabio_item_code

  systems |>
    group_by(
      .data$primary_double_family,
      .data$parent_item_code,
      .data$parent_item_name,
      .data$parent_multi_type
    ) |>
    filter(
      .data$parent_multi_type == "Primary_area",
      .data$parent_item_code %in% fabio_codes,
      all(.data$product_item_code %in% fabio_codes)
    ) |>
    ungroup()
}

.mark_primary_double_boundary_items <- function(labels, primary_double_systems) {
  parent_codes <- primary_double_systems |>
    distinct(.data$parent_item_code) |>
    pull(.data$parent_item_code)

  if (length(parent_codes) == 0L) {
    labels$compare_include <- TRUE
    return(labels)
  }

  labels |>
    mutate(
      compare_include = !.data$fabio_item_code %in% parent_codes
    )
}

.print_primary_double_audit <- function(
  ref_vec,
  ref_labels,
  our_vec,
  our_labels,
  primary_double_systems
) {
  if (nrow(primary_double_systems) == 0L) {
    return(invisible(NULL))
  }

  item_totals <- function(values, labels, value_name) {
    labels |>
      mutate(value = as.numeric(values)) |>
      summarise(
        value = sum(.data$value, na.rm = TRUE),
        .by = "fabio_item_code"
      ) |>
      rename(!!value_name := "value")
  }

  ref_totals <- item_totals(ref_vec, ref_labels, "fabio_x")
  our_totals <- item_totals(our_vec, our_labels, "whep_x")

  parents <- primary_double_systems |>
    distinct(
      .data$primary_double_family,
      .data$parent_item_code,
      .data$parent_item_name
    ) |>
    left_join(ref_totals, by = c("parent_item_code" = "fabio_item_code")) |>
    rename(fabio_parent_x = "fabio_x")

  product_names <- primary_double_systems |>
    distinct(
      .data$primary_double_family,
      .data$product_item_code,
      .data$product_item_name
    ) |>
    summarise(
      product_items = paste(
        paste(.data$product_item_code, .data$product_item_name),
        collapse = " + "
      ),
      .by = "primary_double_family"
    )

  fabio_products <- primary_double_systems |>
    distinct(.data$primary_double_family, .data$product_item_code) |>
    left_join(ref_totals, by = c("product_item_code" = "fabio_item_code")) |>
    summarise(
      fabio_products_x = sum(coalesce(.data$fabio_x, 0), na.rm = TRUE),
      .by = "primary_double_family"
    )

  whep_products <- primary_double_systems |>
    distinct(.data$primary_double_family, .data$product_item_code) |>
    left_join(our_totals, by = c("product_item_code" = "fabio_item_code")) |>
    summarise(
      whep_products_x = sum(coalesce(.data$whep_x, 0), na.rm = TRUE),
      .by = "primary_double_family"
    )

  audit <- parents |>
    left_join(product_names, by = "primary_double_family") |>
    left_join(fabio_products, by = "primary_double_family") |>
    left_join(whep_products, by = "primary_double_family") |>
    mutate(
      product_ratio = .data$whep_products_x /
        pmax(.data$fabio_products_x, 1)
    ) |>
    select(
      "primary_double_family",
      "parent_item_code",
      "parent_item_name",
      "product_items",
      "fabio_parent_x",
      "fabio_products_x",
      "whep_products_x",
      "product_ratio"
    )

  message(
    "Primary-double boundary audit: parent X is excluded from the adjusted ",
    "comparison; product X remains compared directly."
  )
  print(audit, width = Inf)

  invisible(audit)
}

.aggregate_compare_vector <- function(values, labels, value_name) {
  stopifnot(length(values) == nrow(labels))
  if (!"compare_include" %in% names(labels)) {
    labels$compare_include <- TRUE
  }
  labels |>
    mutate(value = as.numeric(values)) |>
    filter(.data$compare_include) |>
    summarise(
      value = sum(.data$value, na.rm = TRUE),
      fabio_item_name = dplyr::first(.data$fabio_item_name),
      .by = c(area_code, fabio_item_code)
    ) |>
    rename(!!value_name := "value")
}

# Join two vectors by (area_code, fabio_item_code), after WHEP rollups.
# Returns: area_code, fabio_item_code, fabio_item_name, ref, our, and errors.
.compare_vectors <- function(ref_vec, ref_labels, our_vec, our_labels) {
  ref_df <- .aggregate_compare_vector(ref_vec, ref_labels, "ref")
  our_df <- .aggregate_compare_vector(our_vec, our_labels, "our")

  inner_join(
    ref_df,
    our_df,
    by = c("area_code", "fabio_item_code"),
    suffix = c("", "_our")
  ) |>
    mutate(
      abs_err = our - ref,
      rel_err = abs_err / pmax(abs(ref), 1)
    ) |>
    select(
      "area_code",
      "fabio_item_code",
      "fabio_item_name",
      "ref",
      "our",
      "abs_err",
      "rel_err"
    )
}

# Print summary statistics for a comparison data frame.
.print_summary <- function(label, cmp) {
  smry <- cmp |>
    summarise(
      median_rel_err = median(abs(.data$rel_err)),
      p90_rel_err = quantile(abs(.data$rel_err), 0.9),
      p99_rel_err = quantile(abs(.data$rel_err), 0.99),
      frac_within_5pct = mean(abs(.data$rel_err) < 0.05),
      frac_within_20pct = mean(abs(.data$rel_err) < 0.20),
      total_ref = sum(.data$ref),
      total_our = sum(.data$our),
      total_rel_diff = (sum(.data$our) - sum(.data$ref)) / sum(.data$ref)
    )
  message(sprintf(
    paste0(
      "  %s — median |rel err|: %.1f%%  P90: %.1f%%  P99: %.1f%%",
      "  within5%%: %.0f%%  within20%%: %.0f%%"
    ),
    label,
    smry$median_rel_err * 100,
    smry$p90_rel_err * 100,
    smry$p99_rel_err * 100,
    smry$frac_within_5pct * 100,
    smry$frac_within_20pct * 100
  ))
  message(sprintf(
    "  %s — FABIO: %.3e   WHEP: %.3e   rel diff: %+.1f%%",
    label,
    smry$total_ref,
    smry$total_our,
    smry$total_rel_diff * 100
  ))
}

# Print top-N mismatches with human-readable names.
.print_top_mismatches <- function(cmp, n = 20, label = "") {
  top <- cmp |>
    arrange(desc(abs(.data$rel_err))) |>
    slice_head(n = n) |>
    add_area_name() |>
    select(
      "area_name",
      "fabio_item_code",
      "fabio_item_name",
      "ref",
      "our",
      "rel_err"
    )
  message(sprintf("\nTop %d %s mismatches (by |rel_err|):", n, label))
  print(top)
}

# Load a FABIO matrix RDS, extract a single year if stored as a named list,
# compare row sums (and optionally col sums) against the WHEP matrix, then
# free memory.
.compare_matrix <- function(
  path,
  label,
  our_mat,
  fabio_labels,
  our_labels,
  yr_col,
  col_sums = FALSE
) {
  if (is.null(path)) {
    return(invisible(NULL))
  }

  message(sprintf("\n=== %s ===", label))

  mat <- readRDS(path)
  if (is.list(mat) && !is.null(names(mat))) {
    mat <- mat[[yr_col]]
  }

  rs_cmp <- .compare_vectors(
    rowSums(mat),
    fabio_labels,
    rowSums(our_mat),
    our_labels
  )
  .print_summary(paste(label, "row sums"), rs_cmp)
  .print_top_mismatches(rs_cmp, n = 20, label = paste(label, "row-sum"))

  if (col_sums) {
    cs_cmp <- .compare_vectors(
      colSums(mat),
      fabio_labels,
      colSums(our_mat),
      our_labels
    )
    .print_summary(paste(label, "col sums"), cs_cmp)
  }

  rm(mat)
}

# --------------------------------------------------------------------------- #
# Load data                                                                     #
# --------------------------------------------------------------------------- #

message("Building FABIO sector label table from io_codes.csv ...")
fabio_labels <- .build_fabio_labels(IO_CODES_PATH)
fabio_items <- distinct(
  fabio_labels,
  .data$fabio_item_code,
  .data$fabio_item_name
)
message(sprintf(
  "  %d FABIO sectors (%d unique comm_codes, %d unique areas)",
  nrow(fabio_labels),
  length(unique(fabio_labels$comm_code)),
  length(unique(fabio_labels$area_code))
))

# X must already be in scope (24000 × 28 matrix, columns = year as character).
stopifnot(exists("X"), is.matrix(X))
yr_col <- as.character(YEAR)
if (!yr_col %in% colnames(X)) {
  stop("Year ", YEAR, " not found in X matrix columns.")
}
fabio_x <- X[, yr_col]

# io must be in scope — built with: io <- build_io_model(years = YEAR)
stopifnot(exists("io"))
our <- .slice_year(io, YEAR)
bridge <- .build_whep_fabio_bridge()
our$labels <- .map_whep_labels_to_fabio(our$labels, bridge, fabio_items)
primary_double_systems <- .build_primary_double_systems(fabio_items)
fabio_labels_pd <- .mark_primary_double_boundary_items(
  fabio_labels,
  primary_double_systems
)
our_labels_pd <- .mark_primary_double_boundary_items(
  our$labels,
  primary_double_systems
)
message(sprintf("  %d WHEP sectors", length(our$X)))

# --------------------------------------------------------------------------- #
# Coverage report                                                               #
# --------------------------------------------------------------------------- #

message("\n=== Coverage ===")

rollups <- our$labels |>
  distinct(
    .data$item_cbs_code,
    .data$fabio_item_code,
    .data$fabio_item_name
  ) |>
  filter(.data$item_cbs_code != .data$fabio_item_code) |>
  add_item_cbs_name()
if (nrow(rollups) > 0) {
  message(sprintf(
    "  %d WHEP item code(s) roll up to a different FABIO code:",
    nrow(rollups)
  ))
  print(arrange(rollups, .data$fabio_item_code, .data$item_cbs_code))
}

if (nrow(primary_double_systems) > 0) {
  boundary_items <- primary_double_systems |>
    distinct(
      .data$primary_double_family,
      .data$parent_item_code,
      .data$parent_item_name
    )
  message(
    "  Primary-double parent item(s) treated as collapsed process ",
    "boundaries in the adjusted comparison:"
  )
  print(boundary_items, width = Inf)
}

our_items <- distinct(our$labels, .data$fabio_item_code, .data$fabio_item_name)

only_fabio_items <- anti_join(fabio_items, our_items, by = "fabio_item_code")
if (nrow(only_fabio_items) > 0) {
  message(
    sprintf(
      "  %d FABIO commodity code(s) with no mapped WHEP equivalent",
      nrow(only_fabio_items)
    ),
    " (dropped from comparison):"
  )
  print(only_fabio_items)
}

only_our_items <- our$labels |>
  distinct(.data$item_cbs_code, .data$fabio_item_code) |>
  anti_join(fabio_items, by = "fabio_item_code") |>
  add_item_cbs_name()
if (nrow(only_our_items) > 0) {
  message(sprintf(
    "  %d WHEP item(s) with no mapped FABIO equivalent:",
    nrow(only_our_items)
  ))
  print(only_our_items)
}

# --------------------------------------------------------------------------- #
# 1. Compare X (total output vector)                                            #
# --------------------------------------------------------------------------- #
#
# NOTE on live animal units:
#   Both FABIO and WHEP store live animal X in HEADS (number of animals
#   slaughtered per year for meat animals; stocks for draft animals).
#   However, the production estimates may differ due to different data
#   adjustments (FABIO adjusts production = slaughtered + exported - imported).

message("\n=== 1. Total output (X) ===")

x_cmp <- .compare_vectors(
  fabio_x,
  fabio_labels,
  our$X,
  our$labels
)

message(sprintf("  Matched %d sectors.", nrow(x_cmp)))
.print_summary("X", x_cmp)
.print_top_mismatches(x_cmp, n = 20, label = "X")

x_cmp_full <- x_cmp |> add_area_name()

# --------------------------------------------------------------------------- #
# 1a. X with primary-double boundary items removed                             #
# --------------------------------------------------------------------------- #

if (nrow(primary_double_systems) > 0) {
  message("\n=== 1a. Total output (X), primary-double adjusted ===")
  .print_primary_double_audit(
    fabio_x,
    fabio_labels,
    our$X,
    our$labels,
    primary_double_systems
  )

  x_cmp_pd <- .compare_vectors(
    fabio_x,
    fabio_labels_pd,
    our$X,
    our_labels_pd
  )

  message(sprintf("  Matched %d sectors.", nrow(x_cmp_pd)))
  .print_summary("X, primary-double adjusted", x_cmp_pd)
  .print_top_mismatches(x_cmp_pd, n = 20, label = "X, primary-double adjusted")
} else {
  x_cmp_pd <- x_cmp
}

# --------------------------------------------------------------------------- #
# 1b. X — breakdown by item (detect systematic scale factors)                  #
# --------------------------------------------------------------------------- #

message("\n=== 1b. X breakdown by item ===")

# For each item: total FABIO output, total WHEP output, their ratio, and the
# median sector-level relative error.  A ratio near 0.001 would indicate a
# 1000-tonne vs tonne unit mismatch for that item group.
x_by_item <- x_cmp_pd |>
  group_by(.data$fabio_item_code, .data$fabio_item_name) |>
  summarise(
    n_areas = n(),
    fabio_total = sum(.data$ref),
    whep_total = sum(.data$our),
    ratio = .data$whep_total / pmax(.data$fabio_total, 1),
    median_rel_err = median(.data$rel_err),
    .groups = "drop"
  ) |>
  arrange(.data$ratio) |>
  select(
    "fabio_item_code",
    "fabio_item_name",
    "n_areas",
    "fabio_total",
    "whep_total",
    "ratio",
    "median_rel_err"
  )

message("Items where WHEP is lowest relative to FABIO (potential unit issues):")
print(head(x_by_item, 20))

message("\nItems where WHEP is highest relative to FABIO:")
print(tail(x_by_item, 20))

# --------------------------------------------------------------------------- #
# 2. Compare Z row and column sums                                              #
# --------------------------------------------------------------------------- #

.compare_matrix(
  FABIO_Z_PATH,
  "Z — inter-industry flows",
  our$Z,
  fabio_labels_pd,
  our_labels_pd,
  yr_col,
  col_sums = TRUE
)

# --------------------------------------------------------------------------- #
# 3. Identity check: X == rowSums(Z) + rowSums(Y) in WHEP                      #
# --------------------------------------------------------------------------- #

message("\n=== 3. Identity check: X == rowSums(Z) + rowSums(Y) (WHEP) ===")
our_resid <- our$X - (rowSums(our$Z) + rowSums(our$Y))
message(sprintf(
  "  max |residual|: %.3e   mean |residual|: %.3e   (should be ~0)",
  max(abs(our_resid)),
  mean(abs(our_resid))
))

# --------------------------------------------------------------------------- #
# 4. Compare Y row sums                                                        #
# --------------------------------------------------------------------------- #

.compare_matrix(
  FABIO_Y_PATH,
  "Y — final demand",
  our$Y,
  fabio_labels_pd,
  our_labels_pd,
  yr_col
)

# --------------------------------------------------------------------------- #
# 5. Scatter plot (optional — requires ggplot2)                                 #
# --------------------------------------------------------------------------- #

if (interactive() && requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  p_x <- ggplot(
    x_cmp,
    aes(x = log10(.data$ref + 1), y = log10(.data$our + 1))
  ) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_abline(slope = 1, intercept = 0, colour = "firebrick") +
    labs(
      title = paste("X — total output comparison,", YEAR),
      x = "log10(FABIO X + 1)",
      y = "log10(WHEP X + 1)"
    ) +
    theme_minimal()

  print(p_x)
}

message("\nDone.")
