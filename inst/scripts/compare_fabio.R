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
#   area_code, item_cbs_code (= item_code in FABIO), comm_code
.build_fabio_labels <- function(io_codes_path) {
  io <- read.csv(io_codes_path, stringsAsFactors = FALSE)
  # io_codes rows are in the same order as the X matrix rows.
  # item_code in io_codes corresponds to item_cbs_code in WHEP.
  data.frame(
    area_code = as.integer(io$area_code),
    item_cbs_code = as.integer(io$item_code),
    comm_code = io$comm_code,
    stringsAsFactors = FALSE
  )
}

# Join two vectors by (area_code, item_cbs_code).
# Returns: area_code, item_cbs_code, ref, our, abs_err, rel_err.
.compare_vectors <- function(ref_vec, ref_labels, our_vec, our_labels) {
  ref_df <- ref_labels |> mutate(ref = ref_vec)
  our_df <- our_labels |> mutate(our = our_vec)

  inner_join(ref_df, our_df, by = c("area_code", "item_cbs_code")) |>
    mutate(
      abs_err = our - ref,
      rel_err = abs_err / pmax(abs(ref), 1)
    ) |>
    select("area_code", "item_cbs_code", "ref", "our", "abs_err", "rel_err")
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
    add_item_cbs_name() |>
    select("area_name", "item_cbs_name", "ref", "our", "rel_err")
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
    fabio_labels[, c("area_code", "item_cbs_code")],
    rowSums(our_mat),
    our_labels[, c("area_code", "item_cbs_code")]
  )
  .print_summary(paste(label, "row sums"), rs_cmp)
  .print_top_mismatches(rs_cmp, n = 20, label = paste(label, "row-sum"))

  if (col_sums) {
    cs_cmp <- .compare_vectors(
      colSums(mat),
      fabio_labels[, c("area_code", "item_cbs_code")],
      colSums(our_mat),
      our_labels[, c("area_code", "item_cbs_code")]
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
message(sprintf("  %d WHEP sectors", length(our$X)))

# --------------------------------------------------------------------------- #
# Coverage report                                                               #
# --------------------------------------------------------------------------- #

message("\n=== Coverage ===")

fabio_items <- distinct(fabio_labels, .data$item_cbs_code)
our_items <- distinct(our$labels, .data$item_cbs_code)

only_fabio_items <- anti_join(fabio_items, our_items, by = "item_cbs_code") |>
  add_item_cbs_name()
if (nrow(only_fabio_items) > 0) {
  message(
    sprintf(
      "  %d commodity code(s) in FABIO with no CBS match in WHEP",
      nrow(only_fabio_items)
    ),
    " (dropped from comparison):"
  )
  print(only_fabio_items)
}

only_our_items <- anti_join(our_items, fabio_items, by = "item_cbs_code") |>
  add_item_cbs_name()
if (nrow(only_our_items) > 0) {
  message(sprintf(
    "  %d item(s) in WHEP with no FABIO equivalent:",
    nrow(only_our_items)
  ))
  print(only_our_items)
}

# --------------------------------------------------------------------------- #
# 1. Compare X (total output vector)                                            #
# --------------------------------------------------------------------------- #
#
# NOTE on live animal units:
#   FABIO stores live animal X in HEADS (number of animals slaughtered per year
#   for meat animals; stocks for draft animals).  WHEP stores them in TONNES
#   (Livestock Units × 0.65 t/LU, representing herd stock, not annual flow).
#   These are fundamentally different quantities and cannot be compared directly.
#   Live animals are therefore excluded from the main summary statistics below
#   and shown separately in section 1c.
#
# Live animal item_cbs_codes (both meat and draft):
.livestock_items <- whep::items_cbs |>
  dplyr::filter(startsWith(item_type, "livestock")) |>
  dplyr::pull(item_cbs_code)

message("\n=== 1. Total output (X) — non-livestock items only ===")

x_cmp_all <- .compare_vectors(
  fabio_x,
  fabio_labels[, c("area_code", "item_cbs_code")],
  our$X,
  our$labels[, c("area_code", "item_cbs_code")]
)

x_cmp <- x_cmp_all |>
  dplyr::filter(!(.data$item_cbs_code %in% .livestock_items))

message(sprintf(
  "  Matched %d sectors (%d livestock excluded from summary).",
  nrow(x_cmp_all),
  nrow(x_cmp_all) - nrow(x_cmp)
))
.print_summary("X (non-livestock)", x_cmp)
.print_top_mismatches(x_cmp, n = 20, label = "X")

x_cmp_full <- x_cmp_all |> add_area_name() |> add_item_cbs_name()

# --------------------------------------------------------------------------- #
# 1b. X — breakdown by item (detect systematic scale factors)                  #
# --------------------------------------------------------------------------- #

message("\n=== 1b. X breakdown by item ===")

# For each item: total FABIO output, total WHEP output, their ratio, and the
# median sector-level relative error.  A ratio near 0.001 would indicate a
# 1000-tonne vs tonne unit mismatch for that item group.
x_by_item <- x_cmp |>
  group_by(.data$item_cbs_code) |>
  summarise(
    n_areas = n(),
    fabio_total = sum(.data$ref),
    whep_total = sum(.data$our),
    ratio = .data$whep_total / pmax(.data$fabio_total, 1),
    median_rel_err = median(.data$rel_err),
    .groups = "drop"
  ) |>
  add_item_cbs_name() |>
  arrange(.data$ratio) |>
  select(
    "item_cbs_name",
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
# 1c. X — live animals (different units; informational only)                   #
# --------------------------------------------------------------------------- #
#
# FABIO X for live animals = heads (slaughtered count or stock).
# WHEP X for live animals  = tonnes from LU × 0.65 (herd stock, not flow).
# Direct comparison is not meaningful; shown here for reference only.

message("\n=== 1c. X — live animals (FABIO: heads; WHEP: tonnes from LU) ===")
message("  (Not comparable — different units and quantity concepts)")

x_live <- x_cmp_all |>
  dplyr::filter(.data$item_cbs_code %in% .livestock_items) |>
  dplyr::group_by(.data$item_cbs_code) |>
  dplyr::summarise(
    n_areas = dplyr::n(),
    fabio_total = sum(.data$ref),
    whep_total = sum(.data$our),
    .groups = "drop"
  ) |>
  add_item_cbs_name() |>
  dplyr::arrange(.data$item_cbs_name) |>
  dplyr::select("item_cbs_name", "n_areas", "fabio_total", "whep_total")

print(x_live)

# --------------------------------------------------------------------------- #
# 2. Compare Z row and column sums                                              #
# --------------------------------------------------------------------------- #

.compare_matrix(
  FABIO_Z_PATH,
  "Z — inter-industry flows",
  our$Z,
  fabio_labels,
  our$labels,
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
  fabio_labels,
  our$labels,
  yr_col
)

# --------------------------------------------------------------------------- #
# 5. Scatter plot (optional — requires ggplot2)                                 #
# --------------------------------------------------------------------------- #

if (requireNamespace("ggplot2", quietly = TRUE)) {
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
