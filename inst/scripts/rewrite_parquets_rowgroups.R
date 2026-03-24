# Rewrite parquet files with year-sorted row groups for memory-efficient reads
#
# WHY: The current parquet files have a single row group (entire file).
# This forces any reader to decompress the FULL file even when filtering
# by year. For faostat-trade (128 MB compressed, 17M rows), this means
# a ~2 GB RAM spike just to extract 1 year of data (~334K rows).
#
# FIX: Rewrite with arrow, sorting by year and using a row group size of
# ~500K rows. This lets arrow::open_dataset() skip row groups whose
# year range doesn't match, cutting peak memory from ~2 GB to ~60 MB
# for a 1-year query.
#
# HOW TO USE:
#   1. Run this script once for each file you want to optimize.
#   2. Upload the rewritten parquet to the pin board (replacing the old one).
#   3. The CSV stays unchanged — only the parquet is rewritten.
#
# IMPORTANT: After uploading, update whep_inputs.csv with the new version.

library(arrow)

# Files to optimize (sorted by size — biggest savings first).
# All use "Year" as year column except where noted.
files_to_rewrite <- list(
  # alias             year_col   disk_size  rows
  # ─────────────────────────────────────────────────
  "faostat-trade",         # "Year"   128 MB  17M rows  ← biggest win
  "faostat-fbs-new",       # "Year"    36 MB   5M rows
  "international-yields",  # "year"    33 MB   ~2M rows
  "faostat-production",    # "Year"    18 MB   ~1M rows
  "faostat-cbs-old-crops", # "Year"    22 MB   ~1M rows
  "faostat-fbs-old",       # "Year"    15 MB   ~1M rows
  "faostat-emissions-livestock", # "Year" 27 MB
  "faostat-cbs-old-animal",# "Year"   6.4 MB
  "luh2-areas",            # "Year"   7.4 MB
  "faostat-production-old" # "Year"   1.8 MB
)

# Year column mapping (most use "Year", exceptions listed)
year_cols <- c(
  "international-yields" = "year",
  "historical-trade-exports" = "year",
  "historical-trade-imports" = "year"
)

rewrite_parquet_sorted <- function(
  input_path,
  output_path,
  year_col = "Year",
  row_group_size = 500000L
) {
  cat("Reading:", input_path, "\n")
  tbl <- read_parquet(input_path, as_data_frame = FALSE)
  n <- tbl$num_rows
  cat("  Rows:", n, " Columns:", tbl$num_columns, "\n")

  # Sort by year so row groups have contiguous year ranges
  cat("  Sorting by", year_col, "...\n")
  idx <- order(as.vector(tbl[[year_col]]))
  tbl <- tbl$Slice(0)  # force compute
  tbl <- tbl$Take(idx)

  # Write with small row groups
  n_groups <- ceiling(n / row_group_size)
  cat("  Writing", n_groups, "row groups (", row_group_size, "rows each) ...\n")

  write_parquet(
    tbl,
    output_path,
    chunk_size = row_group_size
  )

  old_size <- file.size(input_path)
  new_size <- file.size(output_path)
  cat("  Done:", round(old_size / 1e6, 1), "MB ->",
      round(new_size / 1e6, 1), "MB\n")

  # Verify row groups
  pf <- ParquetFileReader$create(output_path)
  cat("  Row groups:", pf$num_row_groups, "\n\n")
}

# ── Example: rewrite one file ─────────────────────────────────────────────

# To rewrite a single file from CSV:
#
#   data <- readr::read_csv("path/to/faostat-trade.csv")
#   arrow::write_parquet(
#     dplyr::arrange(data, Year),
#     "faostat-trade.parquet",
#     chunk_size = 500000L
#   )
#
# To rewrite from an existing parquet (already cached locally):
#
#   rewrite_parquet_sorted(
#     "~/.cache/pins/url/.../faostat-trade.parquet",
#     "faostat-trade-optimized.parquet",
#     year_col = "Year"
#   )

# ── Batch rewrite all files from CSVs ────────────────────────────────────

# Uncomment and adjust paths to rewrite all files from your CSVs:

# csv_dir <- "path/to/your/csvs"
# output_dir <- "path/to/output"
# dir.create(output_dir, showWarnings = FALSE)
#
# for (alias in files_to_rewrite) {
#   csv_path <- file.path(csv_dir, paste0(alias, ".csv"))
#   if (!file.exists(csv_path)) {
#     cat("Skipping", alias, "- CSV not found\n")
#     next
#   }
#
#   ycol <- if (alias %in% names(year_cols)) year_cols[[alias]] else "Year"
#   out_path <- file.path(output_dir, paste0(alias, ".parquet"))
#
#   cat("Processing", alias, "...\n")
#   data <- readr::read_csv(csv_path, show_col_types = FALSE)
#   data <- dplyr::arrange(data, .data[[ycol]])
#
#   arrow::write_parquet(
#     data,
#     out_path,
#     chunk_size = 500000L
#   )
#
#   pf <- ParquetFileReader$create(out_path)
#   cat("  Written:", round(file.size(out_path)/1e6, 1), "MB,",
#       pf$num_row_groups, "row groups\n")
# }

# ── Then update prepare_upload.R ──────────────────────────────────────────

# After rewriting, your create_version() in prepare_upload.R should use
# arrow instead of nanoparquet so future uploads also have proper row groups:
#
#   arrow::write_parquet(
#     dplyr::arrange(data, Year),  # sort by year
#     paths[[2]],
#     chunk_size = 500000L
#   )
#
# This is a drop-in replacement for the nanoparquet::write_parquet() call.

# ── Expected memory improvement ──────────────────────────────────────────

# With year-sorted row groups:
#   faostat-trade (17M rows, 128 MB):
#     Before: must decompress all 17M rows (~2 GB peak) to filter 1 year
#     After:  skips ~95% of row groups, decompresses only ~1M rows (~120 MB peak)
#
#   Total pipeline (1 year):
#     Before: ~3.3 GB peak
#     After:  ~500 MB peak (estimated)
