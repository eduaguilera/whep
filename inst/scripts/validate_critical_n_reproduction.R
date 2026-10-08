# Validation gate for calculate_critical_n() (issue #1291, step 1).
#
# Recomputes the Schulte-Uebbing et al. (2022) critical-nitrogen allowances
# from the archive's own 2010 Input_files and compares them, cell by cell,
# with the 24 deposited critical input and surplus layers and the 24
# exceedance layers of Zenodo record 6395016 v1.0.
#
# Usage, from the package root (the archive is read from the local cache or
# WHEP_CRITICAL_N_DIR, downloaded on first use like read_critical_n()):
#   Rscript --no-init-file inst/scripts/validate_critical_n_reproduction.R
# Writes the per-layer table to stdout and, when an output path is given as
# the first argument, to that CSV.

devtools::load_all(".", quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
dir <- whep:::.resolve_critical_n_dir(NULL)
root <- whep:::.critn_root_path(dir)

started <- Sys.time()
reproduced <- calculate_critical_n(dir = dir)
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

# Cells holding both arable land and intensive grassland, where the source
# does not print its formulas; every other cell has one reducible land use.
mixed <- whep:::.critn_read_inputs(root) |>
  dplyr::filter(.data$area_arable_ha > 0, .data$area_intensive_ha > 0) |>
  dplyr::pull("cell_id")

layers <- tidyr::expand_grid(
  threshold = c("de", "sw", "gw", "mi"),
  land_use = c("ara", "igl", "all"),
  var = c("nin", "nsur", "exc_nin", "exc_nsur")
)

compare_layer <- function(threshold, land_use, var) {
  file <- switch(
    var,
    nin = file.path("Critical N inputs", "nin_crit_"),
    nsur = file.path("Critical N surpluses", "nsur_crit_"),
    exc_nin = file.path("Exceedance of critical N inputs", "exc_nin_crit_"),
    exc_nsur = file.path("Exeedance of critical N surpluses", "exc_nsur_crit_")
  )
  path <- file.path(
    root,
    "Output_files",
    paste0(file, threshold, "_", land_use, "_ph.asc")
  )
  archive <- whep:::.read_esri_asc(path) |>
    whep:::.nbx_add_cell_key("archive") |>
    dplyr::select("cell_id", archive = "value")
  ours <- reproduced |>
    dplyr::filter(
      .data$critical_threshold == .env$threshold,
      .data$critical_land_use == .env$land_use
    ) |>
    dplyr::transmute(
      .data$cell_id,
      .data$area_ha,
      reproduced = switch(
        var,
        nin = .data$critical_n_input_kgn_ha,
        nsur = .data$critical_n_surplus_kgn_ha,
        exc_nin = .data$current_n_input_kgn_ha - .data$critical_n_input_kgn_ha,
        exc_nsur = .data$current_n_surplus_kgn_ha -
          .data$critical_n_surplus_kgn_ha
      )
    )
  both <- dplyr::inner_join(archive, ours, by = "cell_id")
  # A cell the reproduction leaves undefined counts as a miss.
  diff <- abs(both$reproduced - both$archive)
  diff[is.na(diff)] <- Inf
  tibble::tibble(
    layer = paste(var, threshold, land_use, sep = "_"),
    archive_cells = nrow(archive),
    reproduced_cells = nrow(ours),
    only_archive = sum(!archive$cell_id %in% ours$cell_id),
    only_reproduced = sum(!ours$cell_id %in% archive$cell_id),
    within_0.01 = mean(diff <= 0.01),
    within_0.01_single = mean(diff[!both$cell_id %in% mixed] <= 0.01),
    within_0.01_mixed = mean(diff[both$cell_id %in% mixed] <= 0.01),
    within_1 = mean(diff <= 1),
    p99_abs_diff = unname(stats::quantile(diff, 0.99)),
    archive_tg = sum(both$archive * both$area_ha, na.rm = TRUE) / 1e9,
    reproduced_tg = sum(both$reproduced * both$area_ha, na.rm = TRUE) / 1e9
  ) |>
    dplyr::mutate(
      total_diff_pct = 100 * (.data$reproduced_tg / .data$archive_tg - 1)
    )
}

gate <- purrr::pmap(layers, compare_layer) |> purrr::list_rbind()
cli::cli_inform("calculate_critical_n() took {round(elapsed, 1)} s.")
print(gate, n = Inf, width = Inf)
if (length(args) >= 1L) {
  readr::write_csv(gate, args[[1L]])
}
