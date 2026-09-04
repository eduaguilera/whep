# NSE globals for glw_density.R (#1000):
# c("glw_species", "glw_code")

# Gridded livestock head counts for build_gridded_livestock(proxy_method =
# "glw3"), read from the Gridded Livestock of the World version 3 rasters
# (GLW 3; Gilbert et al. 2018, Scientific Data 5:180227,
# doi:10.1038/sdata.2018.227).
#
# CONFIRMED GLW3 FACTS (Harvard Dataverse API, dataverse `glw_3`, read live
# on 2026-09-03; do not re-guess these):
# - Eight datasets, one per GLW species, each at version 3.0, published
#   2018-08-23, licence CC0 1.0 Universal
#   (http://creativecommons.org/publicdomain/zero/1.0). The whep#1000
#   scoping survey recorded CC BY 4.0; the records say CC0, so the rasters
#   carry no attribution condition (the paper is still cited, as science).
# - Each dataset ships eight files. The two this reader can use are
#   `5_<Sp>_2010_Da.tif` (dasymetric) and `6_<Sp>_2010_Aw.tif` (areal
#   weighted), where `<Sp>` is the two-letter species code of
#   `inst/extdata/glw3_species_group.csv`.
# - UNITS, the load-bearing fact. The Dataverse file description of both
#   rasters reads "absolute number of animals per pixel; 4320 by 2160
#   pixels of 0.083333 decimal degrees resolution", and the dataset
#   description "expressed in total number of cattle per pixel (5 min of
#   arc)". The pixel values are therefore HEAD COUNTS, not head per km2 or
#   per hectare, so no area conversion enters this reader: summing the 36
#   five-arcmin pixels that tile a 0.5-degree cell gives that cell's head
#   count directly, which is exactly what `build_gridded_livestock()` reads
#   as `glw_density`'s `density` ("heads per cell, reference year ~2010").
#   `8_Areakm.tif` would be needed only to build a per-km2 product, so
#   `download_glw3.R` does not fetch it.
# - The rasters are a 2010 snapshot. Nothing here is time-varying: the
#   engine masks the density by that year's LUH2 extent, and the resulting
#   weight is normalised within a country, so only the geography of the
#   layer is used, never its absolute level.
#
# The rasters are third-party, multi-GB-class archives, so they follow the
# env-var-gated local-raster mechanism of CLAUDE.md's "Where input data
# comes from": `WHEP_GLW3_DIR` points at a directory that
# `inst/scripts/download/download_glw3.R` fills from the published DOIs,
# and this reader aborts with that instruction when the variable is unset.
# They are not pinned: they are obtainable by anyone, with a published
# per-file MD5, so a pin would add a second uncheckable copy (whep#457).

#' Read GLW3 gridded livestock counts on WHEP's 0.5-degree grid
#'
#' @description
#' Read the Gridded Livestock of the World version 3 rasters (GLW 3,
#' Gilbert et al. 2018) and return them as the `glw_density` table
#' [build_gridded_livestock()] allocates on under
#' `proxy_method = "glw3"`: one row per 0.5-degree cell and WHEP
#' `species_group`, carrying that cell's GLW3 head count.
#'
#' GLW3 is published at 5 arc-minutes (1/12 degree) in absolute animals per
#' pixel, so the 36 pixels tiling a 0.5-degree cell are summed and nothing
#' is rescaled; see *How five-arcmin pixels become 0.5-degree cells*. The
#' eight GLW species are then mapped onto WHEP's eleven species groups by
#' `crosswalk`; see *The GLW3 crosswalk and what it cannot split*.
#'
#' The rasters come from `glw_dir`, else the `WHEP_GLW3_DIR` environment
#' variable, which points at the directory
#' `inst/scripts/download/download_glw3.R` fills from the published Harvard
#' Dataverse DOIs. An unset variable aborts naming that script; there is no
#' fallback to another layer.
#'
#' @param species Which GLW species to read, as named in `crosswalk`'s
#'   `glw_species` column (`"cattle"`, `"buffaloes"`, `"sheep"`,
#'   `"goats"`, `"pigs"`, `"chickens"`, `"ducks"`, `"horses"`). `NULL`
#'   (default) reads all of them. An unknown name aborts listing the known
#'   ones rather than returning a table missing a group.
#' @param variant Which GLW3 product to read: `"DA"` (default, the
#'   dasymetric product, `5_<Sp>_2010_Da.tif`) or `"AW"` (the
#'   areal-weighted product, `6_<Sp>_2010_Aw.tif`), validated with
#'   [rlang::arg_match()]. The two are alternatives, never fallbacks: the
#'   dasymetric product redistributes census counts with high-resolution
#'   covariates, the areal-weighted one spreads them evenly over the
#'   reporting unit's suitable land.
#' @param glw_dir Directory holding the GLW3 GeoTIFFs, overriding
#'   `WHEP_GLW3_DIR`. A `GLW3` subdirectory of it is also searched, since
#'   that is where `download_glw3(dest_dir)` puts them. Defaults to `NULL`.
#' @param crosswalk Optional tibble mapping GLW species onto WHEP species
#'   groups, replacing the packaged default. Required columns
#'   `glw_species`, `glw_code`, `species_group`; see *The GLW3 crosswalk
#'   and what it cannot split*. Defaults to `NULL`.
#' @param example If `TRUE`, return a small fixture instead of reading any
#'   raster. Defaults to `FALSE`.
#'
#' @return A tibble with one row per cell and species group:
#'   - `lon`, `lat`: 0.5-degree cell centre coordinates.
#'   - `species_group`: WHEP livestock functional type.
#'   - `density`: GLW3 head count in that cell, the quantity
#'     [build_gridded_livestock()] documents as "heads per cell".
#'   Cells with no positive count are dropped, so the table is sparse.
#'
#' @section How five-arcmin pixels become 0.5-degree cells:
#' GLW3 pixel values are absolute animal counts per pixel, not densities
#' (Dataverse file description: "absolute number of animals per pixel;
#' 4320 by 2160 pixels of 0.083333 decimal degrees resolution"). The
#' aggregation is therefore a plain block sum: each 0.5-degree cell takes
#' the sum of the 6 x 6 = 36 five-arcmin pixels it contains, missing pixels
#' skipped, and a cell whose pixels are all missing is dropped. No division
#' by area happens anywhere, because none is needed to reach the engine's
#' per-cell head count.
#'
#' The block factor is derived from the raster's own resolution rather than
#' hardcoded, and a resolution that does not divide 0.5 degrees a whole
#' number of times aborts: a raster on another grid would otherwise be
#' silently resampled onto shifted cells.
#'
#' @section The GLW3 crosswalk and what it cannot split:
#' `inst/extdata/glw3_species_group.csv` maps the eight GLW species onto
#' WHEP's `species_group` vocabulary
#' (`inst/extdata/livestock_mapping.csv`). Columns:
#'   - `glw_species`: GLW species name, and the value `species` selects on.
#'   - `glw_code`: the two-letter code in the published file names (`Ct`,
#'     `Bf`, `Sh`, `Gt`, `Pg`, `Ch`, `Ho`, `Dk`).
#'   - `species_group`: the WHEP group the layer feeds.
#'   - `note`: why that row exists, for the reader of the file.
#'
#' Two rules govern a many-to-one or one-to-many row set, and both are
#' applied by this reader:
#'   - **Several GLW species to one group are summed**: `sheep` + `goats`
#'     into `sheep_goats`, `chickens` + `ducks` into `poultry`.
#'   - **One GLW species to several groups gives each group the same
#'     value**: `cattle` feeds both `cattle_dairy` and `cattle_non_dairy`,
#'     `chickens` feeds `chickens_layers` and `chickens_broilers`. GLW3
#'     carries no dairy and no layer/broiler split, so the finer groups
#'     inherit one geography and their national totals still differ. This
#'     is the interim "coarse layer constrains the sum of the finer groups"
#'     rule of whep#1000; task T10 may replace it with a split, and the
#'     replacement is a new crosswalk plus a rule here, not a change of
#'     contract.
#'
#' `camels` and `other` have no GLW species and are absent from the result
#' by construction. That is deliberate:
#' `build_gridded_livestock(proxy_method = "glw3")` aborts naming any group
#' its density table does not cover, so those groups have to be run under
#' `proxy_method = "luh2"` rather than being given a wrong geography here.
#'
#' @source Gilbert, M., Nicolas, G., Cinardi, G., Vanwambeke, S., Van
#'   Boeckel, T. P., Wint, G. R. W. and Robinson, T. P. (2018). Global
#'   distribution data for cattle, buffaloes, horses, sheep, goats, pigs,
#'   chickens and ducks in 2010. *Scientific Data*, 5, 180227.
#'   \doi{10.1038/sdata.2018.227}. Rasters: Harvard Dataverse `glw_3`,
#'   version 3.0, CC0 1.0 Universal.
#'
#' @export
#'
#' @examples
#' read_glw_density(example = TRUE)
read_glw_density <- function(
  species = NULL,
  variant = c("DA", "AW"),
  glw_dir = NULL,
  crosswalk = NULL,
  example = FALSE
) {
  variant <- rlang::arg_match(variant)
  if (isTRUE(example)) {
    return(.example_glw_density())
  }
  crosswalk <- .check_glw_crosswalk(crosswalk %||% .read_glw_crosswalk())
  wanted <- .select_glw_species(species, crosswalk)
  dir <- .resolve_glw_dir(glw_dir)
  .require_terra_for_glw()
  cli::cli_alert_info(
    "GLW3 {variant}: reading {length(wanted)} species from {.path {dir}}"
  )
  purrr::map(
    wanted,
    \(sp) .read_glw_species(sp, crosswalk, dir, variant)
  ) |>
    dplyr::bind_rows() |>
    .apply_glw_crosswalk(crosswalk)
}


# ---- Private helpers --------------------------------------------------

# The packaged crosswalk. Read with utils::read.csv() rather than
# data.table::fread(): the file carries prose in `note`, and CLAUDE.md's
# CSV rule reserves fread() for purely numeric tables.
.read_glw_crosswalk <- function() {
  path <- system.file(
    "extdata",
    "glw3_species_group.csv",
    package = "whep"
  )
  if (!nzchar(path)) {
    cli::cli_abort(c(
      "The packaged GLW3 crosswalk is missing.",
      i = "Expected {.file inst/extdata/glw3_species_group.csv}."
    ))
  }
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble()
}


# A crosswalk is allowed to be many-to-many, but a GLW species must resolve
# to exactly one file: two codes for one species would silently read one of
# them. A repeated (species, group) pair would double the group's count in
# the summation below, so it is refused rather than deduplicated.
.check_glw_crosswalk <- function(crosswalk) {
  .check_columns(
    crosswalk,
    c("glw_species", "glw_code", "species_group"),
    "crosswalk"
  )
  codes <- crosswalk |>
    dplyr::distinct(glw_species, glw_code) |>
    dplyr::count(glw_species) |>
    dplyr::filter(n > 1L)
  if (nrow(codes) > 0L) {
    cli::cli_abort(c(
      "{nrow(codes)} {.field glw_species} value{?s} carr{?ies/y} more than \\
       one {.field glw_code}:",
      "x" = "{.val {codes$glw_species}}."
    ))
  }
  pairs <- crosswalk |>
    dplyr::count(glw_species, species_group) |>
    dplyr::filter(n > 1L)
  if (nrow(pairs) > 0L) {
    cli::cli_abort(c(
      "{nrow(pairs)} {.arg crosswalk} row{?s} {?is/are} duplicated:",
      "x" = "{.val {paste(pairs$glw_species, pairs$species_group)}}.",
      "i" = "A repeated pair would count the layer twice for that group."
    ))
  }
  tibble::as_tibble(crosswalk)
}


# Which GLW species to read. An unknown name aborts: silently skipping it
# would return a table missing a species_group, and the engine's abort
# would then name the group rather than the typo that caused it.
.select_glw_species <- function(species, crosswalk) {
  known <- sort(unique(crosswalk$glw_species))
  if (is.null(species)) {
    return(known)
  }
  species <- unique(as.character(species))
  unknown <- setdiff(species, known)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{length(unknown)} unknown {.arg species} value{?s}:",
      "x" = "{.val {unknown}}.",
      "i" = "Known GLW species: {.val {known}}."
    ))
  }
  species
}


.resolve_glw_dir <- function(glw_dir) {
  resolved <- glw_dir %||% Sys.getenv("WHEP_GLW3_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No GLW3 raster directory available.",
      "i" = "Pass {.arg glw_dir} or set {.envvar WHEP_GLW3_DIR}.",
      "i" = "Fetch the rasters with {.code download_glw3(dest_dir)} from
             {.file inst/scripts/download/download_glw3.R}; the variable
             points at {.path <dest_dir>/GLW3}."
    ))
  }
  if (!dir.exists(resolved)) {
    cli::cli_abort(c(
      "The GLW3 raster directory does not exist:",
      "x" = "{.path {resolved}}."
    ))
  }
  resolved
}


.require_terra_for_glw <- function() {
  if (!rlang::is_installed("terra")) {
    cli::cli_abort(c(
      "Package {.pkg terra} is required to read the GLW3 GeoTIFFs.",
      "i" = "Install it with {.code install.packages(\"terra\")}."
    ))
  }
  invisible(NULL)
}


# The published file names: `5_<Sp>_2010_Da.tif` for the dasymetric
# product and `6_<Sp>_2010_Aw.tif` for the areal-weighted one. The leading
# index is part of the name Dataverse serves, not a sort key added here.
.glw_file_name <- function(code, variant) {
  prefix <- c(DA = "5", AW = "6")[[variant]]
  token <- c(DA = "Da", AW = "Aw")[[variant]]
  sprintf("%s_%s_2010_%s.tif", prefix, code, token)
}


.glw_raster_path <- function(dir, code, variant) {
  file_name <- .glw_file_name(code, variant)
  candidates <- c(
    file.path(dir, file_name),
    file.path(dir, "GLW3", file_name)
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) {
    cli::cli_abort(c(
      "GLW3 raster {.file {file_name}} not found.",
      "x" = "Looked in {.path {candidates}}.",
      "i" = "Fetch it with {.code download_glw3(dest_dir)} from
             {.file inst/scripts/download/download_glw3.R}."
    ))
  }
  found[[1]]
}


# How many five-arcmin pixels tile one WHEP cell along each axis. Derived
# from the raster rather than hardcoded to 6, and refused when it is not a
# whole number: a raster on another grid would otherwise be aggregated onto
# cells shifted off WHEP's 0.5-degree centres, which nothing downstream
# could detect.
.glw_agg_factor <- function(res, target = 0.5) {
  if (length(res) != 2L || !isTRUE(all.equal(res[[1]], res[[2]]))) {
    cli::cli_abort(c(
      "The GLW3 raster is not on a square grid.",
      "x" = "Resolution: {.val {res}} degrees."
    ))
  }
  ratio <- target / res[[1]]
  rounded <- round(ratio)
  ok <- rounded >= 1 && isTRUE(all.equal(ratio, rounded, tolerance = 1e-6))
  if (!ok) {
    cli::cli_abort(c(
      "The GLW3 raster does not tile WHEP's {target}-degree cell.",
      "x" = "{.val {res[[1]]}} degrees goes into it {.val {ratio}} times.",
      "i" = "GLW3 is published at 5 arc-minutes (1/12 degree), which tiles
             the cell exactly 6 x 6."
    ))
  }
  as.integer(rounded)
}


# One species' raster as 0.5-degree head counts. `fun = "sum"` with
# `na.rm = TRUE` skips missing pixels inside a block; a block that is
# entirely missing stays NA and is dropped by `na.rm` in the data-frame
# conversion.
.read_glw_species <- function(sp, crosswalk, dir, variant) {
  code <- crosswalk |>
    dplyr::filter(glw_species == sp) |>
    dplyr::pull(glw_code) |>
    unique()
  raster <- terra::rast(.glw_raster_path(dir, code, variant))
  cells <- raster |>
    terra::aggregate(
      fact = .glw_agg_factor(terra::res(raster)),
      fun = "sum",
      na.rm = TRUE
    ) |>
    terra::as.data.frame(xy = TRUE, na.rm = TRUE)
  names(cells) <- c("lon", "lat", "heads")
  cells |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(heads), heads > 0) |>
    dplyr::mutate(glw_species = sp)
}


# Apply the crosswalk's two rules in one step: the join replicates a GLW
# species onto every group it feeds (same value in each), and the
# summation adds the several species that feed one group.
.apply_glw_crosswalk <- function(counts, crosswalk) {
  .check_columns(
    counts,
    c("lon", "lat", "heads", "glw_species"),
    "counts"
  )
  counts |>
    dplyr::inner_join(
      dplyr::select(crosswalk, glw_species, species_group),
      by = "glw_species",
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      density = sum(heads, na.rm = TRUE),
      .by = c(lon, lat, species_group)
    ) |>
    dplyr::filter(density > 0) |>
    dplyr::arrange(species_group, dplyr::desc(lat), lon)
}
