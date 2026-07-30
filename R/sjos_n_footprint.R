# The embodied-nitrogen trade footprint (SJOS-N Module 4, Task 4.3). Traces one
# per-crop nitrogen category (exceedance / within-boundary / total surplus)
# through WHEP's FABIO footprint framework and splits the consumption-side
# embodied nitrogen into domestic vs traded, mirroring Global's FP_all_N /
# FP_food_all_N (Global/R/sjos_n.r). The per-item_cbs granularity that
# build_n_exceedance_extension() preserves (locked plan decision 14) is what
# makes the trace possible: the footprint attributes each crop's nitrogen to the
# final consumer, so origin_area == target_area is domestic and the rest traded.
# Origin area/item and final-demand identity are retained, and optional producer
# SJOS/nourishment classes are joined rather than collapsed away.

#' Build the embodied-nitrogen trade footprint.
#'
#' @description
#' Trace a per-crop nitrogen category through the FABIO footprint framework and
#' return the consumption-side embodied nitrogen split into domestic and traded
#' flows. The `category` selects which nitrogen mass is carried as the footprint
#' intensity (see [build_n_exceedance_extension()]): `"exceedance"` (default),
#' `"within_boundary"`, or `"production"` (the crop's total surplus).
#'
#' The pipeline is [build_n_exceedance_extension()] to assemble the extension,
#' [build_footprint()] to trace it through the multi-regional input-output model,
#' then a domestic-versus-traded relabelling of the tidy flows: a flow is
#' `"Domestic consumption"` when the nitrogen is emitted and consumed in the same
#' area (`origin_area == target_area`) and `"Traded"` otherwise. Two outputs are
#' returned, mirroring Global's `FP_all_N` and `FP_food_all_N`: `fp_all` is the
#' embodied nitrogen across all final-demand categories, `fp_food` is the subset
#' consumed as food (`target_fd == "food"`).
#'
#' @param exceedance A [build_n_boundary_exceedance()] country-resolution output
#'   passed straight to [build_n_exceedance_extension()]. Not needed when
#'   `example = TRUE`.
#' @param io Optional pre-built [build_io_model()] result reused across
#'   extensions. When `NULL` (default), [build_footprint()] builds it for `years`
#'   from the package inputs (the real-data path, an integration wiring step).
#' @param category Which per-crop nitrogen mass to trace: `"exceedance"`
#'   (default), `"within_boundary"`, or `"production"`. Validated with
#'   [rlang::arg_match()].
#' @param years Years to trace. Defaults to the years present in the extension;
#'   ignored when `io` is supplied.
#' @param data Optional named list of injected inputs. `data$fp_flows` supplies
#'   pre-traced tidy footprint flows (as from [build_footprint()]) directly,
#'   bypassing the model build, for testing the split logic in isolation.
#'   `data$origin_classes` may supply producer classifications keyed by `year`,
#'   `area_code`, `item_cbs_code` (for example [classify_sjos_n()] output).
#' @param example If `TRUE`, return a small hardcoded fixture instead of running
#'   the pipeline. Defaults to `FALSE`.
#'
#' @return A named list with two tibbles:
#'   - `fp_all`: embodied nitrogen by `year`, producer `origin_area` /
#'     `origin_item`, consumer `target_area` / `target_item`, `target_fd`,
#'     `origin` (`"Domestic consumption"` or `"Traded"`), `item_cbs_code`
#'     (an alias of `target_item`) and `impact_u` (tonnes N), stamped with the
#'     traced `category` and optional producer classes.
#'   - `fp_food`: `fp_all` restricted to food consumption (`target_fd ==
#'     "food"`).
#'
#' @export
#' @examples
#' build_sjos_n_footprint(example = TRUE)
build_sjos_n_footprint <- function(
  exceedance = NULL,
  io = NULL,
  category = c("exceedance", "within_boundary", "production"),
  years = NULL,
  data = list(),
  example = FALSE
) {
  if (example) {
    return(.ex_build_sjos_n_footprint())
  }
  category <- rlang::arg_match(category)
  flows <- .sjos_fp_flows(exceedance, io, category, years, data)
  list(
    fp_all = .sjos_fp_consumption(
      flows,
      category,
      data$origin_classes
    ),
    fp_food = .sjos_fp_consumption(
      dplyr::filter(flows, .data$target_fd == "food"),
      category,
      data$origin_classes
    )
  )
}

# ---- Private helpers -------------------------------------------------------

# Trace the chosen nitrogen category to tidy footprint flows, or take pre-traced
# flows injected via data$fp_flows (the isolated split-logic test seam).
.sjos_fp_flows <- function(exceedance, io, category, years, data) {
  if (rlang::has_name(data, "fp_flows")) {
    return(tibble::as_tibble(data$fp_flows))
  }
  build_n_exceedance_extension(exceedance, category) |>
    build_footprint(years = years, io = io, value_col = "impact_u")
}

# Relabel each flow domestic vs traded and aggregate the consumption-side
# embodied nitrogen by consuming area, origin split, and consumed crop.
.sjos_fp_consumption <- function(flows, category, origin_classes = NULL) {
  .check_columns(
    flows,
    c(
      "year",
      "origin_area",
      "origin_item",
      "target_area",
      "target_item",
      "target_fd",
      "value"
    ),
    "footprint flows"
  )
  out <- flows |>
    dplyr::mutate(
      origin = dplyr::if_else(
        .data$origin_area == .data$target_area,
        "Domestic consumption",
        "Traded"
      )
    ) |>
    dplyr::summarise(
      impact_u = .sum_if_any(.data$value),
      .by = c(
        year,
        origin_area,
        origin_item,
        target_area,
        target_item,
        target_fd,
        origin
      )
    ) |>
    dplyr::mutate(
      item_cbs_code = .data$target_item,
      category = category
    )
  .sjos_fp_join_origin_classes(out, origin_classes)
}

.sjos_fp_join_origin_classes <- function(flows, origin_classes) {
  if (is.null(origin_classes)) {
    return(flows)
  }
  .check_columns(
    origin_classes,
    c("year", "area_code", "item_cbs_code"),
    "origin_classes"
  )
  class_cols <- intersect(
    c("nourish", "boundary_side", "sjos_class"),
    names(origin_classes)
  )
  classes <- origin_classes |>
    dplyr::select(
      "year",
      "area_code",
      "item_cbs_code",
      dplyr::all_of(class_cols)
    ) |>
    dplyr::rename(
      origin_area = area_code,
      origin_item = item_cbs_code
    ) |>
    dplyr::distinct()
  dplyr::left_join(
    flows,
    classes,
    by = c("year", "origin_area", "origin_item"),
    relationship = "many-to-one"
  )
}
