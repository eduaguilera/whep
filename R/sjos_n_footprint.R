# The embodied-nitrogen trade footprint (SJOS-N Module 4, Task 4.3). Traces one
# per-crop nitrogen category (exceedance / within-boundary / total surplus)
# through WHEP's FABIO footprint framework and splits the consumption-side
# embodied nitrogen into domestic vs traded, mirroring Global's FP_all_N /
# FP_food_all_N (Global/R/sjos_n.r). The per-item_cbs granularity that
# build_n_exceedance_extension() preserves (locked plan decision 14) is what
# makes the trace possible: the footprint attributes each crop's nitrogen to the
# final consumer, so origin_area == target_area is domestic and the rest traded.
# Origin area/item and final-demand identity are retained, and optional producer
# SJOS/nourishment classes are joined rather than collapsed away. An optional
# consumer-side country-year table joins on target_area and year, so exceedance
# can be cross-tabulated by the consuming country's nourishment class; consumer
# country-years with no class stay NA and are counted, never dropped.

#' Build the embodied-nitrogen trade footprint.
#'
#' @description
#' Trace a per-crop nitrogen category through the FABIO footprint framework and
#' return the consumption-side embodied nitrogen split into domestic and traded
#' flows. The `category` selects which nitrogen mass is carried as the footprint
#' intensity (see [build_n_exceedance_extension()]): `"exceedance"` (default),
#' `"within_boundary"`, or `"production"` (the crop's harvest removal:
#' harvested product plus used residue plus grazed forage).
#'
#' The pipeline is [build_n_exceedance_extension()] to assemble the extension,
#' [build_footprint()] to trace it through the multi-regional input-output model,
#' then a domestic-versus-traded relabelling of the tidy flows: a flow is
#' `"Domestic consumption"` when the nitrogen is emitted and consumed in the same
#' area (`origin_area == target_area`) and `"Traded"` otherwise. Two outputs are
#' returned, mirroring Global's `FP_all_N` and `FP_food_all_N`: `fp_all` is the
#' embodied nitrogen across all final-demand categories, `fp_food` is the subset
#' consumed as food (`target_fd == "food"`).
#' Signed crop attributions are traced as separate positive and negative linear
#' extensions and recombined. Explicit undefined-attribution residuals are
#' rejected by [build_n_exceedance_extension()] before tracing.
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
#'   `data$target_classes` may supply consumer classifications, one row per
#'   country-year keyed by `year` and `area_code`, carrying `nourish` (for
#'   example [normalize_nourishment()] output) and optionally a country-year
#'   `boundary_side` and `sjos_class`. They join on `target_area` and `year` as
#'   `target_nourish`, `target_boundary_side` and `target_sjos_class`. Its
#'   `area_code` must be in the same code space as `target_area`, which is the
#'   IO model's `fd_labels$area_code`: the commodity balances'
#'   `polity_area_code` bucket (see [get_wide_cbs()]), not the source FAOSTAT
#'   area. A consumer bucket with no row in the table is reported as
#'   unclassified; a table keyed in another code space can match the wrong
#'   country wherever the two numberings share a code, so key it on the
#'   bucket. This function classifies nothing: a country-year boundary class is
#'   the caller's to classify after aggregation, and a table with more than one
#'   class per country-year (a crop-level [classify_sjos_n()] output, for
#'   instance) aborts rather than duplicating flows.
#' @param example If `TRUE`, return a small hardcoded fixture instead of running
#'   the pipeline. Defaults to `FALSE`.
#'
#' @return A named list with two tibbles:
#'   - `fp_all`: embodied nitrogen by `year`, producer `origin_area` /
#'     `origin_item`, consumer `target_area` / `target_item`, `target_fd`,
#'     `origin` (`"Domestic consumption"` or `"Traded"`), `item_cbs_code`
#'     (an alias of `target_item`) and `impact_u` (tonnes N), stamped with the
#'     traced `category`, optional producer classes and, when
#'     `data$target_classes` is supplied, the `target_*` consumer classes.
#'   - `fp_food`: `fp_all` restricted to food consumption (`target_fd ==
#'     "food"`).
#'   - `target_class_diag`: only when `data$target_classes` is supplied. One row
#'     per output table (`table`, `"fp_all"` or `"fp_food"`) and every `year`
#'     in `fp_all`, zero-filled where `fp_food` has no flows that year: the
#'     flow and consumer-area counts, `impact_u` (tonnes N), and how many flows,
#'     consumer areas and tonnes N went to a country-year with no `nourish`
#'     class (`n_flows_unclassified`, `n_target_areas_unclassified`,
#'     `impact_u_unclassified`). Those flows keep `NA` in `target_nourish` and
#'     stay in the tables; a warning of class
#'     `whep_sjos_fp_unclassified_target` names the consumer areas.
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
  target_classes <- .sjos_fp_target_class_table(data[["target_classes"]])
  out <- list(
    fp_all = .sjos_fp_consumption(
      flows,
      category,
      data$origin_classes,
      target_classes
    ),
    fp_food = .sjos_fp_consumption(
      dplyr::filter(flows, .data$target_fd == "food"),
      category,
      data$origin_classes,
      target_classes
    )
  )
  if (is.null(target_classes)) {
    return(out)
  }
  out$target_class_diag <- .sjos_fp_target_class_diag(out)
  .sjos_fp_warn_unclassified(out$fp_all)
  out
}

# ---- Private helpers -------------------------------------------------------

# Trace the chosen nitrogen category to tidy footprint flows, or take pre-traced
# flows injected via data$fp_flows (the isolated split-logic test seam).
.sjos_fp_flows <- function(exceedance, io, category, years, data) {
  if (rlang::has_name(data, "fp_flows")) {
    return(tibble::as_tibble(data$fp_flows))
  }
  extension <- build_n_exceedance_extension(exceedance, category)
  if (all(extension$impact_u >= 0)) {
    return(build_footprint(
      extension,
      years = years,
      io = io,
      value_col = "impact_u"
    ))
  }
  .sjos_fp_trace_signed(extension, io, years)
}

# The generic footprint engine intentionally publishes positive flows only.
# A signed crop attribution is therefore traced as two non-negative linear
# extensions and recombined afterwards. This preserves Eduardo's signed-share
# rule without changing the generic engine's contract or dropping negative
# crop contributions.
.sjos_fp_trace_signed <- function(extension, io, years) {
  trace_part <- function(sign) {
    part <- dplyr::mutate(
      extension,
      impact_u = pmax(sign * .data$impact_u, 0)
    ) |>
      dplyr::filter(.data$impact_u > 0)
    if (nrow(part) == 0L) {
      return(NULL)
    }
    build_footprint(
      part,
      years = years,
      io = io,
      value_col = "impact_u"
    ) |>
      dplyr::mutate(value = sign * .data$value)
  }
  flows <- dplyr::bind_rows(trace_part(1), trace_part(-1))
  key <- setdiff(names(flows), "value")
  dplyr::summarise(
    flows,
    value = sum(.data$value),
    .by = dplyr::all_of(key)
  ) |>
    dplyr::filter(.data$value != 0)
}

# Relabel each flow domestic vs traded and aggregate the consumption-side
# embodied nitrogen by consuming area, origin split, and consumed crop.
.sjos_fp_consumption <- function(
  flows,
  category,
  origin_classes = NULL,
  target_classes = NULL
) {
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
  out |>
    .sjos_fp_join_origin_classes(origin_classes) |>
    .sjos_fp_join_target_classes(target_classes)
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

# The consumer-side class table, reduced to one row per country-year and renamed
# onto the flow keys (target_area, target_*). Returns NULL when none is given,
# so the producer-side output is untouched.
.sjos_fp_target_class_table <- function(target_classes) {
  if (is.null(target_classes)) {
    return(NULL)
  }
  .check_columns(
    target_classes,
    c("year", "area_code", "nourish"),
    "target_classes"
  )
  class_cols <- intersect(
    c("nourish", "boundary_side", "sjos_class"),
    names(target_classes)
  )
  classes <- target_classes |>
    dplyr::select("year", "area_code", dplyr::all_of(class_cols)) |>
    dplyr::distinct()
  .sjos_fp_check_country_year(classes)
  classes |>
    dplyr::rename(target_area = "area_code") |>
    dplyr::rename_with(\(x) paste0("target_", x), dplyr::all_of(class_cols))
}

# A consumer class is a country-year property. Two classes for one country-year
# would duplicate every flow into it, so the table is refused instead. The usual
# cause is a crop-level table, whose boundary side differs between crops: the
# consumer's boundary class is classified after aggregation, by the caller.
.sjos_fp_check_country_year <- function(classes) {
  dup <- classes |>
    dplyr::count(.data$year, .data$area_code) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(dup) == 0L) {
    return(invisible())
  }
  cli::cli_abort(c(
    "{.arg target_classes} must hold one class per country-year.",
    "x" = "{nrow(dup)} country-year{?s} carr{?ies/y} more than one class, for
           example area {.val {dup$area_code[[1]]}} in {.val {dup$year[[1]]}}.",
    "i" = "A crop-level table such as {.fn classify_sjos_n} output has a
           boundary side per crop. Classify the consumer country-year after
           aggregation and supply that table, or supply {.field nourish}
           alone."
  ))
}

# Left join, so a consumer country-year missing from the table keeps its flows
# with NA classes. The diagnostic and warning below count those flows.
.sjos_fp_join_target_classes <- function(flows, target_classes) {
  if (is.null(target_classes)) {
    return(flows)
  }
  dplyr::left_join(
    flows,
    target_classes,
    by = c("year", "target_area"),
    relationship = "many-to-one"
  )
}

# Coverage of the consumer join per output table and year. A flow is
# unclassified when its consumer country-year has no nourish class, whether the
# country-year is absent from the table or present with NA. Every table gets a
# row for every year fp_all covers: fp_food can have no flows in a year, and a
# missing row would read as a gap in the diagnostic rather than as zero flows.
.sjos_fp_target_class_diag <- function(out) {
  years <- sort(unique(out$fp_all$year))
  purrr::imap(out[c("fp_all", "fp_food")], \(fp, nm) {
    fp |>
      dplyr::mutate(.unclassified = is.na(.data$target_nourish)) |>
      dplyr::summarise(
        n_flows = dplyr::n(),
        n_flows_unclassified = sum(.data$.unclassified),
        n_target_areas = dplyr::n_distinct(.data$target_area),
        n_target_areas_unclassified = dplyr::n_distinct(
          .data$target_area[.data$.unclassified]
        ),
        # Before impact_u: summarise() rebinds that name to the scalar total.
        impact_u_unclassified = sum(.data$impact_u[.data$.unclassified]),
        impact_u = sum(.data$impact_u),
        .by = "year"
      ) |>
      dplyr::relocate("impact_u", .before = "impact_u_unclassified") |>
      .sjos_fp_diag_complete(years) |>
      dplyr::mutate(table = nm, .before = 1L)
  }) |>
    dplyr::bind_rows()
}

# Structural zeros: the ledger here is the footprint table itself, so a year
# with no rows in it has zero flows and zero tonnes, not an unknown amount.
.sjos_fp_diag_complete <- function(diag, years) {
  tidyr::complete(
    diag,
    year = years,
    fill = list(
      n_flows = 0L,
      n_flows_unclassified = 0L,
      n_target_areas = 0L,
      n_target_areas_unclassified = 0L,
      impact_u = 0,
      impact_u_unclassified = 0
    )
  ) |>
    dplyr::arrange(.data$year)
}

# fp_food is a subset of fp_all, so warning on fp_all covers both tables.
.sjos_fp_warn_unclassified <- function(fp_all) {
  missing <- dplyr::filter(fp_all, is.na(.data$target_nourish))
  if (nrow(missing) == 0L) {
    return(invisible())
  }
  # cli takes the quantity for {?} from the last value before it, and a numeric
  # vector longer than one is not a valid quantity (it aborts). Every plural
  # below is therefore pinned with cli::qty() to a scalar count.
  n_missing <- nrow(missing)
  n_flows <- nrow(fp_all)
  areas <- as.character(sort(unique(missing$target_area)))
  cli::cli_warn(
    c(
      "!" = "{cli::qty(n_missing)}{n_missing} footprint flow{?s} (of {n_flows})
             {cli::qty(n_missing)}{?goes/go} to a consumer country-year with no
             {.field nourish} class; {.field target_nourish} is NA on
             {cli::qty(n_missing)}{?it/them}.",
      "i" = "They carry {signif(sum(missing$impact_u), 4)} of
             {signif(sum(fp_all$impact_u), 4)} t N and stay in the output.",
      "i" = "{cli::qty(length(areas))}Consumer area{?s}: {.val {areas}}."
    ),
    class = "whep_sjos_fp_unclassified_target"
  )
}
