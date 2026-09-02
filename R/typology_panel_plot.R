#' Plot four N indicators as time series per typology
#'
#' @description Creates a four-panel figure showing the temporal evolution of
#'   external N dependency, Finn Cycling Index, pollution (soil + livestock
#'   surplus per ha), and intensification (synthetic + feed imports per ha)
#'   as mean lines per typology with interquartile ribbons.
#'
#' @param finn_data Pre-computed tibble from [create_finn_indicator()].
#'   If `NULL`, computed automatically (slow).
#' @param n_prov_destiny Nitrogen flows tibble from [create_n_prov_destiny()].
#'   If `NULL`, loaded automatically.
#' @param area_df Cropland area per year and province, with columns `year`,
#'   `province_name` and `area_ha`. If `NULL`, read from the `npp_ygpit` pin.
#' @param typo_df Typology assignment per year and province, with columns
#'   `year`, `province_name` and `Typology_base`. If `NULL`, derived from
#'   `create_typo_ts_plot()`.
#'
#' @return A patchwork ggplot object.
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)
#' ) {
#'   # Two provinces at two dates is enough to exercise the four panels; the
#'   # real figure spans 50 provinces and 1860-2023.
#'   flows <- tibble::tribble(
#'     ~year, ~province_name, ~box, ~origin, ~destiny, ~mg_n,
#'     1960, "A", "Cropland", "Synthetic", "Cropland", 900,
#'     1960, "A", "Cropland", "Outside", "livestock_mono", 300,
#'     1960, "A", "Cropland", "Cropland", "population_food", 500,
#'     1960, "B", "Cropland", "Synthetic", "Cropland", 400,
#'     1960, "B", "Cropland", "Outside", "livestock_mono", 100,
#'     2000, "A", "Cropland", "Synthetic", "Cropland", 2600,
#'     2000, "A", "Cropland", "Outside", "livestock_mono", 1800,
#'     2000, "A", "Cropland", "Cropland", "population_food", 700,
#'     2000, "B", "Cropland", "Synthetic", "Cropland", 900,
#'     2000, "B", "Cropland", "Outside", "livestock_mono", 500
#'   )
#'   area_df <- tibble::tribble(
#'     ~year, ~province_name, ~area_ha,
#'     1960, "A", 10000,
#'     1960, "B", 8000,
#'     2000, "A", 9000,
#'     2000, "B", 7000
#'   )
#'   typo_df <- tibble::tribble(
#'     ~year, ~province_name, ~Typology_base,
#'     1960, "A", "Specialized cropping systems",
#'     1960, "B", "Semi-natural agroecosystems",
#'     2000, "A", "Specialized cropping systems",
#'     2000, "B", "Semi-natural agroecosystems"
#'   )
#'   finn_data <- tibble::tribble(
#'     ~year, ~province_name, ~finn_index,
#'     1960, "A", 0.12,
#'     1960, "B", 0.18,
#'     2000, "A", 0.07,
#'     2000, "B", 0.09
#'   )
#'   panel <- plot_typology_indicators_panel(
#'     finn_data = finn_data,
#'     n_prov_destiny = flows,
#'     area_df = area_df,
#'     typo_df = typo_df
#'   )
#' }
plot_typology_indicators_panel <- function(
  finn_data = NULL,
  n_prov_destiny = NULL,
  area_df = NULL,
  typo_df = NULL
) {
  rlang::check_installed(
    c("ggplot2", "patchwork"),
    "to draw the typology indicator panels."
  )
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }
  if (is.null(finn_data)) {
    finn_data <- create_finn_indicator(n_prov_destiny)
  }

  flows <- n_prov_destiny
  area_df <- area_df %||% .panel_area_df()
  typo_df <- typo_df %||% .panel_typology_df()
  colors <- .finn_typology_colors()
  periods <- c(1860, 1920, 1960, 2010)

  p_ext <- .panel_ext_dep(flows, area_df, typo_df, colors, periods)
  p_fci <- .panel_fci(finn_data, typo_df, colors, periods)
  p_pol <- .panel_pollution(flows, area_df, typo_df, colors, periods)
  p_int <- .panel_intensification(flows, area_df, typo_df, colors, periods)

  .wrap_two_by_two(list(p_ext, p_fci, p_pol, p_int))
}


#' Plot four N indicators as period comparisons per typology
#'
#' @description Creates a four-panel figure comparing external N dependency,
#'   Finn Cycling Index, pollution (soil + livestock surplus per ha), and
#'   intensification (synthetic + feed imports per ha) across four reference
#'   periods (1860-1870, 1920-1930, 1960-1970, 2010-2020), analogous to the
#'   periods panel from [plot_finn_circularity()]. Each panel facets by
#'   typology and adds a "Spain (national)" facet computed from the national
#'   GRAFS dataset ([create_n_nat_destiny()]), shown as a single black point
#'   per period since there is only one national observation per period.
#'
#' @param finn_data Pre-computed tibble from [create_finn_indicator()].
#'   If `NULL`, computed automatically (slow).
#' @param n_prov_destiny Nitrogen flows tibble from [create_n_prov_destiny()].
#'   If `NULL`, loaded automatically.
#' @param n_nat_destiny National nitrogen flows tibble from
#'   [create_n_nat_destiny()]. If `NULL`, computed automatically (slow).
#' @param panel_data Named list overriding the two frames otherwise read from
#'   pins: `area_df` (`year`, `province_name`, `area_ha`) and `typo_df`
#'   (`year`, `province_name`, `Typology_base`). Missing elements are loaded
#'   automatically.
#'
#' @return A patchwork ggplot object.
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)
#' ) {
#'   # The four reference periods are 1860-1870, 1920-1930, 1960-1970 and
#'   # 2010-2020, so an example needs at least one year inside two of them.
#'   flows <- tibble::tribble(
#'     ~year, ~province_name, ~box, ~origin, ~destiny, ~mg_n,
#'     1865, "A", "Cropland", "Synthetic", "Cropland", 900,
#'     1865, "A", "Cropland", "Outside", "livestock_mono", 300,
#'     1865, "B", "Cropland", "Synthetic", "Cropland", 400,
#'     1965, "A", "Cropland", "Synthetic", "Cropland", 2600,
#'     1965, "A", "Cropland", "Outside", "livestock_mono", 1800,
#'     1965, "B", "Cropland", "Synthetic", "Cropland", 900
#'   )
#'   panel_data <- list(
#'     area_df = tibble::tribble(
#'       ~year, ~province_name, ~area_ha,
#'       1865, "A", 10000,
#'       1865, "B", 8000,
#'       1965, "A", 9000,
#'       1965, "B", 7000
#'     ),
#'     typo_df = tibble::tribble(
#'       ~year, ~province_name, ~Typology_base,
#'       1865, "A", "Specialized cropping systems",
#'       1865, "B", "Semi-natural agroecosystems",
#'       1965, "A", "Specialized cropping systems",
#'       1965, "B", "Semi-natural agroecosystems"
#'     )
#'   )
#'   finn_data <- tibble::tribble(
#'     ~year, ~province_name, ~finn_index,
#'     1865, "A", 0.12,
#'     1865, "B", 0.18,
#'     1965, "A", 0.07,
#'     1965, "B", 0.09
#'   )
#'   panel <- plot_typology_periods_panel(
#'     finn_data = finn_data,
#'     n_prov_destiny = flows,
#'     n_nat_destiny = flows,
#'     panel_data = panel_data
#'   )
#' }
plot_typology_periods_panel <- function(
  finn_data = NULL,
  n_prov_destiny = NULL,
  n_nat_destiny = NULL,
  panel_data = NULL
) {
  rlang::check_installed(
    c("ggplot2", "patchwork"),
    "to draw the typology indicator panels."
  )
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }
  if (is.null(finn_data)) {
    finn_data <- create_finn_indicator(n_prov_destiny)
  }
  if (is.null(n_nat_destiny)) {
    n_nat_destiny <- create_n_nat_destiny()
  }

  panel_data <- panel_data %||% list()
  flows <- n_prov_destiny
  area_df <- panel_data$area_df %||% .panel_area_df()
  typo_df <- panel_data$typo_df %||% .panel_typology_df()
  colors <- .finn_typology_colors()
  national <- .panel_national_context(n_nat_destiny, area_df)

  p_ext <- .panel_ext_dep_periods(
    flows,
    typo_df,
    colors,
    list(top_row = TRUE, national = national)
  )
  p_fci <- .panel_fci_periods(
    finn_data,
    typo_df,
    colors,
    list(top_row = TRUE, national = national)
  )
  p_pol <- .panel_pollution_periods(
    flows,
    area_df,
    typo_df,
    colors,
    list(top_row = FALSE, national = national)
  )
  p_int <- .panel_intensification_periods(
    flows,
    area_df,
    typo_df,
    colors,
    list(top_row = FALSE, national = national)
  )

  .panel_periods_cross(p_ext, p_fci, p_pol, p_int)
}


# --- Private helpers: data preparation ---------------------------------------

.panel_area_df <- function() {
  whep_read_file("npp_ygpit") |>
    dplyr::rename_with(tolower) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(
      area_ha = sum(area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )
}

.panel_typology_df <- function() {
  create_typo_ts_plot() |>
    dplyr::select(year, province_name, Typology_base) |>
    dplyr::mutate(
      Typology_base = stringr::str_remove(
        Typology_base,
        " \\(intensive\\)| \\(extensive\\)"
      )
    )
}

.national_area_df <- function(area_df) {
  area_df |>
    dplyr::group_by(year) |>
    dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(province_name = "Spain")
}

.panel_national_context <- function(n_nat_destiny, area_df) {
  nat_flows <- n_nat_destiny
  list(
    flows = nat_flows,
    area_df = .national_area_df(area_df),
    finn = create_finn_indicator(nat_flows)
  )
}

.panel_typo_summary <- function(df, typo_df) {
  df |>
    dplyr::inner_join(typo_df, by = c("year", "province_name")) |>
    dplyr::filter(!is.na(value), !is.na(Typology_base)) |>
    dplyr::group_by(year, Typology_base) |>
    dplyr::summarise(
      mean_val = mean(value, na.rm = TRUE),
      q25 = stats::quantile(value, 0.25, na.rm = TRUE),
      q75 = stats::quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}

.panel_ts_plot <- function(
  summary_df,
  ylab,
  colors,
  periods,
  pct_axis = FALSE,
  ylim = NULL
) {
  p <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
      x = year,
      y = mean_val,
      color = Typology_base,
      fill = Typology_base
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q25, ymax = q75),
      alpha = 0.12,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_vline(
      xintercept = periods,
      linetype = "dashed",
      color = "grey60",
      linewidth = 0.35
    ) +
    ggplot2::scale_color_manual(values = colors, name = "Typology") +
    ggplot2::scale_fill_manual(values = colors, name = "Typology") +
    ggplot2::scale_x_continuous(breaks = seq(1860, 2020, by = 20)) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (pct_axis) {
    p <- p +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(accuracy = 1),
        limits = c(0, 1)
      )
  } else if (!is.null(ylim)) {
    p <- p + ggplot2::scale_y_continuous(limits = ylim)
  }
  p
}

.panel_periods_plot <- function(df, typo_df, colors, ylab, opts = list()) {
  opts <- utils::modifyList(
    list(pct_axis = FALSE, top_row = TRUE, spain_values = NULL, ylim = NULL),
    opts
  )
  ranges <- .fci_period_ranges()
  typo_order <- .fci_typology_order()
  if (!is.null(opts$spain_values)) {
    typo_order <- c(typo_order, "Spain (national)")
  }

  province_means <- df |>
    dplyr::inner_join(typo_df, by = c("year", "province_name")) |>
    dplyr::filter(!is.na(value), !is.na(Typology_base)) |>
    .assign_period_label() |>
    dplyr::group_by(province_name, Typology_base, period_label) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      period_label = factor(period_label, levels = ranges$period_label),
      Typology_base = factor(Typology_base, levels = typo_order)
    )

  spain_means <- .spain_period_means(opts$spain_values, ranges, typo_order)
  .panel_periods_geom(province_means, spain_means, colors, ylab, opts)
}

# Aggregates the national indicator series to a single mean value per
# reference period, matched to the "Spain (national)" facet level so it
# lines up with the provincial typology facets.
.spain_period_means <- function(spain_values, ranges, typo_order) {
  if (is.null(spain_values)) {
    return(NULL)
  }
  spain_values |>
    dplyr::filter(!is.na(value)) |>
    .assign_period_label() |>
    dplyr::group_by(period_label) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      period_label = factor(period_label, levels = ranges$period_label),
      Typology_base = factor("Spain (national)", levels = typo_order)
    )
}

.panel_periods_geom <- function(
  province_means,
  spain_means,
  colors,
  ylab,
  opts
) {
  p <- ggplot2::ggplot(
    province_means,
    ggplot2::aes(x = period_label, y = value, color = Typology_base)
  ) +
    ggplot2::geom_boxplot(
      fill = NA,
      outlier.shape = NA,
      linewidth = 0.5
    ) +
    ggplot2::geom_jitter(width = 0.15, size = 1.2, alpha = 0.55) +
    ggplot2::facet_wrap(
      ~Typology_base,
      nrow = 1,
      labeller = ggplot2::label_wrap_gen(width = 18)
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(
        size = 9,
        margin = ggplot2::margin(t = 4, b = 4)
      ),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.margin = ggplot2::margin(t = 8, r = 4, b = 4, l = 4)
    )

  if (!is.null(spain_means)) {
    p <- p +
      ggplot2::geom_point(
        data = spain_means,
        mapping = ggplot2::aes(x = period_label, y = value),
        inherit.aes = FALSE,
        color = "black",
        shape = 18,
        size = 3
      )
  }

  if (opts$pct_axis) {
    p <- p +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(accuracy = 1),
        limits = c(0, 1)
      )
  } else if (!is.null(opts$ylim)) {
    p <- p + ggplot2::scale_y_continuous(limits = opts$ylim)
  }
  .panel_periods_row_theme(p, opts$top_row)
}

# Top-row panels keep the typology strip and drop the x-axis (period)
# labels; bottom-row panels do the opposite. This avoids repeating the
# same typology names and period labels in all four combined panels.
.panel_periods_row_theme <- function(p, top_row) {
  if (top_row) {
    p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  } else {
    p +
      ggplot2::theme(
        strip.text = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank()
      )
  }
}

.panel_divider <- function() {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "grey40", color = NA)
    )
}

# Composes four ggplots into a 2x2 grid with a shared bottom legend.
#
# Deliberately avoids patchwork's `+`, `/` and `&` operators on plain ggplot
# objects: those are exported methods that only take effect once patchwork is
# *attached*, so `p_a + p_b` fails with "Can't add `p_b` to a <ggplot> object"
# for anyone who has not run library(patchwork). patchwork is a Suggests, and
# whep never attaches it. wrap_plots() is a plain namespaced call, and the
# per-panel theme is applied with ggplot2's own `+`.
.wrap_two_by_two <- function(panels) {
  panels |>
    purrr::map(~ .x + ggplot2::theme(legend.position = "bottom")) |>
    patchwork::wrap_plots(nrow = 2, guides = "collect")
}

.panel_periods_cross <- function(p_tl, p_tr, p_bl, p_br) {
  divider <- .panel_divider()
  design <- "
    AAAVBBB
    AAAVBBB
    AAAVBBB
    LLLVRRR
    CCCVDDD
    CCCVDDD
    CCCVDDD
  "
  patchwork::wrap_plots(
    A = p_tl,
    B = p_tr,
    C = p_bl,
    D = p_br,
    V = divider,
    L = divider,
    R = divider,
    design = design,
    widths = c(1, 1, 1, 0.015, 1, 1, 1),
    heights = c(1, 1, 1, 0.015, 1, 1, 1)
  )
}


# --- Private helpers: indicator computation ----------------------------------

# population_food_inedible is the remainder .split_food_inedible_loss()
# (n_prov_destiny.R) split out of population_food; it left the producing
# system exactly like the edible fraction did, so every destiny list below
# that includes population_food includes it too.
.ext_dep_values <- function(flows) {
  ext <- flows |>
    dplyr::filter(
      (origin %in%
        c("Synthetic", "Fixation", "Deposition") &
        destiny %in% c("Cropland", "semi_natural_agroecosystems")) |
        (origin == "Outside" &
          destiny %in%
            c(
              "livestock_rum",
              "livestock_mono",
              "population_food",
              "population_food_inedible",
              "population_other_uses"
            ))
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(ext_mg = sum(mg_n, na.rm = TRUE), .groups = "drop")

  int <- flows |>
    dplyr::filter(
      (origin %in%
        c("Livestock", "People") &
        destiny %in% c("Cropland", "semi_natural_agroecosystems")) |
        (origin %in%
          c("Cropland", "semi_natural_agroecosystems") &
          destiny %in%
            c(
              "livestock_rum",
              "livestock_mono",
              "population_food",
              "population_food_inedible",
              "population_other_uses"
            ))
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(int_mg = sum(mg_n, na.rm = TRUE), .groups = "drop")

  dplyr::full_join(ext, int, by = c("year", "province_name")) |>
    dplyr::mutate(
      year = as.numeric(year),
      dplyr::across(c(ext_mg, int_mg), ~ dplyr::coalesce(.x, 0)),
      value = ext_mg / (ext_mg + int_mg)
    ) |>
    dplyr::filter(is.finite(value))
}

.panel_ext_dep <- function(flows, area_df, typo_df, colors, periods) {
  summary_df <- .panel_typo_summary(.ext_dep_values(flows), typo_df)
  .panel_ts_plot(
    summary_df,
    "External N dependency",
    colors,
    periods,
    pct_axis = TRUE
  )
}

.panel_ext_dep_periods <- function(flows, typo_df, colors, opts = list()) {
  opts <- utils::modifyList(list(top_row = TRUE, national = NULL), opts)
  spain_values <- if (is.null(opts$national)) {
    NULL
  } else {
    .ext_dep_values(opts$national$flows)
  }
  .panel_periods_plot(
    .ext_dep_values(flows),
    typo_df,
    colors,
    "External N dependency",
    list(pct_axis = TRUE, top_row = opts$top_row, spain_values = spain_values)
  )
}

.fci_values <- function(finn_data) {
  finn_data |>
    dplyr::mutate(year = as.numeric(year), value = finn_index)
}

.panel_fci <- function(finn_data, typo_df, colors, periods) {
  summary_df <- .panel_typo_summary(.fci_values(finn_data), typo_df)
  .panel_ts_plot(summary_df, "Finn Cycling Index", colors, periods)
}

.panel_fci_periods <- function(finn_data, typo_df, colors, opts = list()) {
  opts <- utils::modifyList(list(top_row = TRUE, national = NULL), opts)
  spain_values <- if (is.null(opts$national)) {
    NULL
  } else {
    .fci_values(opts$national$finn)
  }
  .panel_periods_plot(
    .fci_values(finn_data),
    typo_df,
    colors,
    "Finn Cycling Index",
    list(top_row = opts$top_row, spain_values = spain_values)
  )
}

.pollution_values <- function(flows, area_df) {
  soil_in <- flows |>
    dplyr::filter(
      origin %in%
        c("Synthetic", "Fixation", "Deposition", "Livestock", "People"),
      destiny == "Cropland"
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(soil_in = sum(mg_n, na.rm = TRUE), .groups = "drop")

  soil_out <- flows |>
    dplyr::filter(
      origin == "Cropland",
      destiny %in%
        c(
          "population_food",
          "population_food_inedible",
          "population_other_uses",
          "livestock_rum",
          "livestock_mono",
          "export"
        )
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(soil_out = sum(mg_n, na.rm = TRUE), .groups = "drop")

  lv_in <- flows |>
    dplyr::filter(
      destiny %in% c("livestock_rum", "livestock_mono"),
      origin != "Livestock"
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(lv_in = sum(mg_n, na.rm = TRUE), .groups = "drop")

  lv_out <- flows |>
    dplyr::filter(
      origin == "Livestock",
      destiny %in%
        c(
          "Cropland",
          "semi_natural_agroecosystems",
          "population_food",
          "population_food_inedible",
          "population_other_uses",
          "export"
        )
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(lv_out = sum(mg_n, na.rm = TRUE), .groups = "drop")

  purrr::reduce(
    list(soil_in, soil_out, lv_in, lv_out, area_df),
    dplyr::full_join,
    by = c("year", "province_name")
  ) |>
    dplyr::mutate(
      year = as.numeric(year),
      dplyr::across(
        c(soil_in, soil_out, lv_in, lv_out),
        ~ dplyr::coalesce(.x, 0)
      ),
      surplus_mg = (soil_in - soil_out) + (lv_in - lv_out),
      value = surplus_mg * 1000 / area_ha
    ) |>
    # Negative surplus is a real value, not noise; only drop non-finite.
    dplyr::filter(is.finite(value))
}

.panel_pollution <- function(flows, area_df, typo_df, colors, periods) {
  summary_df <- .panel_typo_summary(
    .pollution_values(flows, area_df),
    typo_df
  )
  .panel_ts_plot(
    summary_df,
    "N surplus (kg N / ha)",
    colors,
    periods,
    ylim = c(0, 60)
  )
}

.panel_pollution_periods <- function(
  flows,
  area_df,
  typo_df,
  colors,
  opts = list()
) {
  opts <- utils::modifyList(list(top_row = TRUE, national = NULL), opts)
  spain_values <- if (is.null(opts$national)) {
    NULL
  } else {
    .pollution_values(opts$national$flows, opts$national$area_df)
  }
  .panel_periods_plot(
    .pollution_values(flows, area_df),
    typo_df,
    colors,
    "N surplus (kg N / ha)",
    list(
      top_row = opts$top_row,
      spain_values = spain_values,
      ylim = c(0, 60)
    )
  )
}

.intensification_values <- function(flows, area_df) {
  flows |>
    dplyr::filter(
      (origin == "Synthetic" &
        destiny %in% c("Cropland", "semi_natural_agroecosystems")) |
        (origin == "Outside" &
          destiny %in% c("livestock_rum", "livestock_mono"))
    ) |>
    dplyr::group_by(year, province_name) |>
    dplyr::summarise(intens_mg = sum(mg_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(area_df, by = c("year", "province_name")) |>
    dplyr::mutate(
      year = as.numeric(year),
      value = intens_mg * 1000 / area_ha
    ) |>
    dplyr::filter(is.finite(value))
}

.panel_intensification <- function(flows, area_df, typo_df, colors, periods) {
  summary_df <- .panel_typo_summary(
    .intensification_values(flows, area_df),
    typo_df
  )
  .panel_ts_plot(
    summary_df,
    "Synthetic + feed imports (kg N / ha)",
    colors,
    periods,
    ylim = c(0, 60)
  )
}

.panel_intensification_periods <- function(
  flows,
  area_df,
  typo_df,
  colors,
  opts = list()
) {
  opts <- utils::modifyList(list(top_row = TRUE, national = NULL), opts)
  spain_values <- if (is.null(opts$national)) {
    NULL
  } else {
    .intensification_values(opts$national$flows, opts$national$area_df)
  }
  .panel_periods_plot(
    .intensification_values(flows, area_df),
    typo_df,
    colors,
    "Synthetic + feed imports (kg N / ha)",
    list(
      top_row = opts$top_row,
      spain_values = spain_values,
      ylim = c(0, 60)
    )
  )
}
