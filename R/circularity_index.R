#' Calculate Finn Cycling Index for each province and year
#'
#' @description Calculates the Finn Cycling Index (FCI) for the GRAFS nitrogen
#'   flow network following Finn (1976) and Allesina & Ulanowicz (2004). The
#'   index measures the fraction of total system throughput that is cycled
#'   through internal compartments (Cropland, semi-natural agroecosystems,
#'   Livestock, and People).
#'
#' @param n_prov_destiny Nitrogen flows tibble from [create_n_prov_destiny()].
#'   If `NULL`, loaded automatically.
#'
#' @return A tibble with columns `year`, `province_name`, and `finn_index`.
#' @export
#'
#' @examples
#' # create_finn_indicator()
create_finn_indicator <- function(n_prov_destiny = NULL) {
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }

  mapping <- .finn_mapping()

  n_prov_destiny |>
    dplyr::filter(as.numeric(year) <= .grafs_last_year()) |>
    dplyr::group_by(year, province_name) |>
    dplyr::group_map(~ .finn_for_group(.x, .y, mapping)) |>
    purrr::list_rbind()
}


#' Plot Finn Cycling Index evolution and period comparison
#'
#' @description Creates three panels: (a) temporal evolution of mean FCI per
#'   typology with interquartile ribbon; (b) FCI distribution at four key
#'   periods as boxplots; (c) net change in mean FCI from first to last period.
#'
#' @param periods Integer vector of four years to highlight.
#' @param finn_data Pre-computed tibble from [create_finn_indicator()].
#'   If `NULL`, computed automatically (slow).
#' @param n_prov_destiny Passed to [create_finn_indicator()] when
#'   `finn_data` is `NULL`.
#'
#' @return A named list with ggplot objects `evolution`, `periods`, `change`.
#' @export
#'
#' @examples
#' # plots <- plot_finn_circularity()
plot_finn_circularity <- function(
  periods = c(1860, 1920, 1960, 2020),
  finn_data = NULL,
  n_prov_destiny = NULL
) {
  if (is.null(finn_data)) {
    finn_data <- create_finn_indicator(n_prov_destiny)
  }
  typologies <- create_typo_ts_plot() |>
    dplyr::select(year, province_name, Typology_base)
  fci_df <- finn_data |>
    dplyr::mutate(year = as.numeric(year)) |>
    dplyr::inner_join(typologies, by = c("year", "province_name")) |>
    dplyr::filter(!is.na(finn_index), !is.na(Typology_base)) |>
    dplyr::mutate(
      Typology_base = stringr::str_remove(
        Typology_base,
        " \\(intensive\\)| \\(extensive\\)"
      )
    )
  list(
    evolution = .plot_fci_evolution(fci_df, periods),
    periods = .plot_fci_periods(fci_df, periods),
    change = .plot_fci_change(fci_df, periods)
  )
}


# --- Private helpers: FCI calculation ----------------------------------------

.finn_mapping <- function() {
  list(
    compartments = c(
      "Cropland",
      "semi_natural_agroecosystems",
      "Livestock",
      "People"
    ),
    external_inputs = c("Outside", "Synthetic", "Fixation", "Deposition"),
    destiny_to_comp = tibble::tribble(
      ~destiny,                       ~to_comp,
      "Cropland",                     "Cropland",
      "semi_natural_agroecosystems",  "semi_natural_agroecosystems",
      "livestock_rum",                "Livestock",
      "livestock_mono",               "Livestock",
      "population_food",              "People",
      "population_other_uses",        "People"
    )
  )
}

.finn_for_group <- function(df, keys, mapping) {
  comps <- mapping$compartments

  flows <- df |>
    dplyr::left_join(mapping$destiny_to_comp, by = "destiny") |>
    dplyr::mutate(
      from_comp = dplyr::if_else(origin %in% comps, origin, NA_character_)
    )

  flow_matrix <- .finn_flow_matrix(
    dplyr::filter(flows, !is.na(from_comp), !is.na(to_comp)),
    comps
  )
  z <- .finn_input_vector(
    dplyr::filter(flows, origin %in% mapping$external_inputs, !is.na(to_comp)),
    comps
  )

  tibble::tibble(
    year = keys$year,
    province_name = keys$province_name,
    finn_index = .calculate_finn(flow_matrix, z)
  )
}

# Builds an n x n flow matrix where entry [i, j] is the flow from
# compartment i to j.
.finn_flow_matrix <- function(internal_flows, compartments) {
  n <- length(compartments)
  flow_matrix <- matrix(0, n, n, dimnames = list(compartments, compartments))
  if (nrow(internal_flows) == 0) {
    return(flow_matrix)
  }

  agg <- internal_flows |>
    dplyr::group_by(from_comp, to_comp) |>
    dplyr::summarise(mg_n = sum(mg_n, na.rm = TRUE), .groups = "drop")

  # Indexed as [from, to] via an explicit row/column matrix. Computing a
  # column-major linear index by hand transposes the matrix, which silently
  # turns the colSums() in .calculate_finn() into outflow instead of inflow.
  idx <- cbind(
    match(agg$from_comp, compartments),
    match(agg$to_comp, compartments)
  )
  flow_matrix[idx] <- agg$mg_n
  flow_matrix
}

.finn_input_vector <- function(ext_flows, compartments) {
  z <- stats::setNames(rep(0, length(compartments)), compartments)
  if (nrow(ext_flows) == 0) {
    return(z)
  }

  agg <- ext_flows |>
    dplyr::group_by(to_comp) |>
    dplyr::summarise(mg_n = sum(mg_n, na.rm = TRUE), .groups = "drop")

  z[agg$to_comp] <- agg$mg_n
  z
}

# Computes the Finn Cycling Index (FCI) via the Leontief inverse, following
# Finn (1976). Throughflow per compartment is the external input plus its
# total inflow from other compartments; the structural matrix normalises
# each flow by its receiving compartment's throughflow; the Leontief
# inverse of that structural matrix gives the expected number of visits to
# each compartment per unit of throughflow. FCI is the throughflow-weighted
# share of total system throughput that cycles back to its own
# compartment rather than passing straight through.
.calculate_finn <- function(flow_matrix, z) {
  throughflow <- z + colSums(flow_matrix)
  if (any(throughflow <= 0)) {
    return(NA_real_)
  }

  struct_matrix <- sweep(flow_matrix, 2, throughflow, "/")
  n <- nrow(flow_matrix)

  tryCatch(
    {
      leontief_inv <- solve(diag(n) - struct_matrix)
      d <- diag(leontief_inv)
      if (any(d <= 0)) {
        return(NA_real_)
      }
      sum(throughflow * (1 - 1 / d)) / sum(throughflow)
    },
    error = function(e) NA_real_
  )
}


# --- Private helpers: plots --------------------------------------------------

.finn_typology_colors <- function() {
  c(
    "Semi-natural agroecosystems" = "#66a61e",
    "Specialized cropping systems" = "#F7DD5A",
    "Specialized livestock systems" = "#b3001b",
    "Connected crop-livestock systems" = "#7A4F20",
    "Disconnected crop-livestock systems" = "#E67E00"
  )
}

.finn_fci_summary <- function(fci_df) {
  fci_df |>
    dplyr::group_by(year, Typology_base) |>
    dplyr::summarise(
      mean_fci = mean(finn_index, na.rm = TRUE),
      q25 = stats::quantile(finn_index, 0.25, na.rm = TRUE),
      q75 = stats::quantile(finn_index, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}

.plot_fci_evolution <- function(fci_df, periods) {
  colors <- .finn_typology_colors()
  summary_df <- .finn_fci_summary(fci_df)

  ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
      x = year,
      y = mean_fci,
      color = Typology_base,
      fill = Typology_base
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q25, ymax = q75),
      alpha = 0.15,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_vline(
      xintercept = periods,
      linetype = "dashed",
      color = "grey50",
      linewidth = 0.4
    ) +
    ggplot2::scale_y_continuous(
      name = "Finn Cycling Index (FCI)",
      sec.axis = ggplot2::sec_axis(
        ~ . / (1 - .),
        name = "CyCt (avg. recyclings per N atom)"
      )
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_x_continuous(breaks = seq(1860, 2020, by = 10)) +
    ggplot2::labs(x = "Year", color = "Typology", fill = "Typology") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

.fci_period_ranges <- function() {
  tibble::tribble(
    ~period_label, ~year_start, ~year_end,
    "1860-1870",   1860L,       1870L,
    "1920-1930",   1920L,       1930L,
    "1960-1970",   1960L,       1970L,
    "2010-2020",   2010L,       2020L
  )
}

.fci_typology_order <- function() {
  c(
    "Semi-natural agroecosystems",
    "Connected crop-livestock systems",
    "Disconnected crop-livestock systems",
    "Specialized cropping systems",
    "Specialized livestock systems"
  )
}

# Labels rows with a reference period ("1860-1870", ...) based on `year`,
# dropping rows outside all reference periods. Shared across period-based
# summaries and plots (here and in typology_panel_plot.R).
.assign_period_label <- function(df) {
  df |>
    dplyr::mutate(
      period_label = dplyr::case_when(
        year >= 1860 & year <= 1870 ~ "1860-1870",
        year >= 1920 & year <= 1930 ~ "1920-1930",
        year >= 1960 & year <= 1970 ~ "1960-1970",
        year >= 2010 & year <= 2020 ~ "2010-2020"
      )
    ) |>
    dplyr::filter(!is.na(period_label))
}

.plot_fci_periods <- function(fci_df, periods) {
  colors <- .finn_typology_colors()
  ranges <- .fci_period_ranges()
  typo_order <- .fci_typology_order()

  df <- fci_df |>
    dplyr::filter(!is.na(finn_index)) |>
    .assign_period_label() |>
    dplyr::mutate(
      period_label = factor(period_label, levels = ranges$period_label),
      Typology_base = factor(Typology_base, levels = typo_order)
    )

  means <- .fci_period_means(fci_df, ranges) |>
    dplyr::mutate(Typology_base = factor(Typology_base, levels = typo_order))

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = period_label, y = finn_index, color = Typology_base)
  ) +
    ggplot2::geom_jitter(width = 0.15, size = 1.2, alpha = 0.55) +
    ggplot2::geom_crossbar(
      data = means,
      ggplot2::aes(y = mean_fci, ymin = mean_fci, ymax = mean_fci),
      color = "black",
      width = 0.55,
      linewidth = 0.8
    ) +
    ggplot2::facet_wrap(
      ~Typology_base,
      nrow = 1,
      labeller = ggplot2::label_wrap_gen(width = 18)
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(x = NULL, y = "Finn Cycling Index (FCI)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

.plot_fci_change <- function(fci_df, periods) {
  ranges <- .fci_period_ranges()
  typo_colors <- .finn_typology_colors()
  typo_order <- .fci_typology_order()

  period_means <- .fci_period_means(fci_df, ranges)

  with_prev <- period_means |>
    dplyr::arrange(Typology_base, period_label) |>
    dplyr::group_by(Typology_base) |>
    dplyr::mutate(prev_fci = dplyr::lag(mean_fci)) |>
    dplyr::ungroup() |>
    dplyr::filter(period_label != "1860-1870") |>
    dplyr::mutate(
      change_pct = (mean_fci - prev_fci) / prev_fci * 100
    )

  sort_order <- with_prev |>
    dplyr::filter(period_label == "2010-2020") |>
    dplyr::arrange(change_pct) |>
    dplyr::pull(Typology_base)

  change_df <- with_prev |>
    dplyr::mutate(
      Typology_base = factor(Typology_base, levels = sort_order)
    )

  ggplot2::ggplot(
    change_df,
    ggplot2::aes(
      x = change_pct,
      y = Typology_base,
      fill = Typology_base
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(round(change_pct), "%"),
        hjust = dplyr::if_else(change_pct < 0, 1.1, -0.1)
      ),
      size = 3,
      color = "grey30"
    ) +
    ggplot2::facet_wrap(~period_label, ncol = 3) +
    ggplot2::scale_fill_manual(values = typo_colors) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.2, 0.2))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      x = "Change in mean FCI vs. previous period (%)",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

.fci_period_means <- function(fci_df, ranges) {
  fci_df |>
    dplyr::filter(!is.na(finn_index)) |>
    .assign_period_label() |>
    dplyr::group_by(Typology_base, period_label) |>
    dplyr::summarise(
      mean_fci = mean(finn_index, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      period_label = factor(period_label, levels = ranges$period_label)
    )
}
