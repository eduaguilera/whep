#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidyr)
  library(stringr)
})

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required to run this script.")
}

devtools::load_all(".")


build_indicator_data <- function() {
  flows <- create_n_prov_destiny()
  npp_ygpit <- whep_read_file("npp-ygpit")

  area_df <- npp_ygpit |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      area_ha = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )

  n_inputs <- flows |>
    dplyr::filter(
      Origin %in% c("Synthetic", "Deposition", "Fixation"),
      Destiny %in% c("Cropland", "semi_natural_agroecosystems")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      total_new_input_mgn = sum(MgN, na.rm = TRUE),
      .groups = "drop"
    )

  soil_input_shares <- flows |>
    dplyr::filter(
      Destiny %in% c("Cropland", "semi_natural_agroecosystems"),
      Origin %in%
        c(
          "Synthetic",
          "Livestock",
          "Fixation",
          "Deposition",
          "People"
        )
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      synthetic_share = dplyr::if_else(
        sum(MgN, na.rm = TRUE) > 0,
        sum(MgN[Origin == "Synthetic"], na.rm = TRUE) /
          sum(MgN, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    )

  feed_import_share <- flows |>
    dplyr::filter(
      Destiny %in% c("livestock_mono", "livestock_rum"),
      Origin %in% c("Cropland", "semi_natural_agroecosystems", "Outside")
    ) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      feed_import_share = dplyr::if_else(
        sum(MgN, na.rm = TRUE) > 0,
        sum(MgN[Origin == "Outside"], na.rm = TRUE) /
          sum(MgN, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    )

  typology_df <- create_typo_ts_plot() |>
    dplyr::select(Year, Province_name, Typology_base) |>
    dplyr::rename(Typology = Typology_base) |>
    dplyr::mutate(
      Typology = stringr::str_remove(
        Typology,
        " \\((intensive|extensive)\\)"
      )
    )

  n_inputs |>
    dplyr::left_join(area_df, by = c("Year", "Province_name")) |>
    dplyr::left_join(soil_input_shares, by = c("Year", "Province_name")) |>
    dplyr::left_join(feed_import_share, by = c("Year", "Province_name")) |>
    dplyr::left_join(typology_df, by = c("Year", "Province_name")) |>
    dplyr::mutate(
      intensification_kg_ha = dplyr::if_else(
        area_ha > 0,
        (total_new_input_mgn * 1000) / area_ha,
        NA_real_
      ),
      specialization_max = pmax(synthetic_share, feed_import_share),
      specialization_mean = (synthetic_share + feed_import_share) / 2,
      specialization_l2 = sqrt(
        (synthetic_share^2 + feed_import_share^2) / 2
      ),
      Period = dplyr::case_when(
        Year < 1900 ~ "1860-1900",
        Year < 1950 ~ "1900-1950",
        Year < 1990 ~ "1950-1990",
        TRUE ~ "1990-2021"
      )
    ) |>
    dplyr::filter(
      !is.na(Typology),
      !is.na(intensification_kg_ha),
      !is.na(synthetic_share),
      !is.na(feed_import_share)
    )
}

build_toy_indicator_data <- function() {
  set.seed(42)

  years <- seq(1860, 2020, by = 10)
  provinces <- paste0("Province_", seq_len(12))
  typologies <- c(
    "Semi-natural agroecosystems",
    "Specialized cropping systems",
    "Specialized livestock systems",
    "Connected crop-livestock systems",
    "Disconnected crop-livestock systems"
  )

  tidyr::crossing(
    Year = years,
    Province_name = provinces
  ) |>
    dplyr::mutate(
      Typology = sample(typologies, dplyr::n(), replace = TRUE),
      synthetic_share = pmin(
        0.95,
        pmax(
          0.02,
          0.15 + (Year - 1860) / 260 + stats::rnorm(dplyr::n(), 0, 0.08)
        )
      ),
      feed_import_share = pmin(
        0.95,
        pmax(
          0.02,
          0.10 + (Year - 1860) / 300 + stats::rnorm(dplyr::n(), 0, 0.10)
        )
      ),
      intensification_kg_ha = pmax(
        5,
        25 + (Year - 1860) * 0.8 + stats::rnorm(dplyr::n(), 0, 25)
      ),
      specialization_max = pmax(synthetic_share, feed_import_share),
      specialization_mean = (synthetic_share + feed_import_share) / 2,
      specialization_l2 = sqrt(
        (synthetic_share^2 + feed_import_share^2) / 2
      ),
      Period = dplyr::case_when(
        Year < 1900 ~ "1860-1900",
        Year < 1950 ~ "1900-1950",
        Year < 1990 ~ "1950-1990",
        TRUE ~ "1990-2021"
      )
    )
}

plot_intens_vs_spec <- function(df, out_dir) {
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = intensification_kg_ha,
      y = specialization_max,
      color = Typology
    )
  ) +
    ggplot2::geom_point(alpha = 0.30, size = 1.2) +
    ggplot2::facet_wrap(~Period) +
    ggplot2::labs(
      title = "Intensification vs specialization (max definition)",
      subtitle = "Specialization = max(synthetic share, feed import share)",
      x = "Intensification (kg N / ha)",
      y = "Specialization (0-1)",
      color = "Typology"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(out_dir, "01_intensification_vs_specialization.png"),
    plot = p,
    width = 11,
    height = 7,
    dpi = 300
  )
}

plot_component_shares <- function(df, out_dir) {
  ts_df <- df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      synthetic_share = mean(synthetic_share, na.rm = TRUE),
      feed_import_share = mean(feed_import_share, na.rm = TRUE),
      specialization_max = mean(specialization_max, na.rm = TRUE),
      intensification_kg_ha = mean(intensification_kg_ha, na.rm = TRUE),
      .groups = "drop"
    )

  share_long <- ts_df |>
    dplyr::select(
      Year,
      synthetic_share,
      feed_import_share,
      specialization_max
    ) |>
    tidyr::pivot_longer(
      cols = -Year,
      names_to = "indicator",
      values_to = "value"
    )

  p <- ggplot2::ggplot(
    share_long,
    ggplot2::aes(x = Year, y = value, color = indicator)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(
      values = c(
        synthetic_share = "#E41A1C",
        feed_import_share = "#377EB8",
        specialization_max = "#4DAF4A"
      ),
      labels = c(
        synthetic_share = "Synthetic share",
        feed_import_share = "Feed import share",
        specialization_max = "Specialization (max)"
      )
    ) +
    ggplot2::labs(
      title = "Specialization components over time",
      subtitle = "National means across provinces",
      x = "Year",
      y = "Share (0-1)",
      color = "Series"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(out_dir, "02_specialization_components.png"),
    plot = p,
    width = 11,
    height = 7,
    dpi = 300
  )
}

plot_spec_definitions <- function(df, out_dir) {
  compare_df <- df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      specialization_max = mean(specialization_max, na.rm = TRUE),
      specialization_mean = mean(specialization_mean, na.rm = TRUE),
      specialization_l2 = mean(specialization_l2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = -Year,
      names_to = "definition",
      values_to = "value"
    )

  p <- ggplot2::ggplot(
    compare_df,
    ggplot2::aes(x = Year, y = value, color = definition)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(
      values = c(
        specialization_max = "#1B9E77",
        specialization_mean = "#D95F02",
        specialization_l2 = "#7570B3"
      ),
      labels = c(
        specialization_max = "max(synthetic, feed import)",
        specialization_mean = "(synthetic + feed import) / 2",
        specialization_l2 = "sqrt((synthetic^2 + feed import^2)/2)"
      )
    ) +
    ggplot2::labs(
      title = "Specialization definition sensitivity",
      subtitle = "How the specialization trend changes with formula choice",
      x = "Year",
      y = "Specialization (0-1)",
      color = "Definition"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(out_dir, "03_specialization_definitions.png"),
    plot = p,
    width = 11,
    height = 7,
    dpi = 300
  )
}

write_explanation <- function(df, out_dir) {
  summary_df <- df |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      intensification_kg_ha = mean(intensification_kg_ha, na.rm = TRUE),
      synthetic_share = mean(synthetic_share, na.rm = TRUE),
      feed_import_share = mean(feed_import_share, na.rm = TRUE),
      specialization_max = mean(specialization_max, na.rm = TRUE),
      specialization_mean = mean(specialization_mean, na.rm = TRUE),
      specialization_l2 = mean(specialization_l2, na.rm = TRUE),
      .groups = "drop"
    )

  readr::write_csv(
    summary_df,
    file.path(out_dir, "indicator_summary_timeseries.csv")
  )

  txt <- c(
    "Intensification and specialization indicators: interpretation guide",
    "",
    "1) Intensification",
    "   Formula: intensification = new N inputs / area",
    "   In code: (Synthetic + Deposition + Fixation) in Mg N, converted to kg",
    "   and divided by area (ha).",
    "",
    "2) Specialization components",
    "   synthetic_share = Synthetic / total soil N inputs",
    "   feed_import_share = Outside feed N / total feed N to livestock",
    "",
    "3) Specialization in current plot code",
    "   specialization_max = max(synthetic_share, feed_import_share)",
    "   Interpretation: a province is classified as specialized when either",
    "   synthetic dependency OR imported feed dependency is high.",
    "",
    "4) Why confusion happens",
    "   The repository currently uses multiple specialization formulas in",
    "   different functions (max, average, and diversity-based metrics).",
    "   These are not numerically equivalent and can show different trends.",
    "",
    "5) Suggested rule",
    "   Keep one primary definition for reporting (e.g. specialization_max)",
    "   and show alternatives only in sensitivity checks.",
    "",
    "Generated files:",
    "  01_intensification_vs_specialization.png",
    "  02_specialization_components.png",
    "  03_specialization_definitions.png",
    "  indicator_summary_timeseries.csv"
  )

  writeLines(txt, con = file.path(out_dir, "indicator_explanation.txt"))
}

run_indicator_explainer <- function(
  out_dir = "inst/scripts/output/indicator_explainer",
  use_toy_data = FALSE
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  indicator_df <- if (isTRUE(use_toy_data)) {
    build_toy_indicator_data()
  } else {
    build_indicator_data()
  }

  if (nrow(indicator_df) == 0) {
    stop(
      paste0(
        "No rows available for plotting after data preparation. ",
        "Common causes are: missing typology join values, zero denominators ",
        "in share calculations, or zero area. Try `use_toy_data = TRUE` to ",
        "verify plotting and inspect intermediate joins in `build_indicator_data()`."
      )
    )
  }

  message(
    "Plotting ",
    nrow(indicator_df),
    " rows across years ",
    min(indicator_df$Year, na.rm = TRUE),
    "-",
    max(indicator_df$Year, na.rm = TRUE),
    "."
  )

  plot_intens_vs_spec(indicator_df, out_dir)
  plot_component_shares(indicator_df, out_dir)
  plot_spec_definitions(indicator_df, out_dir)
  write_explanation(indicator_df, out_dir)

  message("Created indicator explainer outputs in: ", out_dir)
  invisible(indicator_df)
}

if (sys.nframe() == 0) {
  run_indicator_explainer(use_toy_data = TRUE)
}
