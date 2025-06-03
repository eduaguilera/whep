#' Plots
#' Plot Spain Destinies ----------------------------------------------------------------------------------------------------------------------
#' Summarize and transform MgN → GgN; make Imports negative
MgN_time_series <- GRAFS_prod_destiny_final |>
  dplyr::group_by(Year, Destiny) |>
  dplyr::summarise(
    MgN_total = sum(MgN, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    GgN_total = MgN_total / 1000,
    GgN_total = ifelse(Destiny == "Import", -GgN_total, GgN_total)
  )

#' Create a time series line plot in GgN
ggplot2::ggplot(MgN_time_series, aes(x = Year, y = GgN_total, color = Destiny)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggplot2::labs(
    title = "Nitrogen in Spain by Destiny",
    x = "Year",
    y = "Nitrogen (GgN)",
    color = "Destiny"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = "N_Spain_Destiny.png",
  width = 16,
  height = 10,
  dpi = 300
)


#' Plot Provinces Destinies -------------------------------------------------------------------------------------------------------------------
#' Summarize and transform MgN to GgN; set Import to negative
MgN_time_series_province <- GRAFS_prod_destiny_final |>
  dplyr::filter(Province_name != "Sea") |>
  dplyr::group_by(Year, Province_name, Destiny) |>
  dplyr::summarise(MgN_total = sum(MgN, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(
    GgN_total = MgN_total / 1000,
    GgN_total = ifelse(Destiny == "Import", -GgN_total, GgN_total)
  )

#' Plot with facet per province and horizontal zero line
ggplot2::ggplot(MgN_time_series_province, aes(x = Year, y = GgN_total, color = Destiny)) +
  ggplot2::geom_line(size = 0.8) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggplot2::facet_wrap(~Province_name, scales = "free_y") +
  ggplot2::labs(
    title = "Nitrogen Spain per Destiny and Province",
    x = "Year",
    y = "Gg N",
    color = "Destiny"
  ) +
  ggplot2::theme_minimal(base_size = 8) +
  ggplot2::theme(
    strip.text = element_text(size = 7),
    axis.text = element_text(size = 6),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  filename = "N_Spain_Provinces.png",
  width = 16,
  height = 10,
  dpi = 300
)

#' Plot per Box and provinces -----------------------------------------------------------------------------------------------------------------------
#' Summarize MgN per year, province, and box
MgN_time_series_box_province <- GRAFS_prod_destiny_final |>
  dplyr::filter(Province_name != "Sea") |>
  dplyr::group_by(Year, Province_name, Box) |>
  dplyr::summarise(MgN_total = sum(MgN, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(
    GgN_total = MgN_total / 1000
  )


#' Faceted time series plot per province
ggplot2::ggplot(MgN_time_series_box_province, aes(x = Year, y = GgN_total, color = Box)) +
  ggplot2::geom_line(size = 0.8) +
  ggplot2::facet_wrap(~Province_name, scales = "free_y") +
  ggplot2::labs(
    title = "Nitrogen in Spain by Box and Province",
    x = "Year",
    y = "Gg N",
    color = "Box"
  ) +
  ggplot2::theme_minimal(base_size = 8) +
  ggplot2::theme(
    strip.text = element_text(size = 7),
    axis.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 9, face = "bold"),
    legend.position = "bottom"
  )
ggplot2::ggsave(
  filename = "N_Spain_Provinces_Box.png",
  width = 16,
  height = 10,
  dpi = 300
)
