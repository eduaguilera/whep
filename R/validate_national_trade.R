#' @title Validate national net trade balance
#'
#' @description
#' Validates the provincial GRAFS model by comparing Spain's national net
#' trade computed bottom-up (summing provincial net balances per item) against
#' official FAOSTAT figures. Because internal inter-province flows cancel out
#' pairwise, the sum of provincial net balances equals Spain's true
#' international net trade.
#'
#' @param n_prov_destiny Optional pre-computed output from
#'   `create_n_prov_destiny()`. If `NULL`, calls that function internally.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble with columns `year`, `item`, `net_prov`, `net_fao`, and
#'   `diff_net` (all in MgN).
#'
#' @export
#'
#' @examples
#' validate_national_trade(example = TRUE)
validate_national_trade <- function(n_prov_destiny = NULL, example = FALSE) {
  if (example) {
    return(.example_nat_trade())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }

  codes_coefs_items_full <- whep_read_file("codes_coefs_items_full")
  biomass_coefs <- whep::biomass_coefs
  pie_full_destinies_fm <- whep_read_file("pie_full_destinies_fm")

  # Items excluded: internal ecosystem flows (Grass, Crop residues groups)
  # have no real international trade; Fish and Agro-industry are modelled
  # as fully imported so the comparison with FAO is meaningless.
  # Acorns, Fodder mix, Fodder legumes are Primary crops used as internal
  # feed with negligible or no FAO trade data.
  excluded_boxes <- c("Fish", "Agro-industry")
  excluded_items <- c(
    codes_coefs_items_full |>
      dplyr::filter(group %in% c("Grass", "Crop residues")) |>
      dplyr::pull(item),
    "Acorns",
    "Fodder mix",
    "Fodder legumes"
  )

  prov_net <- .sum_provincial_net(
    n_prov_destiny,
    excluded_boxes,
    excluded_items
  )
  fao_net <- .compute_fao_net_n(
    pie_full_destinies_fm,
    codes_coefs_items_full,
    biomass_coefs,
    excluded_items
  )

  prov_net |>
    dplyr::full_join(fao_net, by = c("year", "item")) |>
    dplyr::mutate(
      net_prov = dplyr::coalesce(net_prov, 0),
      net_fao = dplyr::coalesce(net_fao, 0),
      diff_net = net_prov - net_fao
    )
}

.sum_provincial_net <- function(
  n_prov_destiny,
  excluded_boxes,
  excluded_items
) {
  # Inter-province flows cancel: A's export to B equals B's import from A.
  n_prov_destiny |>
    dplyr::filter(
      !box %in% excluded_boxes,
      !item %in% excluded_items
    ) |>
    dplyr::summarise(
      net_prov = sum(
        dplyr::case_when(
          destiny == "export" ~ mg_n,
          origin == "Outside" ~ -mg_n,
          TRUE ~ 0
        ),
        na.rm = TRUE
      ),
      .by = c("year", "item")
    )
}

.compute_fao_net_n <- function(
  pie_full_destinies_fm,
  codes_coefs_items_full,
  biomass_coefs,
  excluded_items
) {
  excluded_by_box <- codes_coefs_items_full |>
    dplyr::mutate(
      group = dplyr::recode(group, "Additives" = "Agro-industry")
    ) |>
    dplyr::filter(group %in% c("Fish", "Agro-industry")) |>
    dplyr::pull(item)

  all_excluded <- union(excluded_by_box, excluded_items)

  pie_full_destinies_fm |>
    dplyr::filter(
      Element %in% c("Export", "Import"),
      !Item %in% all_excluded
    ) |>
    dplyr::group_by(Year, Item, Element) |>
    dplyr::summarise(
      value_fm = sum(Value_destiny, na.rm = TRUE),
      .groups = "drop"
    ) |>
    .convert_trade_fm_to_n(codes_coefs_items_full, biomass_coefs) |>
    dplyr::summarise(
      net_fao = sum(
        dplyr::if_else(Element == "Export", value_n, -value_n),
        na.rm = TRUE
      ),
      .by = c("Year", "Item")
    ) |>
    dplyr::rename(year = Year, item = Item)
}

#' @title Validate national net trade against raw historical FAO series
#'
#' @description
#' Validates the provincial GRAFS model's bottom-up national net trade against
#' the original historical Export/Import series for Spain contained in
#' `Europe_FAO_completed.xlsx` (1849-1960), the raw source data behind the
#' package's processed trade figures. Comparison is restricted to the item and
#' year combinations actually reported in the raw sheets, since coverage there
#' is sparser than in the processed dataset.
#'
#' @param n_prov_destiny Optional pre-computed output from
#'   `create_n_prov_destiny()`. If `NULL`, calls that function internally.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble with columns `year`, `item`, `net_prov`, `net_fao`, and
#'   `diff_net` (all in MgN).
#'
#' @export
#'
#' @examples
#' validate_national_trade_raw(example = TRUE)
validate_national_trade_raw <- function(
  n_prov_destiny = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_nat_trade_raw())
  }
  if (is.null(n_prov_destiny)) {
    n_prov_destiny <- create_n_prov_destiny()
  }

  codes_coefs_items_full <- whep_read_file("codes_coefs_items_full")
  biomass_coefs <- whep::biomass_coefs
  excluded_items <- .default_excluded_trade_items(codes_coefs_items_full)

  prov_net <- .sum_provincial_net(
    n_prov_destiny,
    c("Fish", "Agro-industry"),
    excluded_items
  )
  raw_net <- .compute_raw_excel_net_n(
    codes_coefs_items_full,
    biomass_coefs,
    excluded_items
  )

  raw_net |>
    dplyr::left_join(prov_net, by = c("year", "item")) |>
    dplyr::mutate(
      net_prov = dplyr::coalesce(net_prov, 0),
      diff_net = net_prov - net_fao
    )
}

#' @title Compute national trade flows: model vs. raw historical FAO series
#'
#' @description
#' Computes gross export and import flows (in MgN) by item and year, both
#' from the national GRAFS model (production and consumption aggregated to
#' Spain before splitting into export/import, avoiding the double-counting of
#' inter-provincial trade that a province-level sum would introduce) and from
#' the raw historical Export/Import series for Spain in
#' `Europe_FAO_completed.xlsx` (1849-1960), restricted to the item/year
#' combinations reported in the raw sheets. Items are classified as `"Crop"`
#' or `"Livestock"` for downstream aggregation, e.g. in
#' `plot_national_trade_flows_raw()`.
#'
#' @param n_nat_destiny Optional pre-computed output from
#'   `create_n_nat_destiny()`. If `NULL`, calls that function internally.
#' @param example If `TRUE`, return a small hardcoded output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble with columns `year`, `item`, `category` (`"Crop"` or
#'   `"Livestock"`), `source` (`"WHEP model"` or `"FAO (raw)"`), `flow`
#'   (`"Export"` or `"Import"`), and `value_n` (MgN).
#'
#' @export
#'
#' @examples
#' compute_trade_flows_raw(example = TRUE)
compute_trade_flows_raw <- function(n_nat_destiny = NULL, example = FALSE) {
  if (example) {
    return(.example_trade_flows_raw())
  }
  if (is.null(n_nat_destiny)) {
    n_nat_destiny <- create_n_nat_destiny()
  }

  codes_coefs_items_full <- whep_read_file("codes_coefs_items_full")
  biomass_coefs <- whep::biomass_coefs
  excluded_items <- .default_excluded_trade_items(codes_coefs_items_full)

  national_flows <- .sum_national_flows(
    n_nat_destiny,
    c("Fish", "Agro-industry"),
    excluded_items
  )
  raw_flows <- .compute_raw_excel_flows_n(
    codes_coefs_items_full,
    biomass_coefs,
    excluded_items
  )

  .combine_trade_flow_sources(national_flows, raw_flows, codes_coefs_items_full)
}

#' @title Plot national net trade validation
#'
#' @description
#' Plots a time series comparing Spain's national net trade computed
#' bottom-up from the provincial model against official FAOSTAT figures.
#' Values are aggregated over all items per year.
#'
#' @param validation Optional pre-computed output from
#'   `validate_national_trade()`. If `NULL`, calls that function internally.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' validation <- tibble::tribble(
#'   ~year, ~item, ~net_prov, ~net_fao, ~diff_net,
#'   1960, "Barley and products", 12000, 9500, 2500,
#'   1960, "Bovine Meat", -3000, -2800, -200,
#'   2000, "Barley and products", 56943, 2175, 54768,
#'   2000, "Bovine Meat", 1601, 1569, 32
#' )
#' p <- plot_national_trade_validation(validation)
plot_national_trade_validation <- function(validation = NULL) {
  if (is.null(validation)) {
    validation <- validate_national_trade()
  }

  plot_data <- validation |>
    dplyr::summarise(
      net_prov = sum(net_prov, na.rm = TRUE),
      net_fao = sum(net_fao, na.rm = TRUE),
      .by = "year"
    )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = year)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = net_fao / 1000, ymax = net_prov / 1000),
      fill = "grey80",
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = net_prov / 1000, colour = "Provincial model"),
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = net_fao / 1000, colour = "FAOSTAT"),
      linewidth = 0.8
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey50"
    ) +
    ggplot2::scale_colour_manual(
      values = c("Provincial model" = "#2166ac", "FAOSTAT" = "#d6604d")
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Net trade (Gg N)",
      colour = NULL,
      title = "National net trade: provincial model vs. FAOSTAT"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}

#' @title Plot national trade flows: model vs. raw historical FAO series
#'
#' @description
#' Plots time series of gross exports and imports, aggregated to Crop vs.
#' Livestock, comparing the provincial GRAFS model's bottom-up national trade
#' against the raw historical Export/Import series for Spain.
#'
#' @param trade_flows Optional pre-computed output from
#'   `compute_trade_flows_raw()`. If `NULL`, calls that function
#'   internally.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @examples
#' trade_flows <- tibble::tribble(
#'   ~year, ~item, ~source, ~category, ~flow, ~value_n,
#'   1930, "Nuts and products", "FAO (raw)", "Crop", "Export", 825,
#'   1930, "Nuts and products", "WHEP model", "Crop", "Export", 910,
#'   1930, "Bovine Meat", "FAO (raw)", "Livestock", "Import", 340,
#'   1930, "Bovine Meat", "WHEP model", "Livestock", "Import", 295
#' )
#' p <- plot_national_trade_flows_raw(trade_flows)
plot_national_trade_flows_raw <- function(trade_flows = NULL) {
  if (is.null(trade_flows)) {
    trade_flows <- compute_trade_flows_raw()
  }

  plot_data <- trade_flows |>
    dplyr::summarise(
      value_n = sum(value_n, na.rm = TRUE),
      .by = c(year, category, source, flow)
    )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = year,
      y = value_n / 1000,
      colour = source,
      linetype = flow
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~category, scales = "free_y") +
    ggplot2::scale_colour_manual(
      values = c("WHEP model" = "#2166ac", "FAO (raw)" = "#d6604d")
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Trade flow (Gg N)",
      colour = NULL,
      linetype = NULL,
      title = "National trade flows: WHEP model vs. raw historical FAO series"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}

.default_excluded_trade_items <- function(codes_coefs_items_full) {
  # Grass/Crop residues have no real international trade; Acorns, Fodder mix
  # and Fodder legumes are Primary crops used as internal feed with
  # negligible or no FAO trade data.
  c(
    codes_coefs_items_full |>
      dplyr::filter(group %in% c("Grass", "Crop residues")) |>
      dplyr::pull(item),
    "Acorns",
    "Fodder mix",
    "Fodder legumes"
  )
}

.sum_national_flows <- function(
  n_nat_destiny,
  excluded_boxes,
  excluded_items
) {
  # Unlike a province-level sum, n_nat_destiny already nets production and
  # consumption to Spain as a whole before splitting export/import, so gross
  # flows here aren't inflated by inter-provincial trade.
  n_nat_destiny |>
    dplyr::filter(
      !box %in% excluded_boxes,
      !item %in% excluded_items
    ) |>
    dplyr::summarise(
      export = sum(dplyr::if_else(destiny == "export", mg_n, 0), na.rm = TRUE),
      import = sum(dplyr::if_else(origin == "Outside", mg_n, 0), na.rm = TRUE),
      .by = c(year, item)
    )
}

.classify_item_category <- function(items, codes_coefs_items_full) {
  codes_coefs_items_full |>
    dplyr::filter(item %in% items) |>
    dplyr::distinct(item, group) |>
    dplyr::mutate(
      category = dplyr::if_else(
        group %in% c("Livestock", "Livestock products"),
        "Livestock",
        "Crop"
      )
    ) |>
    dplyr::select(item, category)
}

.combine_trade_flow_sources <- function(
  national_flows,
  raw_flows,
  codes_coefs_items_full
) {
  categories <- .classify_item_category(
    unique(raw_flows$item),
    codes_coefs_items_full
  )

  dplyr::bind_rows(
    raw_flows |> dplyr::mutate(source = "FAO (raw)"),
    national_flows |>
      dplyr::semi_join(raw_flows, by = c("year", "item")) |>
      dplyr::mutate(source = "WHEP model")
  ) |>
    dplyr::inner_join(categories, by = "item") |>
    tidyr::pivot_longer(
      cols = c(export, import),
      names_to = "flow",
      values_to = "value_n"
    ) |>
    dplyr::mutate(flow = stringr::str_to_title(flow))
}

.convert_trade_fm_to_n <- function(
  trade_fm,
  codes_coefs_items_full,
  biomass_coefs
) {
  trade_fm |>
    dplyr::left_join(
      codes_coefs_items_full |> dplyr::select(item, Name_biomass),
      by = c("Item" = "item")
    ) |>
    dplyr::left_join(
      biomass_coefs |>
        dplyr::select(
          Name_biomass,
          Product_kgDM_kgFM,
          Product_kgN_kgDM,
          Residue_kgDM_kgFM,
          Residue_kgN_kgDM
        ),
      by = "Name_biomass"
    ) |>
    dplyr::mutate(
      prod_type = dplyr::case_when(
        Name_biomass %in% c("Grass", "Fallow") ~ "Grass",
        Name_biomass == "Average wood" ~ "Residue",
        TRUE ~ "Product"
      ),
      dm_coef = dplyr::if_else(
        prod_type == "Product",
        Product_kgDM_kgFM,
        dplyr::coalesce(Residue_kgDM_kgFM, Product_kgDM_kgFM)
      ),
      n_coef = dplyr::if_else(
        prod_type == "Product",
        Product_kgN_kgDM,
        dplyr::coalesce(Residue_kgN_kgDM, Product_kgN_kgDM)
      ),
      value_n = value_fm * dm_coef * n_coef
    ) |>
    dplyr::select(Year, Item, Element, value_n)
}

.compute_raw_excel_net_n <- function(
  codes_coefs_items_full,
  biomass_coefs,
  excluded_items
) {
  .prepare_raw_excel_trade_n(
    codes_coefs_items_full,
    biomass_coefs,
    excluded_items
  ) |>
    dplyr::summarise(
      net_fao = sum(
        dplyr::if_else(Element == "Export", value_n, -value_n),
        na.rm = TRUE
      ),
      .by = c(Year, Item)
    ) |>
    dplyr::rename(year = Year, item = Item)
}

.compute_raw_excel_flows_n <- function(
  codes_coefs_items_full,
  biomass_coefs,
  excluded_items
) {
  .prepare_raw_excel_trade_n(
    codes_coefs_items_full,
    biomass_coefs,
    excluded_items
  ) |>
    tidyr::pivot_wider(
      names_from = Element,
      values_from = value_n,
      values_fill = 0
    ) |>
    dplyr::rename(year = Year, item = Item, export = Export, import = Import)
}

.prepare_raw_excel_trade_n <- function(
  codes_coefs_items_full,
  biomass_coefs,
  excluded_items
) {
  excluded_by_box <- codes_coefs_items_full |>
    dplyr::mutate(
      group = dplyr::recode(group, "Additives" = "Agro-industry")
    ) |>
    dplyr::filter(group %in% c("Fish", "Agro-industry")) |>
    dplyr::pull(item)

  all_excluded <- union(excluded_by_box, excluded_items)

  .read_raw_trade_data() |>
    dplyr::inner_join(whep::cbs_trade_codes, by = c("Item" = "item_trade")) |>
    dplyr::mutate(Item = item_cbs) |>
    dplyr::filter(!Item %in% all_excluded) |>
    dplyr::summarise(
      value_fm = sum(value_fm, na.rm = TRUE),
      .by = c(Year, Item, Element)
    ) |>
    .convert_trade_fm_to_n(codes_coefs_items_full, biomass_coefs)
}

# Reads Spain's historical Export/Import series, already reshaped to long
# format and converted to Mg (fresh matter) by
# `data-raw/europe_fao_spain_trade.R`. That script extracts it from the source
# workbook `Europe_FAO_completed.xlsx`, a 19-sheet 35-country compilation of
# which only these two sheets and only the Spanish rows are ever used; the
# workbook is not shipped with the package.
.read_raw_trade_data <- function() {
  path <- system.file(
    "extdata",
    "europe_fao_spain_trade.csv",
    package = "whep"
  )

  readr::read_csv(
    path,
    col_types = readr::cols(
      Element = readr::col_character(),
      Item = readr::col_character(),
      Year = readr::col_integer(),
      value_fm = readr::col_double()
    )
  )
}
