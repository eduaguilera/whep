# diagnose_tomato_trade_basis.R
#
# Check the old-vintage trade anchor `fbs_trade_basis = "rebased"` puts FBS
# item 2601 "Tomatoes and products" on, against trade rebuilt from the
# traded products themselves (whep#1631).
#
# 1. Across all areas, the old vintage's import level against FAOSTAT's HS6
#    product trade converted at FAO's modal technical conversion factors.
# 2. For the largest EU importers, CN8 trade from Eurostat Comext, which
#    splits tomato paste by dry matter, converted under every factor set in
#    `.tomato_fresh_eq_factors()`, against the published new vintage,
#    the old vintage and the rebased import.
# 3. For Germany, the same on the BLE/BZL April-March marketing year, from
#    monthly Comext, against BLE table 4040500-0000.
#
# Needs the cached FAOSTAT pins and network access to the Eurostat Comext
# dissemination API (no key). Runs in about a minute. Not part of any build.
#
# Comext responses read for the whep#1631 report on 2026-11-06 (Comext
# "updated" stamp 2026-10-16), sha256 of the raw JSON:
#   annual, nine reporters, 2010-2013:
#     18db7f47b1620214be8a95247c5935ee133b359e3b124e53292033845b82fb4b
#   monthly, Germany, 2009-01 to 2014-03 (also ketchup, dried, values):
#     37df42aee5d9e9a5a29962292852f97eb545756fc5fc48eab66e078fcc0fa8a6a
# Comext revises back data, so a later download can differ slightly.

suppressPackageStartupMessages(devtools::load_all(".", quiet = TRUE))

comext_url <- function(reporters, freq, since, until) {
  products <- whep:::.tomato_cn8_classes()$cn8
  paste0(
    "https://ec.europa.eu/eurostat/api/comext/dissemination/statistics/",
    "1.0/data/DS-045409?format=JSON&lang=en&freq=",
    freq,
    "&partner=WORLD&flow=1&flow=2&indicators=QUANTITY_IN_100KG",
    paste0("&reporter=", reporters, collapse = ""),
    paste0("&product=", products, collapse = ""),
    "&sinceTimePeriod=",
    since,
    "&untilTimePeriod=",
    until
  )
}

# JSON-stat to a long tibble: one row per reporter, product, flow and period,
# in tonnes.
read_comext <- function(url) {
  js <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  dims <- unlist(js$id)
  labels <- purrr::map(dims, \(d) {
    idx <- unlist(js$dimension[[d]]$category$index)
    names(sort(idx))
  })
  sizes <- unlist(js$size)
  flat <- as.integer(names(js$value))
  strides <- rev(cumprod(rev(c(sizes[-1], 1))))
  cols <- purrr::map(seq_along(dims), \(i) {
    labels[[i]][(flat %/% strides[i]) %% sizes[i] + 1L]
  })
  names(cols) <- dims
  tibble::as_tibble(cols) |>
    dplyr::mutate(value = unlist(js$value) / 10) |>
    dplyr::transmute(
      reporter,
      cn8 = product,
      flow = dplyr::if_else(flow == "1", "import", "export"),
      period = time,
      value
    )
}

# Comext reporter -> FAOSTAT area_code, the code the FBS pins carry.
reporter_area <- tibble::tribble(
  ~reporter, ~area_code,
  "DE",      79L,
  "GB",      229L,
  "FR",      68L,
  "PL",      173L,
  "NL",      150L,
  "BE",      255L,
  "IT",      106L,
  "ES",      203L,
  "AT",      11L
)

years <- 2010:2013
fbs <- purrr::map(
  c(new = "faostat-fbs-new", old = "faostat-fbs-old"),
  \(alias) {
    whep:::.extract_cb(alias, years = years) |>
      tibble::as_tibble() |>
      dplyr::filter(
        item_cbs_code == 2601,
        area_code < 5000,
        area_code != 351
      ) |>
      dplyr::summarise(
        value = sum(value),
        .by = c(area_code, item_cbs_code, year, element)
      )
  }
)
rebase <- whep:::.fbs_rebase_factors(fbs$new, fbs$old, "rebased") |>
  tibble::as_tibble() |>
  dplyr::filter(item_cbs_code == 2601)
fbs_trade <- dplyr::full_join(
  dplyr::rename(fbs$new, fbs_new = value),
  dplyr::rename(fbs$old, fbs_old = value),
  by = c("area_code", "item_cbs_code", "year", "element")
) |>
  dplyr::filter(element %in% c("import", "export")) |>
  dplyr::left_join(
    dplyr::select(rebase, area_code, element, rebase_old_new),
    by = c("area_code", "element")
  ) |>
  dplyr::mutate(
    fbs_rebased = fbs_new * dplyr::coalesce(rebase_old_new, 1)
  )

# 1. The old vintage against FAO's modal conversion of HS6 trade -------------

hs6 <- whep:::.read_fao_trade(years = years) |>
  tibble::as_tibble() |>
  dplyr::filter(item_code_trade %in% 388:392) |>
  dplyr::summarise(
    value = sum(value),
    .by = c(area_code, year, element, item_code_trade)
  ) |>
  dplyr::mutate(
    cn8 = dplyr::case_match(
      item_code_trade,
      388 ~ "07020000",
      390 ~ "20095090",
      391 ~ "20029031",
      392 ~ "20021010"
    )
  ) |>
  dplyr::select(area_code, year, element, cn8, value)
hs6_modal <- whep:::.tomato_fresh_equivalent(hs6, "fao_tcf_modal")
anchor_check <- fbs_trade |>
  dplyr::inner_join(hs6_modal, by = c("area_code", "year", "element")) |>
  dplyr::filter(year == 2010, element == "import", fbs_old > 20000) |>
  dplyr::mutate(old_over_modal = fbs_old / fresh_equivalent)
cli::cli_h1("Old vintage / FAO modal conversion of HS6 trade, 2010 imports")
cli::cli_text("{nrow(anchor_check)} areas importing over 20 kt (old vintage).")
print(stats::quantile(
  anchor_check$old_over_modal,
  c(0.1, 0.25, 0.5, 0.75, 0.9)
))

# 2. EU importers on CN8 trade -------------------------------------------------

annual <- read_comext(
  comext_url(reporter_area$reporter, "A", min(years), max(years))
) |>
  dplyr::mutate(year = as.integer(period)) |>
  dplyr::left_join(reporter_area, by = "reporter")
cn8_fresh <- annual |>
  dplyr::select(area_code, reporter, year, element = flow, cn8, value) |>
  whep:::.tomato_fresh_equivalent() |>
  dplyr::select(-value) |>
  tidyr::pivot_wider(names_from = factor_set, values_from = fresh_equivalent)
eu <- fbs_trade |>
  dplyr::inner_join(cn8_fresh, by = c("area_code", "year", "element")) |>
  dplyr::mutate(
    dplyr::across(
      c(
        fbs_new,
        fbs_old,
        fbs_rebased,
        dplyr::starts_with(c("dm_", "fao_", "grade_"))
      ),
      \(x) x / 1000
    )
  ) |>
  dplyr::select(
    reporter,
    year,
    element,
    fbs_new,
    fbs_old,
    fbs_rebased,
    dm_bound_low,
    fao_tcf_modal,
    grade_typical,
    dm_bound_high
  ) |>
  dplyr::arrange(element, year, dplyr::desc(fbs_new))
cli::cli_h1("FBS 2601 trade and CN8 fresh equivalents, kt")
eu |>
  dplyr::filter(year %in% 2010:2011) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round)) |>
  as.data.frame() |>
  print()

# 3. Germany on the BLE marketing year ----------------------------------------

# BLE/BZL, *Verbrauch von Gemüse nach Arten*, table 4040500-0000, sheet
# "SJ 2025 Kapitel D, V", C25: tomatoes 2011/12, 1 000 t. Vorbemerkung A10:
# April-March marketing years; imports of products in fresh weight. A BLE
# press release of 2025-07-23 adds that the figure includes the tomatoes in
# processed products such as ketchup.
ble_2011_12_kt <- 2049.5467975

monthly <- read_comext(comext_url("DE", "M", "2011-01", "2012-12")) |>
  dplyr::mutate(
    year = as.integer(substr(period, 1, 4)),
    month = as.integer(substr(period, 6, 7)),
    marketing_year = dplyr::if_else(month >= 4L, year, year - 1L)
  )
de_period <- dplyr::bind_rows(
  monthly |>
    dplyr::filter(year == 2011L) |>
    dplyr::mutate(period = "calendar 2011"),
  monthly |>
    dplyr::filter(marketing_year == 2011L) |>
    dplyr::mutate(period = "April 2011-March 2012")
) |>
  dplyr::select(period, flow, cn8, value)
de_fresh <- de_period |>
  whep:::.tomato_fresh_equivalent() |>
  dplyr::select(-value) |>
  tidyr::pivot_wider(names_from = flow, values_from = fresh_equivalent)
de_fbs <- fbs$new |>
  dplyr::filter(area_code == 79L, year == 2011L) |>
  dplyr::select(element, value) |>
  tibble::deframe()
de <- de_fresh |>
  dplyr::mutate(
    net_import = import - export,
    domestic_supply = de_fbs[["production"]] + net_import,
    food = domestic_supply - de_fbs[["losses"]]
  ) |>
  dplyr::mutate(dplyr::across(import:food, \(x) x / 1000))
scope_extra <- de_period |>
  dplyr::inner_join(whep:::.tomato_cn8_classes(), by = "cn8") |>
  dplyr::filter(!in_fbs_2601) |>
  dplyr::summarise(
    net_product_kt = sum(dplyr::if_else(flow == "import", value, -value)) /
      1000,
    .by = c(period, product_class)
  )
cli::cli_h1("Germany, FBS 2601 scope, kt (production and losses from FBS 2011)")
de |>
  dplyr::arrange(period, food) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round)) |>
  as.data.frame() |>
  print()
cli::cli_text(
  "BLE 2011/12 consumption: {round(ble_2011_12_kt, 1)} kt, ketchup included."
)
cli::cli_h2("Net trade outside FBS 2601 that BLE may include, product kt")
print(scope_extra)
