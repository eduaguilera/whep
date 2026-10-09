.npp_coef_ref <- function(coefs, model, param, component = NULL) {
  m <- coefs$model == model & coefs$parameter == param
  if (!is.null(component)) {
    m <- m & coefs$component == component
  }
  coefs$value[m]
}

test_that("calculate_potential_npp miami matches the afsetools formula", {
  x <- tibble::tibble(temp_c = 15, water_input_mm = 600, aet_mm = 450)
  out <- whep::calculate_potential_npp(x, method = "miami")

  testthat::expect_true(
    all(c("npp_potential_dm_t_ha", "method_npp_potential") %in% names(out))
  )
  testthat::expect_equal(out$method_npp_potential, "miami")

  coefs <- whep::whep_coef_table("npp_model_coefs")
  t_max <- .npp_coef_ref(coefs, "Miami", "Max_gCm2yr", "F_MAT")
  t_mid <- .npp_coef_ref(coefs, "Miami", "Midpoint", "F_MAT")
  t_rate <- .npp_coef_ref(coefs, "Miami", "Rate", "F_MAT")
  p_max <- .npp_coef_ref(coefs, "Miami", "Max_gCm2yr", "F_MAP")
  p_rate <- .npp_coef_ref(coefs, "Miami", "Rate", "F_MAP")
  expected <- pmin(
    (t_max / (1 + exp(t_mid - t_rate * 15))) / 100,
    (p_max * (1 - exp(-p_rate * 600))) / 100
  )
  testthat::expect_equal(out$npp_potential_dm_t_ha, expected)
})

test_that("calculate_potential_npp nceas uses non-tree ANPP", {
  x <- tibble::tibble(temp_c = 15, water_input_mm = 600, aet_mm = 450)
  out <- whep::calculate_potential_npp(x, method = "nceas")

  coefs <- whep::whep_coef_table("npp_model_coefs")
  carbon_fraction <- whep::whep_coef_table("weed_coefs")$residue_c_kgdm_weed
  a_max <- .npp_coef_ref(coefs, "NCEAS_nontree_ANPP", "Max_gCm2yr")
  a_rate <- .npp_coef_ref(coefs, "NCEAS_nontree_ANPP", "Rate")
  expected <- a_max * (1 - exp(-a_rate * 600)) / (100 * carbon_fraction)

  testthat::expect_equal(out$npp_potential_dm_t_ha, expected)
  testthat::expect_gt(out$npp_potential_dm_t_ha, 0)
})

test_that("lpjml potential-NPP uses gross above-ground grass NPP", {
  cells <- whep::read_lpjml_grass_productivity(example = TRUE)
  x <- dplyr::select(cells, lon, lat, year)
  out <- whep::calculate_potential_npp(
    x,
    method = "lpjml",
    lpjml = list(example = TRUE)
  )
  sh <- whep::grass_access_shares()
  joined <- dplyr::left_join(out, cells, by = c("lon", "lat", "year"))
  testthat::expect_equal(
    joined$npp_potential_dm_t_ha,
    joined$grass_npp * sh$aboveground * sh$grazable * 0.01 / sh$w_c_dm
  )
  testthat::expect_equal(unique(out$method_npp_potential), "lpjml")
})

test_that("lpjml potential-NPP errors without grid coordinates", {
  testthat::expect_error(
    whep::calculate_potential_npp(
      tibble::tibble(year = 2000),
      method = "lpjml"
    ),
    "lon"
  )
})

test_that("calculate_crop_residues ipcc matches the IPCC linear model", {
  x <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  out <- whep::calculate_crop_residues(x, method = "ipcc")

  bc <- whep::whep_coef_table("bio_coefs")
  rc <- whep::whep_coef_table("ipcc_residue_coefs")
  mp <- whep::whep_coef_table("ipcc_crop_mapping")
  pd <- bc$product_dm_kgfm[bc$item_prod_code == "15"]
  ic <- mp$ipcc_crop[mp$item_prod_code == "15"]
  sl <- rc$slope_ag[rc$ipcc_crop == ic]
  it <- rc$intercept_ag_dm_t_ha[rc$ipcc_crop == ic]
  expected <- pmax(sl * (100 * pd / 40) + it, 0) * 40

  testthat::expect_equal(out$residue_dm_t, expected)
  testthat::expect_equal(out$method_residue, "ipcc")
  testthat::expect_equal(out$product_dm_t, 100 * pd)
})

test_that("calculate_crop_residues ratio uses the bio_coefs ratio", {
  x <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  out <- whep::calculate_crop_residues(x, method = "ratio")
  bc <- whep::whep_coef_table("bio_coefs")
  rr <- bc$residue_kg_product_fm_kg[bc$item_prod_code == "15"]
  rd <- bc$residue_dm_kgfm[bc$item_prod_code == "15"]
  testthat::expect_equal(out$residue_dm_t, 100 * rr * rd)
})

test_that("calculate_crop_residues ensemble blends ipcc and ratio", {
  x <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  ens <- whep::calculate_crop_residues(x, method = "ensemble")$residue_dm_t
  ipcc <- whep::calculate_crop_residues(x, method = "ipcc")$residue_dm_t
  ratio <- whep::calculate_crop_residues(x, method = "ratio")$residue_dm_t
  testthat::expect_equal(ens, 0.5 * ipcc + 0.5 * ratio)
})

test_that("calculate_crop_residues errors on missing columns", {
  testthat::expect_error(
    whep::calculate_crop_residues(tibble::tibble(item_prod_code = "15")),
    "missing required"
  )
})

test_that("calculate_crop_roots root_shoot uses the IPCC RS ratio", {
  x <- tibble::tibble(
    item_prod_code = "15",
    product_dm_t = 87.9,
    residue_dm_t = 135.75,
    area_ha = 40
  )
  out <- whep::calculate_crop_roots(x, method = "root_shoot")
  rk <- whep::whep_coef_table("ipcc_root_coefs")
  mp <- whep::whep_coef_table("ipcc_crop_mapping")
  rs <- rk$rs_default[rk$ipcc_crop == mp$ipcc_crop[mp$item_prod_code == "15"]]
  testthat::expect_equal(out$root_dm_t, (87.9 + 135.75) * rs)
  testthat::expect_equal(out$method_root, "root_shoot")
})

test_that("calculate_crop_roots reference uses BG ref per hectare", {
  x <- tibble::tibble(
    item_prod_code = "15",
    product_dm_t = 87.9,
    residue_dm_t = 135.75,
    area_ha = 40
  )
  out <- whep::calculate_crop_roots(x, method = "reference")
  rk <- whep::whep_coef_table("ipcc_root_coefs")
  mp <- whep::whep_coef_table("ipcc_crop_mapping")
  bg <- rk$bg_ref_dm_t_ha[
    rk$ipcc_crop == mp$ipcc_crop[mp$item_prod_code == "15"]
  ]
  testthat::expect_equal(out$root_dm_t, bg * 40)
})

test_that("calculate_crop_roots ensemble blends RS and reference", {
  x <- tibble::tibble(
    item_prod_code = "15",
    product_dm_t = 87.9,
    residue_dm_t = 135.75,
    area_ha = 40
  )
  ens <- whep::calculate_crop_roots(x, method = "ensemble")$root_dm_t
  rs <- whep::calculate_crop_roots(x, method = "root_shoot")$root_dm_t
  ref <- whep::calculate_crop_roots(x, method = "reference")$root_dm_t
  testthat::expect_equal(ens, 0.5 * rs + 0.5 * ref)
})

test_that("calculate_crop_npp sums product, residue and root", {
  x <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  out <- whep::calculate_crop_npp(x)
  testthat::expect_true(
    all(
      c(
        "product_dm_t",
        "residue_dm_t",
        "root_dm_t",
        "crop_npp_dm_t",
        "method_residue",
        "method_root"
      ) %in%
        names(out)
    )
  )
  testthat::expect_equal(
    out$crop_npp_dm_t,
    out$product_dm_t + out$residue_dm_t + out$root_dm_t
  )
})

test_that("calculate_npp_carbon_nitrogen partitions N and C", {
  base <- tibble::tibble(
    item_prod_code = "15",
    production_t = 100,
    area_ha = 40
  ) |>
    whep::calculate_crop_npp()
  out <- whep::calculate_npp_carbon_nitrogen(base)
  bc <- whep::whep_coef_table("bio_coefs")
  r <- bc[bc$item_prod_code == "15", ]
  testthat::expect_equal(out$product_n_t, out$product_dm_t * r$product_n_kgdm)
  testthat::expect_equal(
    out$root_n_t,
    out$root_dm_t * r$root_n_kgdm * (1 + r$rhizodeposit_n_kgn_krootn)
  )
  testthat::expect_equal(
    out$crop_npp_n_t,
    out$product_n_t + out$residue_n_t + out$root_n_t
  )
  testthat::expect_equal(out$total_npp_dm_t, out$crop_npp_dm_t)
  testthat::expect_false("residue_soil_n_t" %in% names(out))
})

test_that("calculate_npp_carbon_nitrogen adds soil residue when destiny present", {
  base <- tibble::tibble(
    item_prod_code = "15",
    production_t = 100,
    area_ha = 40,
    residue_soil_dm_t = 50
  ) |>
    whep::calculate_crop_npp()
  out <- whep::calculate_npp_carbon_nitrogen(base)
  bc <- whep::whep_coef_table("bio_coefs")
  rn <- bc$residue_n_kgdm[bc$item_prod_code == "15"]
  testthat::expect_equal(out$residue_soil_n_t, 50 * rn)
})

test_that("calculate_crop_npp_components scales weeds and flags provisional", {
  base <- tibble::tibble(
    item_prod_code = "15",
    production_t = 100,
    area_ha = 40,
    year = 2000,
    npp_potential_dm_t_ha = 5
  ) |>
    whep::calculate_crop_npp()
  out <- suppressWarnings(whep::calculate_crop_npp_components(base))
  ws <- whep::whep_coef_table("weed_npp_scaling")
  sc <- ws$weed_scaling[ws$item_prod_code == "15" & ws$year == 2000]
  testthat::expect_equal(out$weed_ag_dm_t, 40 * sc * 5)
  testthat::expect_true(out$weed_scaling_to_be_revised)
  testthat::expect_true(
    all(c("weed_npp_dm_t", "total_npp_n_t", "weed_npp_c_t") %in% names(out))
  )
})

test_that("calculate_crop_npp_components warns it is Spain-specific", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  base <- tibble::tibble(
    item_prod_code = "15",
    production_t = 100,
    area_ha = 40,
    year = 2000,
    npp_potential_dm_t_ha = 5
  ) |>
    whep::calculate_crop_npp()
  testthat::expect_warning(
    whep::calculate_crop_npp_components(base),
    "to_be_revised|Spain"
  )
})

test_that("calculate_crop_residues handles zero area and unknown items", {
  z <- whep::calculate_crop_residues(
    tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 0)
  )
  testthat::expect_equal(z$yield_dm_t_ha, 0)
  testthat::expect_true(is.finite(z$residue_dm_t))
  u <- whep::calculate_crop_residues(
    tibble::tibble(item_prod_code = "NOPE", production_t = 100, area_ha = 40)
  )
  testthat::expect_equal(u$residue_dm_t, 0)
  testthat::expect_equal(nrow(u), 1L)
})

test_that("calculate_crop_roots floors at zero for items with no RS source", {
  out <- whep::calculate_crop_roots(tibble::tibble(
    item_prod_code = "NOPE",
    product_dm_t = 10,
    residue_dm_t = 5,
    area_ha = 4
  ))
  testthat::expect_equal(out$root_dm_t, 0)
})

# whep#1034. The weed carbon stream is a required input of
# build_soil_carbon_inputs(), and on the default path it is identically zero
# because nothing on that path produces weed biomass. Every property the NPP
# partition is tested by is satisfied by that zero.

test_that("an absent weed stream is stamped, not merely zeroed", {
  out <- tibble::tibble(
    item_prod_code = c("15", "56"),
    production_t = c(100, 200),
    area_ha = c(40, 60)
  ) |>
    whep::calculate_crop_npp() |>
    whep::calculate_npp_carbon_nitrogen()

  # The additivity property test in test_properties.R holds here exactly.
  testthat::expect_equal(
    out$total_npp_c_t,
    out$crop_npp_c_t + out$weed_npp_c_t
  )
  testthat::expect_true(all(out$weed_npp_c_t == 0))
  # And the stamp is what tells the two states apart.
  testthat::expect_true(all(out$method_weed_npp == "absent_zero"))
})

test_that("a supplied weed stream is stamped per row", {
  out <- tibble::tibble(
    item_prod_code = c("15", "56"),
    production_t = c(100, 200),
    area_ha = c(40, 60),
    weed_ag_dm_t = c(5, NA_real_)
  ) |>
    whep::calculate_crop_npp() |>
    whep::calculate_npp_carbon_nitrogen()

  testthat::expect_equal(out$method_weed_npp, c("supplied", "absent_zero"))
  testthat::expect_gt(out$weed_npp_c_t[[1]], 0)
  testthat::expect_equal(out$weed_npp_c_t[[2]], 0)
})

test_that("stamping the weed stream moves no number", {
  out <- tibble::tibble(
    item_prod_code = c("15", "56"),
    production_t = c(100, 200),
    area_ha = c(40, 60)
  ) |>
    whep::calculate_crop_npp() |>
    whep::calculate_npp_carbon_nitrogen()

  testthat::expect_equal(out$weed_ag_dm_t, c(0, 0))
  testthat::expect_equal(out$weed_npp_n_t, c(0, 0))
  testthat::expect_equal(out$total_npp_c_t, out$crop_npp_c_t)
})

test_that("residues stamp items with no IPCC crop group as uncorrected", {
  mapping <- whep::whep_coef_table("ipcc_crop_mapping")
  unmapped <- setdiff(
    whep::whep_coef_table("bio_coefs")$item_prod_code,
    mapping$item_prod_code[!is.na(mapping$crop_group)]
  )
  testthat::skip_if(length(unmapped) == 0L)
  x <- tibble::tibble(
    item_prod_code = c("15", unmapped[[1]]),
    production_t = 100,
    area_ha = 40
  )
  out <- whep::calculate_crop_residues(x)
  testthat::expect_equal(
    out$method_residue_group,
    c("ipcc_group", "unmapped_uncorrected")
  )
  # stamping moves no number
  testthat::expect_equal(
    out$residue_dm_t[[1]],
    whep::calculate_crop_residues(x[1, ])$residue_dm_t
  )
})

# Modern-variety adoption is tabulated by decade (1900, 1920, 1940, 1950, ...,
# 2020). Joined on the exact year, every year that does not end in a zero found
# no row, and `replace_na(modern_share, 1)` switched the harvest-index
# correction off: a sawtooth, residue high on decade years only (whep#1034).
.variety_wheat <- function(years, region = "Sub-saharan Africa") {
  tibble::tibble(
    item_prod_code = "15",
    production_t = 100,
    area_ha = 40,
    year = years,
    region_hanpp = region
  )
}

.variety_share <- function(region, crop, year) {
  whep::whep_coef_table("modern_variety_adoption") |>
    dplyr::filter(
      region_hanpp == region,
      crop_group == crop,
      year == !!year
    ) |>
    dplyr::pull(modern_share)
}

test_that("modern-variety adoption reaches the years between decades", {
  out <- whep::calculate_crop_residues(.variety_wheat(1970:1980))
  residue <- out$residue_dm_t
  bare <- whep::calculate_crop_residues(
    dplyr::select(.variety_wheat(1975), -year, -region_hanpp)
  )$residue_dm_t
  share_1975 <- (.variety_share("Sub-saharan Africa", "Wheat", 1970) +
    .variety_share("Sub-saharan Africa", "Wheat", 1980)) /
    2
  gap <- whep::whep_coef_table("hi_crop_ranges") |>
    dplyr::filter(crop_group == "Wheat") |>
    dplyr::pull(hi_gap_factor)

  testthat::expect_true(all(diff(residue) < 0))
  testthat::expect_equal(
    residue[out$year == 1975],
    bare * (1 + (1 - share_1975) * (gap - 1))
  )
})

test_that("modern-variety adoption holds its end values beyond the table", {
  out <- whep::calculate_crop_residues(.variety_wheat(c(
    1850,
    1900,
    2020,
    2023
  )))
  residue <- rlang::set_names(out$residue_dm_t, out$year)

  testthat::expect_equal(residue[["1850"]], residue[["1900"]])
  testthat::expect_equal(residue[["2023"]], residue[["2020"]])
})

test_that("a moved region vocabulary is refused, not read as fully modern", {
  moved <- .variety_wheat(2010, region = "Africa (Sub-Saharan)")
  out <- testthat::with_mocked_bindings(
    whep::calculate_crop_residues(moved),
    check_labels_supplied = function(data, ...) invisible(data)
  )
  bare <- whep::calculate_crop_residues(
    dplyr::select(moved, -year, -region_hanpp)
  )

  expect_supplied_guard(
    identity = isTRUE(all.equal(out$residue_dm_t, bare$residue_dm_t)),
    guard = whep::calculate_crop_residues(moved),
    class = "whep_absent_label"
  )
})

test_that("a polity with no HANPP region keeps the uncorrected residue", {
  out <- whep::calculate_crop_residues(.variety_wheat(2010, NA_character_))
  bare <- whep::calculate_crop_residues(
    dplyr::select(.variety_wheat(2010), -year, -region_hanpp)
  )

  testthat::expect_equal(out$residue_dm_t, bare$residue_dm_t)
})

# afsetools' IPCC_crop_mapping lists cotton, bananas, coconuts, coffee, cocoa,
# green maize and the other forages, but under spellings ("Coffee green",
# "Maize green") the biomass vocabulary does not use, so the port inherited
# none of them and 12.8% of 2010 harvested area got no variety correction
# (whep#1437).
test_that("the largest crops by area carry an IPCC crop group", {
  big <- c("328", "645", "636", "248", "656", "661", "639", "486")
  mapping <- whep::whep_coef_table("ipcc_crop_mapping")

  testthat::expect_setequal(
    mapping$item_prod_code[!is.na(mapping$crop_group)] |> intersect(big),
    big
  )
  testthat::expect_equal(
    mapping$crop_group[match(big, mapping$item_prod_code)],
    rep("Other", length(big))
  )
})

test_that("coffee residue gets the modern-variety correction", {
  x <- tibble::tibble(
    item_prod_code = "656",
    production_t = 100,
    area_ha = 40,
    year = 1970,
    region_hanpp = "Sub-saharan Africa"
  )
  out <- whep::calculate_crop_residues(x)
  bare <- whep::calculate_crop_residues(dplyr::select(x, -year, -region_hanpp))
  share <- .variety_share("Sub-saharan Africa", "Other", 1970)
  gap <- whep::whep_coef_table("hi_crop_ranges") |>
    dplyr::filter(crop_group == "Other") |>
    dplyr::pull(hi_gap_factor)

  testthat::expect_lt(share, 1)
  testthat::expect_equal(out$method_residue_group, "ipcc_group")
  testthat::expect_equal(
    out$residue_dm_t,
    bare$residue_dm_t * (1 + (1 - share) * (gap - 1))
  )
})
