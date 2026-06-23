# Live numerical-equivalence regression against the afsetools originals.
#
# Skips unless a full afsetools is installed with the source BNF/NPP functions
# AND its load_general_data() global data: that is the case on a developer
# machine, but not on CI (where afsetools lacks these), so this test runs
# locally and skips cleanly on CI. The afsetools functions are not exported in
# its NAMESPACE, so they are reached via getFromNamespace().

.afse <- function(n) utils::getFromNamespace(n, "afsetools")

.afse_ready <- function() {
  if (!requireNamespace("afsetools", quietly = TRUE)) {
    return(FALSE)
  }
  fns <- c(
    "calculate_crop_residues",
    "calculate_crop_roots",
    "calc_crop_bnf",
    "calc_nonsymbiotic_bnf",
    "load_general_data"
  )
  all(vapply(
    fns,
    function(f) exists(f, where = asNamespace("afsetools")),
    logical(1)
  ))
}

test_that("whep crop-NPP / BNF ports match the afsetools originals", {
  testthat::skip_if_not(
    .afse_ready(),
    "afsetools with source functions not installed"
  )
  # afsetools functions resolve their data objects (Biomass_coefs, ...) via the
  # namespace -> base -> globalenv chain, so load_general_data() must populate
  # the global environment, not the test frame.
  loaded <- tryCatch(
    {
      eval(
        quote(utils::getFromNamespace("load_general_data", "afsetools")()),
        envir = globalenv()
      )
      TRUE
    },
    error = function(e) FALSE
  )
  testthat::skip_if_not(loaded, "afsetools::load_general_data() unavailable")

  af_r <- .afse("calculate_crop_residues")(
    data.frame(Name_biomass = "Wheat", Prod_ygpit_Mg = 100, Area_ygpit_ha = 40)
  )
  wh_r <- whep::calculate_crop_residues(
    tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  )
  testthat::expect_equal(wh_r$residue_dm_t, af_r$Residue_MgDM, tolerance = 1e-6)
  testthat::expect_equal(wh_r$product_dm_t, af_r$Prod_MgDM, tolerance = 1e-6)

  pr <- wh_r$product_dm_t
  re <- wh_r$residue_dm_t
  af_k <- .afse("calculate_crop_roots")(
    data.frame(
      Name_biomass = "Wheat",
      Prod_MgDM = pr,
      Residue_MgDM = re,
      Area_ygpit_ha = 40
    )
  )
  wh_k <- whep::calculate_crop_roots(
    tibble::tibble(
      item_prod_code = "15",
      product_dm_t = pr,
      residue_dm_t = re,
      area_ha = 40
    )
  )
  testthat::expect_equal(wh_k$root_dm_t, af_k$Root_MgDM, tolerance = 1e-6)

  af_b <- .afse("calc_crop_bnf")(
    data.frame(Name_biomass = "Beans", Crop_NPP_MgN = 10, Prod_MgN = 5)
  )
  wh_b <- whep::calculate_crop_bnf(
    tibble::tibble(item_prod_code = "176", crop_npp_n_t = 10, product_n_t = 5)
  )
  testthat::expect_equal(wh_b$crop_bnf_t, af_b$CropBNF, tolerance = 1e-6)
  testthat::expect_equal(
    wh_b$crop_bnf_anglade_t,
    af_b$CropBNF2,
    tolerance = 1e-6
  )

  af_n <- .afse("calc_nonsymbiotic_bnf")(data.frame(Area_ygpit_ha = 40))
  wh_n <- whep::calculate_nonsymbiotic_bnf(tibble::tibble(area_ha = 40))
  testthat::expect_equal(wh_n$nonsymbiotic_bnf_t, af_n$NSBNF, tolerance = 1e-6)
})
