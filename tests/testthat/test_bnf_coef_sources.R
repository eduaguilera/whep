# Cell-level provenance gates for `inst/extdata/coefs/bnf.csv`.
#
# The tracked long sidecar is the durable contract. Every non-missing numeric
# cell in `bnf.csv` must occur exactly once and remain in one of three honest
# evidence classes. The independent source transcriptions below prevent the
# sidecar from merely agreeing with a second copy of the coefficient table.

# ---- Anglade et al. (2015), Ecosphere 6(3):37 -------------------------------

.anglade_2015_table1 <- function() {
  tibble::tibble(
    species = c("Alfalfa", "Clover", "Faba bean", "Lentil", "Pea", "All"),
    shoot_n_pct_n = c(101, 327, 40, 48, 55, 571),
    shoot_n_pct_median = c(2.9, 3.8, 2.5, 2.2, 2.7, 3.2),
    shoot_n_pct_q1 = c(2.6, 3.1, 2.3, 1.9, 2.5, 2.6),
    shoot_n_pct_q3 = c(3.5, 4.5, 3.2, 2.4, 3, 4),
    ndfa_pct_n = c(120, 409, 91, 92, 239, 951),
    ndfa_pct_median = c(70, 86, 75, 66, 71, 78),
    ndfa_pct_q1 = c(60, 74, 68, 61, 59, 65),
    ndfa_pct_q3 = c(83, 93, 85, 74, 82, 89),
    shoot_n_kg_ha_n = c(101, 327, 82, 74, 188, 772),
    shoot_n_kg_ha_median = c(119, 112, 174, 96, 132, 120),
    shoot_n_kg_ha_q1 = c(77, 44, 113, 79, 87, 67),
    shoot_n_kg_ha_q3 = c(188, 216, 230, 130, 190, 200),
    shoot_dm_t_ha_n = c(123, 428, 42, 47, 86, 726),
    shoot_dm_t_ha_median = c(3.3, 3, 7.3, 4.5, 2.35, 3.4),
    shoot_dm_t_ha_q1 = c(2, 1.5, 4.8, 4.1, 1, 1.6),
    shoot_dm_t_ha_q3 = c(4.6, 5.6, 9, 6.6, 5.8, 6),
    shoot_n_fixed_n = c(142, 495, 82, 74, 216, 1009),
    shoot_n_fixed_median = c(70, 77, 139, 71.5, 82, 80),
    shoot_n_fixed_q1 = c(41, 38, 88, 37, 42, 42),
    shoot_n_fixed_q3 = c(121, 158, 167, 85, 121, 139),
    nhi_pct_n = c(NA, NA, 34, 55, 100, 189),
    nhi_pct_median = c(NA, NA, 74, 75, 75, 75),
    nhi_pct_q1 = c(NA, NA, 63, 65, 68, 65),
    nhi_pct_q3 = c(NA, NA, 77, 78, 82, 80)
  )
}

.anglade_2015_text <- list(
  bgn_grain_legume = 1.3,
  bgn_forage_legume = 1.7,
  nhi_alfalfa = 0.9
)

.anglade_frac <- function(species, column) {
  table1 <- .anglade_2015_table1()
  table1[[column]][table1$species == species] / 100
}

.anglade_values <- function() {
  forage <- .anglade_2015_text$bgn_forage_legume
  grain <- .anglade_2015_text$bgn_grain_legume
  tibble::tribble(
    ~name_bnf, ~coefficient, ~expected, ~source_id,
    "Alfalfa", "ndfa", .anglade_frac("Alfalfa", "ndfa_pct_median"),
    "anglade_2015",
    "Alfalfa", "n_harvest_index", .anglade_2015_text$nhi_alfalfa,
    "anglade_2015",
    "Alfalfa", "below_ground_n_ratio", forage, "anglade_2015",
    "Clover", "ndfa", .anglade_frac("Clover", "ndfa_pct_median"),
    "anglade_2015",
    "Clover", "below_ground_n_ratio", forage, "anglade_2015",
    "Fava bean", "ndfa", .anglade_frac("Faba bean", "ndfa_pct_median"),
    "anglade_2015",
    "Fava bean", "n_harvest_index",
    .anglade_frac("Faba bean", "nhi_pct_median"), "anglade_2015",
    "Fava bean", "below_ground_n_ratio", grain, "anglade_2015",
    "Lentils", "ndfa", .anglade_frac("Lentil", "ndfa_pct_median"),
    "anglade_2015",
    "Lentils", "n_harvest_index",
    .anglade_frac("Lentil", "nhi_pct_median"), "anglade_2015",
    "Lentils", "below_ground_n_ratio", grain, "anglade_2015",
    "Pea", "ndfa", .anglade_frac("Pea", "ndfa_pct_median"),
    "anglade_2015",
    "Pea", "n_harvest_index", .anglade_frac("Pea", "nhi_pct_median"),
    "anglade_2015",
    "Pea", "below_ground_n_ratio", grain, "anglade_2015"
  )
}

# ---- Herridge et al. (2008), Plant and Soil 311:1-18 ------------------------

.herridge_values <- function() {
  tibble::tribble(
    ~name_bnf, ~coefficient, ~expected, ~source_id,
    "Beans", "ndfa", 0.40, "herridge_2008",
    "Groundnuts, with shell", "ndfa", 0.68, "herridge_2008",
    "Rice", "nonsymbiotic_base_kg_ha", 33, "herridge_2008",
    "Sugarcane", "nonsymbiotic_base_kg_ha", 25, "herridge_2008"
  )
}

# ---- Lassaletta et al. (2014), ERL Supplementary Methods Table S1-2 --------

.lassaletta_s1_values <- function() {
  tibble::tribble(
    ~name_bnf, ~coefficient, ~expected, ~source_id,
    "Beans", "n_harvest_index", 0.75, "lassaletta_2014_erl_s1",
    "Beans", "below_ground_n_ratio", 1.3, "lassaletta_2014_erl_s1",
    "Clover", "n_harvest_index", 0.9, "lassaletta_2014_erl_s1",
    "Green leguminous", "ndfa", 0.68, "lassaletta_2014_erl_s1",
    "Green leguminous", "n_harvest_index", 0.5,
    "lassaletta_2014_erl_s1",
    "Green leguminous", "below_ground_n_ratio", 1.3,
    "lassaletta_2014_erl_s1",
    "Groundnuts, with shell", "n_harvest_index", 0.5,
    "lassaletta_2014_erl_s1",
    "Groundnuts, with shell", "below_ground_n_ratio", 1.3,
    "lassaletta_2014_erl_s1",
    "Other pulses", "ndfa", 0.68, "lassaletta_2014_erl_s1",
    "Other pulses", "n_harvest_index", 0.75, "lassaletta_2014_erl_s1",
    "Other pulses", "below_ground_n_ratio", 1.3,
    "lassaletta_2014_erl_s1",
    "Soyabeans", "ndfa", 0.57, "lassaletta_2014_erl_s1",
    "Soyabeans", "n_harvest_index", 0.73, "lassaletta_2014_erl_s1",
    "Soyabeans", "below_ground_n_ratio", 1.4,
    "lassaletta_2014_erl_s1"
  )
}

.bnf_numeric_columns <- function() {
  c(
    "ndfa",
    "n_harvest_index",
    "below_ground_n_ratio",
    "nonsymbiotic_base_kg_ha",
    "leguminous_share"
  )
}

.bnf_stored <- function(bnf, name, coefficient) {
  value <- bnf[[coefficient]][bnf$name_bnf == name]
  testthat::expect_length(value, 1)
  value
}

.bnf_cell_key <- function(x) {
  paste(x$name_bnf, x$coefficient, sep = " / ")
}

.bnf_stored_cells <- function(bnf) {
  bnf |>
    dplyr::select(name_bnf, dplyr::all_of(.bnf_numeric_columns())) |>
    tidyr::pivot_longer(
      cols = -name_bnf,
      names_to = "coefficient",
      values_to = "stored_value",
      values_drop_na = TRUE
    )
}

.bnf_is_blank <- function(x) {
  is.na(x) | stringr::str_trim(x) == ""
}

.bnf_source_values <- function() {
  dplyr::bind_rows(
    .anglade_values(),
    .herridge_values(),
    .lassaletta_s1_values()
  )
}

# ---- Tests ------------------------------------------------------------------

test_that("the transcribed Anglade Table 1 reproduces its own totals", {
  table1 <- .anglade_2015_table1()
  species <- table1[table1$species != "All", ]
  all_row <- table1[table1$species == "All", ]
  n_columns <- grep("_n$", names(table1), value = TRUE)

  testthat::expect_length(n_columns, 6)
  for (column in n_columns) {
    testthat::expect_equal(
      sum(species[[column]], na.rm = TRUE),
      all_row[[column]],
      info = column
    )
  }
})

test_that("every transcribed Anglade median lies inside its own IQR", {
  table1 <- .anglade_2015_table1()
  median_columns <- grep("_median$", names(table1), value = TRUE)

  testthat::expect_length(median_columns, 6)
  for (column in median_columns) {
    stem <- sub("_median$", "", column)
    median <- table1[[column]]
    q1 <- table1[[paste0(stem, "_q1")]]
    q3 <- table1[[paste0(stem, "_q3")]]
    keep <- !is.na(median)
    testthat::expect_true(all(q1[keep] <= median[keep]))
    testthat::expect_true(all(median[keep] <= q3[keep]))
    testthat::expect_true(all(q1[keep] < q3[keep]))
  }
})

test_that("the provenance sidecar covers every BNF coefficient once", {
  bnf <- whep::whep_coef_table("bnf")
  provenance <- whep::whep_coef_table("bnf_provenance")
  cells <- .bnf_stored_cells(bnf)

  testthat::expect_equal(nrow(cells), 60)
  testthat::expect_equal(nrow(provenance), 60)
  testthat::expect_equal(anyDuplicated(.bnf_cell_key(provenance)), 0)
  testthat::expect_setequal(.bnf_cell_key(provenance), .bnf_cell_key(cells))

  compared <- dplyr::inner_join(
    cells,
    provenance |>
      dplyr::select(name_bnf, coefficient, provenance_value = stored_value),
    by = c("name_bnf", "coefficient")
  )
  testthat::expect_equal(nrow(compared), 60)
  testthat::expect_equal(compared$provenance_value, compared$stored_value)
})

test_that("the evidence vocabulary and 32-15-13 partition are fixed", {
  provenance <- whep::whep_coef_table("bnf_provenance")
  vocabulary <- c(
    "source_asserted",
    "derivation_asserted",
    "genuinely_unresolved"
  )
  counts <- table(factor(provenance$evidence_class, levels = vocabulary))

  testthat::expect_setequal(unique(provenance$evidence_class), vocabulary)
  testthat::expect_identical(as.integer(counts), c(32L, 15L, 13L))
})

test_that("source assertions are complete and reproduce reported values", {
  provenance <- whep::whep_coef_table("bnf_provenance")
  source_rows <- provenance |>
    dplyr::filter(evidence_class == "source_asserted")
  required <- c(
    "source_id",
    "source_title",
    "source_year",
    "source_venue",
    "source_doi",
    "publication_locator",
    "reported_value",
    "reported_unit",
    "transformation",
    "verification_result",
    "verification_rationale"
  )

  testthat::expect_false(any(vapply(
    source_rows[required],
    function(x) any(.bnf_is_blank(x)),
    logical(1)
  )))
  transformed <- dplyr::case_when(
    source_rows$transformation == "reported percent / 100" ~
      source_rows$reported_value / 100,
    source_rows$transformation == "none" ~ source_rows$reported_value,
    .default = NA_real_
  )
  testthat::expect_false(any(is.na(transformed)))
  testthat::expect_equal(transformed, source_rows$stored_value)
  testthat::expect_true(all(source_rows$verification_result == "matched"))
})

test_that("all 32 source assertions match independent transcriptions", {
  provenance <- whep::whep_coef_table("bnf_provenance") |>
    dplyr::filter(evidence_class == "source_asserted")
  expected <- .bnf_source_values()

  testthat::expect_equal(nrow(expected), 32)
  testthat::expect_setequal(.bnf_cell_key(provenance), .bnf_cell_key(expected))
  compared <- dplyr::inner_join(
    expected,
    provenance |>
      dplyr::select(
        name_bnf,
        coefficient,
        stored_value,
        provenance_source = source_id
      ),
    by = c("name_bnf", "coefficient")
  )
  testthat::expect_equal(compared$stored_value, compared$expected)
  testthat::expect_identical(compared$provenance_source, compared$source_id)
})

test_that("derivations are complete and recompute their stored values", {
  bnf <- whep::whep_coef_table("bnf")
  derived <- whep::whep_coef_table("bnf_provenance") |>
    dplyr::filter(evidence_class == "derivation_asserted")
  required <- c(
    "derivation_parent",
    "derivation_formula",
    "verification_result",
    "verification_rationale"
  )

  testthat::expect_false(any(vapply(
    derived[required],
    function(x) any(.bnf_is_blank(x)),
    logical(1)
  )))
  testthat::expect_true(
    all(derived$verification_result == "exact_derivation_match")
  )

  copied <- derived |>
    dplyr::filter(source_id == "whep_bnf_row_copy")
  parent_value <- purrr::map2_dbl(
    copied$derivation_parent,
    copied$coefficient,
    function(parent, coefficient) .bnf_stored(bnf, parent, coefficient)
  )
  testthat::expect_equal(copied$stored_value, parent_value)
  testthat::expect_true(all(
    copied$derivation_formula == "stored_value = parent stored_value"
  ))

  defined <- derived |>
    dplyr::filter(source_id == "whep_stand_definition")
  pure_legumes <- c(
    "Alfalfa",
    "Beans",
    "Clover",
    "Fava bean",
    "Green leguminous",
    "Groundnuts, with shell",
    "Lentils",
    "Other pulses",
    "Pea",
    "Soyabeans"
  )
  non_legumes <- c("Rice", "Sugarcane")
  expected <- ifelse(defined$name_bnf %in% pure_legumes, 1, 0)
  testthat::expect_setequal(defined$name_bnf, c(pure_legumes, non_legumes))
  testthat::expect_equal(defined$stored_value, expected)
  testthat::expect_setequal(
    unique(defined$derivation_formula),
    c(
      "pure legume crop => leguminous_share = 1",
      "non-legume system => leguminous_share = 0"
    )
  )
})

test_that("unresolved cells contain no guessed source assertion", {
  unresolved <- whep::whep_coef_table("bnf_provenance") |>
    dplyr::filter(evidence_class == "genuinely_unresolved")
  required <- c(
    "source_id",
    "source_identity_status",
    "publication_locator",
    "verification_result",
    "verification_rationale",
    "authority_caveat",
    "next_resolution_condition"
  )

  testthat::expect_equal(nrow(unresolved), 13)
  testthat::expect_false(any(vapply(
    unresolved[required],
    function(x) any(.bnf_is_blank(x)),
    logical(1)
  )))
  testthat::expect_true(all(is.na(unresolved$reported_value)))
  testthat::expect_true(all(.bnf_is_blank(unresolved$reported_unit)))
  testthat::expect_true(all(.bnf_is_blank(unresolved$derivation_parent)))
  testthat::expect_true(all(.bnf_is_blank(unresolved$derivation_formula)))
  testthat::expect_true(all(unresolved$transformation == "none"))
  testthat::expect_true(
    all(unresolved$verification_result == "unresolved_without_guessing")
  )
})

test_that("nitrogen harvest index is never dry-matter harvest index", {
  provenance <- whep::whep_coef_table("bnf_provenance")
  nhi <- provenance |>
    dplyr::filter(coefficient == "n_harvest_index")

  testthat::expect_true(all(
    nhi$denominator == "shoot N; fraction in harvested grain or product"
  ))
  testthat::expect_false(any(
    nhi$source_id == "herridge_2008" &
      nhi$evidence_class == "source_asserted"
  ))
  faba <- nhi |>
    dplyr::filter(name_bnf == "Fava bean")
  testthat::expect_equal(faba$stored_value, 0.74)
  testthat::expect_equal(faba$reported_value, 74)
  testthat::expect_false(faba$reported_value == 34)
})

test_that("Lassaletta authority is the corrected ERL supplement", {
  provenance <- whep::whep_coef_table("bnf_provenance")
  lassaletta <- provenance |>
    dplyr::filter(source_id == "lassaletta_2014_erl_s1")
  expected_title <- paste(
    "50 year trends in nitrogen use efficiency of world cropping systems:",
    "the relationship between yield and nitrogen input to cropland"
  )

  testthat::expect_equal(nrow(lassaletta), 14)
  testthat::expect_true(all(lassaletta$source_title == expected_title))
  testthat::expect_true(all(
    lassaletta$source_venue == "Environmental Research Letters 9:105011"
  ))
  testthat::expect_true(all(
    lassaletta$source_doi == "10.1088/1748-9326/9/10/105011"
  ))
  testthat::expect_true(all(stringr::str_detect(
    lassaletta$publication_locator,
    "Supplementary Methods, PDF p\\. 5, Table S1-2"
  )))
  testthat::expect_true(all(stringr::str_detect(
    lassaletta$source_context,
    "Table S1-2 crop vectors; Table S1-3 adjacent non-symbiotic BNF"
  )))
  stale_authority <- lassaletta |>
    dplyr::select(
      source_title,
      source_venue,
      source_doi,
      source_context,
      publication_locator
    ) |>
    unlist(use.names = FALSE) |>
    paste(collapse = "\n")
  testthat::expect_false(stringr::str_detect(
    stale_authority,
    "Biogeosciences|Spain"
  ))
})

test_that("provenance contains no local or ignored evidence paths", {
  provenance <- whep::whep_coef_table("bnf_provenance")
  text <- provenance |>
    dplyr::select(where(is.character)) |>
    unlist(use.names = FALSE) |>
    paste(collapse = "\n")
  forbidden <- paste(
    "validation/cache",
    "render/",
    "ARTICULOS_DIR",
    "[A-Za-z]:[/\\\\]",
    "/home/",
    "/Users/",
    "Biogeosciences",
    "Biogeochem",
    sep = "|"
  )

  testthat::expect_false(stringr::str_detect(text, forbidden))
})

test_that("bnf.csv retains its T20 content identity", {
  path <- system.file("extdata", "coefs", "bnf.csv", package = "whep")
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  has_crlf <- any(raw == as.raw(13))
  expected <- if (has_crlf) {
    "8DBB11204003D72EECDC29EB72E483D7"
  } else {
    "1D715A15F42EF9271820240256EB646D"
  }
  expected_size <- if (has_crlf) 1267 else 1249

  testthat::expect_identical(file.info(path)$size, expected_size)
  testthat::expect_identical(
    toupper(unname(tools::md5sum(path))),
    expected
  )
})

test_that("grain-legume NHI agrees with the composition route", {
  bnf <- whep::whep_coef_table("bnf")
  bio <- whep::whep_coef_table("bio_coefs")
  names_bnf <- whep::whep_coef_table("names_bnf")
  grain_rows <- c(
    "Beans",
    "Fava bean",
    "Lentils",
    "Pea",
    "Other pulses",
    "Groundnuts, with shell",
    "Soyabeans"
  )
  mapped <- merge(
    names_bnf[, c("item_prod_code", "name_bnf")],
    bio[, c(
      "item_prod_code",
      "product_dm_kgfm",
      "residue_dm_kgfm",
      "residue_kg_product_fm_kg",
      "product_n_kgdm",
      "residue_n_kgdm"
    )],
    by = "item_prod_code"
  )
  mapped <- mapped[mapped$name_bnf %in% grain_rows, ]
  grain_n <- mapped$product_n_kgdm * mapped$product_dm_kgfm
  residue_n <- mapped$residue_n_kgdm *
    mapped$residue_dm_kgfm *
    mapped$residue_kg_product_fm_kg
  implied <- grain_n / (grain_n + residue_n)
  stored <- vapply(
    mapped$name_bnf,
    function(name) .bnf_stored(bnf, name, "n_harvest_index"),
    numeric(1)
  )

  testthat::expect_identical(nrow(mapped), 14L)
  testthat::expect_identical(length(unique(mapped$name_bnf)), 7L)
  testthat::expect_true(all(implied / stored > 0.7 & implied / stored < 1.4))
})
