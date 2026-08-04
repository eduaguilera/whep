# .calculate_finn --------------------------------------------------------------

test_that(".calculate_finn returns 0 when nothing cycles", {
  comps <- c("A", "B")
  flow_matrix <- matrix(0, 2, 2, dimnames = list(comps, comps))
  z <- stats::setNames(c(10, 5), comps)

  expect_equal(.calculate_finn(flow_matrix, z), 0)
})

test_that(".calculate_finn matches the analytic Finn index for a two-node cycle", {
  # A receives 10 from outside and 10 back from B; A sends 20 to B; B exports
  # the other 10. Every compartment balances.
  #
  # T_A = 10 + 10 = 20, T_B = 0 + 20 = 20.
  # G[i, j] = f_ij / T_j, so G = [[0, 1], [10/20, 0]].
  # diag((I - G)^-1) = 1 / (1 - 10/20) = 2 for both compartments, hence
  # FCI = sum(T * (1 - 1/2)) / sum(T) = 0.5.
  comps <- c("A", "B")
  # Rows are the source, columns the destination: A -> B is 20, B -> A is 10.
  flow_matrix <- matrix(
    c(
      0,
      20,
      10,
      0
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(comps, comps)
  )
  z <- stats::setNames(c(10, 0), comps)

  expect_equal(.calculate_finn(flow_matrix, z), 0.5)
})

test_that(".calculate_finn matches the analytic index for a single self-loop", {
  # One compartment with external input 10 and a self-loop of s: the index is
  # s / (10 + s), so s = 30 gives 0.75.
  flow_matrix <- matrix(30, 1, 1, dimnames = list("A", "A"))
  z <- stats::setNames(10, "A")

  expect_equal(.calculate_finn(flow_matrix, z), 30 / 40)
})

test_that(".calculate_finn returns NA when a compartment has no throughflow", {
  comps <- c("A", "B")
  flow_matrix <- matrix(0, 2, 2, dimnames = list(comps, comps))
  z <- stats::setNames(c(10, 0), comps)

  expect_true(is.na(.calculate_finn(flow_matrix, z)))
})

test_that(".calculate_finn grows with the amount recycled", {
  comps <- c("A", "B")
  # A -> B fixed at 20; B -> A is the recycled flow. Analytically the index is
  # recycled / (20 + recycled).
  build <- function(recycled) {
    matrix(
      c(0, 20, recycled, 0),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(comps, comps)
    )
  }
  z <- stats::setNames(c(20, 0), comps)

  low <- .calculate_finn(build(2), z)
  high <- .calculate_finn(build(15), z)

  expect_equal(low, 2 / 22)
  expect_equal(high, 15 / 35)
  expect_lt(low, high)
})


# .finn_flow_matrix ------------------------------------------------------------

test_that(".finn_flow_matrix places flows at [from, to] and aggregates", {
  comps <- c("Cropland", "Livestock")
  internal <- tibble::tribble(
    ~from_comp, ~to_comp, ~mg_n,
    "Cropland", "Livestock", 30,
    "Cropland", "Livestock", 20,
    "Livestock", "Cropland", 5
  )

  out <- .finn_flow_matrix(internal, comps)

  expect_equal(out["Cropland", "Livestock"], 50)
  expect_equal(out["Livestock", "Cropland"], 5)
  expect_equal(out["Cropland", "Cropland"], 0)
  expect_equal(dim(out), c(2L, 2L))
})

test_that(".finn_flow_matrix returns a zero matrix for no internal flows", {
  comps <- c("Cropland", "Livestock")
  empty <- tibble::tibble(
    from_comp = character(),
    to_comp = character(),
    mg_n = numeric()
  )

  out <- .finn_flow_matrix(empty, comps)

  expect_true(all(out == 0))
  expect_equal(dimnames(out), list(comps, comps))
})


# .finn_input_vector -----------------------------------------------------------

test_that(".finn_input_vector sums external inputs per compartment", {
  comps <- c("Cropland", "Livestock")
  ext <- tibble::tribble(
    ~to_comp, ~mg_n,
    "Cropland", 100,
    "Cropland", 40,
    "Livestock", 7
  )

  out <- .finn_input_vector(ext, comps)

  expect_equal(out[["Cropland"]], 140)
  expect_equal(out[["Livestock"]], 7)
})

test_that(".finn_input_vector returns zeros when there are no external inputs", {
  comps <- c("Cropland", "Livestock")
  empty <- tibble::tibble(to_comp = character(), mg_n = numeric())

  expect_equal(unname(.finn_input_vector(empty, comps)), c(0, 0))
})


# .finn_mapping ----------------------------------------------------------------

test_that(".finn_mapping keeps compartments and external inputs disjoint", {
  mapping <- .finn_mapping()

  expect_length(intersect(mapping$compartments, mapping$external_inputs), 0)
  # Every mapped destiny must resolve to a declared compartment, or the flow
  # would be silently dropped from the network.
  expect_true(all(mapping$destiny_to_comp$to_comp %in% mapping$compartments))
})


# .finn_for_group --------------------------------------------------------------

test_that(".finn_for_group builds the network and returns one row per group", {
  flows <- tibble::tribble(
    ~origin, ~destiny, ~mg_n,
    "Synthetic", "Cropland", 100,
    "Cropland", "livestock_rum", 40,
    "Livestock", "Cropland", 10,
    "Cropland", "population_food", 20,
    "Deposition", "semi_natural_agroecosystems", 5,
    "semi_natural_agroecosystems", "livestock_rum", 5,
    "People", "Cropland", 2
  )

  out <- .finn_for_group(
    flows,
    tibble::tibble(year = 2000, province_name = "A"),
    .finn_mapping()
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$year, 2000)
  expect_equal(out$province_name, "A")
  expect_gt(out$finn_index, 0)
  expect_lt(out$finn_index, 1)
})

test_that(".finn_for_group measures throughflow as inflow, not outflow", {
  # Cropland takes 100 from outside and 10 back from Livestock, sends 40 to
  # Livestock and 20 to People. Its inflow is 110 but its within-network
  # outflow is only 60, because food and exports leave the four compartments.
  # Building the flow matrix transposed makes colSums() read 60 instead of 110
  # and inflates the index, so pin the analytic inflow-based value here.
  #
  # T_Cropland = 100 + 10 = 110, T_Livestock = 40, T_People = 20.
  # G[i, j] = f_ij / T_j gives a single cycle Cropland -> Livestock -> Cropland
  # with product (40/40) * (10/110) = 1/11, so diag((I - G)^-1) is 11/10 for
  # both cycling compartments and 1 for People.
  # Semi-natural takes 5 from deposition and neither feeds nor is fed by the
  # others, so it only adds to total throughput.
  # FCI = (110 + 40) * (1 - 10/11) / (110 + 5 + 40 + 20).
  flows <- tibble::tribble(
    ~origin, ~destiny, ~mg_n,
    "Synthetic", "Cropland", 100,
    "Cropland", "livestock_rum", 40,
    "Livestock", "Cropland", 10,
    "Cropland", "population_food", 20,
    "Deposition", "semi_natural_agroecosystems", 5
  )

  out <- .finn_for_group(
    flows,
    tibble::tibble(year = 2000, province_name = "A"),
    .finn_mapping()
  )

  expect_equal(out$finn_index, (110 + 40) * (1 - 10 / 11) / 175)
})

test_that(".finn_for_group returns NA if any compartment has no inflow", {
  # All four compartments must receive something, or the whole province-year
  # drops out of the series. Here nothing reaches semi-natural.
  flows <- tibble::tribble(
    ~origin, ~destiny, ~mg_n,
    "Synthetic", "Cropland", 100,
    "Cropland", "livestock_rum", 40,
    "Livestock", "Cropland", 10,
    "Cropland", "population_food", 20
  )

  out <- .finn_for_group(
    flows,
    tibble::tibble(year = 2000, province_name = "A"),
    .finn_mapping()
  )

  expect_true(is.na(out$finn_index))
})

test_that(".finn_for_group ignores flows whose origin is not a compartment", {
  # Only external inputs, no compartment-to-compartment flow: nothing cycles.
  flows <- tibble::tribble(
    ~origin, ~destiny, ~mg_n,
    "Synthetic", "Cropland", 100,
    "Deposition", "semi_natural_agroecosystems", 50,
    "Outside", "livestock_mono", 25,
    "Fixation", "population_food", 10
  )

  out <- .finn_for_group(
    flows,
    tibble::tibble(year = 1900, province_name = "B"),
    .finn_mapping()
  )

  expect_equal(out$finn_index, 0)
})
