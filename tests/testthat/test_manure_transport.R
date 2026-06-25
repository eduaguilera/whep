.toy_sources <- function(surplus = 10) {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~surplus_n,
    ~surplus_c,
    ~surplus_vs,
    2020L,
    "ESP",
    "1.5_40",
    surplus,
    surplus * 9,
    surplus * 0.6
  )
}

.toy_sinks <- function(r1 = 4, r2 = 6, terr2 = "ESP") {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~room_n,
    2020L,
    "ESP",
    "1_40",
    r1,
    2020L,
    terr2,
    "2_40",
    r2
  )
}

test_that(".ring_offsets(1) reproduces the king-move stencil", {
  ring <- whep:::.ring_offsets(1)
  king <- whep:::.king_move_offsets()
  key <- function(d) sort(paste(d$dlon, d$dlat))
  expect_equal(key(ring), key(king))
  expect_equal(nrow(ring), 8L)
})

test_that("transport conserves N, C and VS (transported + residual = surplus)", {
  res <- whep::allocate_manure_transport(.toy_sources(10), .toy_sinks())
  expect_equal(sum(res$applied_n), 10, tolerance = 1e-8)
  expect_equal(sum(res$applied_c), 90, tolerance = 1e-8)
  expect_equal(sum(res$applied_vs), 6, tolerance = 1e-8)
})

test_that("room-weighted split fills neighbours in proportion to room", {
  res <- whep::allocate_manure_transport(.toy_sources(10), .toy_sinks(4, 6))
  tr <- res[res$kind == "transported", ]
  expect_equal(tr$applied_n[tr$sub_territory == "1_40"], 4, tolerance = 1e-6)
  expect_equal(tr$applied_n[tr$sub_territory == "2_40"], 6, tolerance = 1e-6)
  expect_false("residual" %in% res$kind) # all 10 placed
})

test_that("a sink never receives more than its room; rest is residual", {
  # Total room 5 < surplus 10 -> 5 transported, 5 residual at the source cell.
  res <- whep::allocate_manure_transport(.toy_sources(10), .toy_sinks(2, 3))
  expect_equal(sum(res$applied_n), 10, tolerance = 1e-8)
  tr <- res[res$kind == "transported", ]
  expect_equal(tr$applied_n[tr$sub_territory == "1_40"], 2, tolerance = 1e-6)
  expect_equal(tr$applied_n[tr$sub_territory == "2_40"], 3, tolerance = 1e-6)
  resid <- res[res$kind == "residual", ]
  expect_equal(resid$sub_territory, "1.5_40")
  expect_equal(resid$applied_n, 5, tolerance = 1e-6)
})

test_that("transport stays within the polity (cross_polity = FALSE)", {
  # "2_40" is in FRA: it must not receive; only the ESP sink "1_40" (room 4) can.
  res <- whep::allocate_manure_transport(
    .toy_sources(10),
    .toy_sinks(4, 6, terr2 = "FRA")
  )
  tr <- res[res$kind == "transported", ]
  expect_equal(nrow(tr), 1L)
  expect_equal(tr$sub_territory, "1_40")
  expect_equal(tr$applied_n, 4, tolerance = 1e-6) # capped at the ESP sink's room
  expect_equal(sum(res$applied_n), 10, tolerance = 1e-8) # 4 + 6 residual
})

test_that("over-subscribed sink is shared and capped under contention", {
  # Two sources flank a single sink with room 8; each offers 6 -> demand 12,
  # scaled to room 8 (4 each); 2 each left as residual.
  sources <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~surplus_n,
    ~surplus_c,
    ~surplus_vs,
    2020L,
    "ESP",
    "1_40",
    6,
    54,
    3.6,
    2020L,
    "ESP",
    "2_40",
    6,
    54,
    3.6
  )
  sink <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~room_n,
    2020L,
    "ESP",
    "1.5_40",
    8
  )
  res <- whep::allocate_manure_transport(sources, sink)
  tr <- res[res$kind == "transported", ]
  expect_equal(sum(tr$applied_n), 8, tolerance = 1e-6) # sink filled to room
  expect_equal(tr$applied_n[tr$sub_territory == "1.5_40"], 8, tolerance = 1e-6)
  resid <- res[res$kind == "residual", ]
  expect_equal(sum(resid$applied_n), 4, tolerance = 1e-6) # two each unsent
  expect_equal(sum(res$applied_n), 12, tolerance = 1e-8)
})

test_that("a source with no reachable sink keeps its whole surplus as residual", {
  far <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~room_n,
    2020L,
    "ESP",
    "50_10",
    100
  )
  res <- whep::allocate_manure_transport(.toy_sources(10), far)
  expect_false("transported" %in% res$kind)
  expect_equal(res$applied_n, 10, tolerance = 1e-8)
  expect_equal(res$kind, "residual")
})

test_that("repeated rows for a cell are summed, not partially dropped", {
  # Two rows for the same source cell (4 + 6) and a roomy sink: all 10 t must
  # transport, none lost (regression: duplicate keys once leaked mass).
  src <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~surplus_n,
    ~surplus_c,
    ~surplus_vs,
    2020L,
    "ESP",
    "1.5_40",
    4,
    36,
    2.4,
    2020L,
    "ESP",
    "1.5_40",
    6,
    54,
    3.6
  )
  sink <- tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~room_n,
    2020L,
    "ESP",
    "1_40",
    100
  )
  res <- whep::allocate_manure_transport(src, sink)
  expect_equal(sum(res$applied_n), 10, tolerance = 1e-8)
  expect_equal(sum(res$applied_c), 90, tolerance = 1e-8)
  expect_equal(sum(res$applied_vs), 6, tolerance = 1e-8)
  tr <- res[res$kind == "transported", ]
  expect_equal(tr$applied_n, 10, tolerance = 1e-6)
})

test_that("allocate_manure_transport guards bad input", {
  expect_error(
    whep::allocate_manure_transport(
      .toy_sources(),
      .toy_sinks(),
      options = list(n_rings = 0)
    ),
    "n_rings"
  )
  bad_src <- dplyr::select(.toy_sources(), -"surplus_c")
  expect_error(
    whep::allocate_manure_transport(bad_src, .toy_sinks()),
    "source_cells"
  )
  bad_snk <- dplyr::select(.toy_sinks(), -"room_n")
  expect_error(
    whep::allocate_manure_transport(.toy_sources(), bad_snk),
    "sink_cells"
  )
})
