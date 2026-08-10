.registered_of_kind <- function(kind) {
  whep::polity_identity_conventions(kind = kind)
}

.registry_object <- function(name) {
  get(name, envir = asNamespace("whep"))
}

test_that("the register's vocabulary is closed and its keys are unique", {
  registry <- whep::polity_identity_conventions()

  expect_s3_class(registry, "tbl_df")
  expect_setequal(
    names(registry),
    c(
      "object",
      "kind",
      "territory_key",
      "identity",
      "status",
      "carrier",
      "resolver",
      "rationale"
    )
  )
  expect_equal(anyDuplicated(registry$object), 0L)
  expect_true(all(
    registry$kind %in% c("package_data", "input_pin", "function_output")
  ))
  expect_true(all(
    registry$identity %in%
      c("present_day_polity", "polity_period", "identity_free")
  ))
  expect_true(all(
    registry$status %in% c("carried", "resolved_by_consumer", "recommended")
  ))
  expect_false(any(is.na(registry$object)))
  expect_false(any(is.na(registry$rationale)))
  # Every rationale is one squished sentence, not the wrapped source literal.
  expect_false(any(grepl("\\s{2,}|\\n", registry$rationale)))
})

test_that("filtering by kind keeps the rows of that kind and refuses others", {
  registry <- whep::polity_identity_conventions()

  expect_true(all(.registered_of_kind("package_data")$kind == "package_data"))
  expect_equal(
    nrow(.registered_of_kind(c("input_pin", "function_output"))),
    sum(registry$kind %in% c("input_pin", "function_output"))
  )
  expect_error(
    whep::polity_identity_conventions(kind = "grid"),
    "must name a registered kind"
  )
})

# THE LOAD-BEARING GUARD. #458 allows resolving a year-less object to the
# present-day polity "as an explicit documented choice, never as an unstated
# side effect". A new year-less territory-keyed dataset that nobody declared an
# identity for is precisely that unstated side effect, so it fails here.
test_that("every year-less territory-keyed dataset is registered", {
  found <- names(whep:::.yearless_territorial_datasets())
  registered <- .registered_of_kind("package_data")$object

  expect_setequal(
    found,
    c(
      "regions_full",
      "polities_cats",
      "gleam_geographic_hierarchy",
      "mueller_synthetic_n",
      "crops_manure_n",
      "gleam_dressing_percentages",
      "gleam_fracremove",
      "gleam_mechanization_levels"
    )
  )
  expect_true(all(found %in% registered))
  # And nothing is registered that the detector cannot see, which would mean
  # the register describes a dataset that has since gained a year column.
  expect_true(all(registered %in% found))
})

test_that("the detector reads every dataset a caller can reach", {
  # `utils::data()` indexes .rda files, so on its own it misses the 45 tables
  # sharing data/livestock_coefs.rda. If the union ever silently narrowed to
  # that index, the guard above would go quiet instead of failing.
  exposed <- whep:::.exposed_dataset_names()

  expect_gt(length(exposed), 90L)
  expect_true(all(
    c("regions_full", "gleam_fracremove", "ipcc_2006_enteric_ef") %in% exposed
  ))
})

test_that("declared territory keys exist and the object really has no year", {
  registry <- .registered_of_kind("package_data")

  purrr::walk2(registry$object, registry$territory_key, function(object, key) {
    value <- .registry_object(object)
    expect_false(
      any(grepl("year", names(value), ignore.case = TRUE)),
      label = paste(object, "has no year column")
    )
    declared <- stringr::str_split_1(key, ",\\s*")
    expect_true(
      all(declared %in% names(value)),
      label = paste(object, "declares only columns it has")
    )
    expect_setequal(
      declared,
      names(value)[whep:::.is_territory_key(names(
        value
      ))]
    )
  })
})

# The trap the register exists to make visible: a column named `polity_code`
# that is not a polity code. If a carrier were ever pointed at it, every join
# from that dataset to `polities` would come back empty and nothing else would
# notice.
test_that("a carried polity column really holds polity codes", {
  registry <- .registered_of_kind("package_data") |>
    dplyr::filter(.data$status == "carried")

  expect_gt(nrow(registry), 0L)
  purrr::walk2(registry$object, registry$carrier, function(object, carrier) {
    expect_false(is.na(carrier), label = paste(object, "names a carrier"))
    value <- .registry_object(object)
    expect_true(carrier %in% names(value))
    codes <- stats::na.omit(value[[carrier]])
    expect_gt(length(codes), 0L)
    expect_true(
      all(codes %in% whep::polities$polity_code),
      label = paste(object, carrier, "holds only polity codes")
    )
  })
})

test_that("regions_full's legacy polity_code is not a polity code", {
  # Documented on both datasets as a legacy ISO3-like prefix. Pinned here so
  # the two vocabularies cannot be quietly conflated: if this column is ever
  # migrated to real codes, the register and the docs must move with it.
  purrr::walk(c("regions_full", "polities_cats"), function(object) {
    value <- .registry_object(object)
    stems <- stats::na.omit(value$polity_code)
    expect_gt(length(stems), 0L)
    expect_equal(sum(stems %in% whep::polities$polity_code), 0L)
  })
})

# The register's `resolver` column, re-executed. There are two present-day
# routes and the register names which one each object takes: the numeric
# `area_code` route through the crosswalk, and the label route for an object
# whose territory key is an ISO3 code or a name. Reading the route off the
# register rather than hard-coding it is what keeps that column load-bearing.
.present_day_resolution <- function(object, resolver) {
  value <- .registry_object(object)
  if (startsWith(resolver, "add_polity_code")) {
    return(
      tibble::tibble(area_code = value$code) |>
        whep::add_polity_code(year_column = NULL) |>
        dplyr::pull("polity_code")
    )
  }
  label_column <- stringr::str_match(
    resolver,
    "^resolve_polity_label\\(([^,)]+)"
  )[, 2]
  expect_true(label_column %in% names(value))
  whep::resolve_polity_label(
    value[[label_column]],
    year = whep:::.present_day_polity_year()
  )
}

test_that("the present-day carrier is what the documented resolution gives", {
  registry <- .registered_of_kind("package_data") |>
    dplyr::filter(
      .data$identity == "present_day_polity" & .data$status == "carried"
    )

  expect_gt(nrow(registry), 1L)
  purrr::pwalk(
    list(registry$object, registry$carrier, registry$resolver),
    function(object, carrier, resolver) {
      value <- .registry_object(object)
      expect_equal(
        .present_day_resolution(object, resolver),
        value[[carrier]],
        label = paste(object, "matches", resolver)
      )
    }
  )
})

# THE FRESHNESS BACKSTOP FOR THE GLEAM TABLE. `test_data_raw_freshness.R`
# cannot rebuild `data/livestock_coefs.rda` -- its builder needs `openxlsx`,
# which the package does not declare -- so nothing there would notice
# `gleam_geographic_hierarchy$reporting_polity_code` going stale against the
# resolver. The equality above is that check: the column is recomputed from
# `polities` on every run, so a snapshot refresh that moves a country's polity
# fails here until the table is rebuilt.
test_that("the GLEAM registry's carried polity is a present-day one", {
  value <- whep::gleam_geographic_hierarchy
  codes <- value$reporting_polity_code

  # One row per country, one polity per country: an aggregate answering for two
  # of GLEAM's countries would be a bucket, not an identity (whep#563).
  expect_equal(anyDuplicated(stats::na.omit(codes)), 0L)
  expect_equal(nrow(value), 204L)

  # `polities` is an sf data frame and sf is only suggested, so the attribute
  # columns are taken by name rather than by subsetting the object.
  found <- match(codes, whep::polities$polity_code)
  live <- list(
    end_year = whep::polities$end_year[found],
    successor = whep::polities$successor[found]
  )
  resolved <- !is.na(codes)
  # Every resolved code is an OPEN period: it reaches the snapshot's sentinel
  # and nothing succeeds it. Resolving at any earlier year would land some of
  # them on a period that has since been succeeded -- 38 of the 204 iso3 values
  # name a different polity at 1961 than at 2010.
  expect_equal(
    unique(live$end_year[resolved]),
    max(whep::polities$end_year, na.rm = TRUE)
  )
  successors <- live$successor[resolved]
  expect_true(all(is.na(successors) | !nzchar(trimws(successors))))

  # The gaps are named rather than counted: each is a territory GLEAM reports
  # and whep-polities has no polity for at all (upstream whep-polities#187,
  # the same class as ABW and VAT in whep-polities#185). `NA` keeps them
  # visible; inventing a polity downstream is what the epic forbids.
  expect_setequal(value$iso3[!resolved], c("ATF", "SGS", "WLF"))
})

test_that("a consumer-resolved label really needs the consumer's year", {
  registry <- .registered_of_kind("package_data") |>
    dplyr::filter(.data$status == "resolved_by_consumer")

  expect_gt(nrow(registry), 0L)
  expect_true(all(is.na(registry$carrier)))
  expect_false(any(is.na(registry$resolver)))

  labels <- sort(unique(whep::mueller_synthetic_n$iso3c))
  early <- whep::resolve_polity_label(
    labels,
    source = "mueller-synthetic-n",
    year = 1961L
  )
  late <- whep::resolve_polity_label(
    labels,
    source = "mueller-synthetic-n",
    year = 2020L
  )
  moved <- !is.na(early) & !is.na(late) & early != late
  # Attaching one polity code to this table would be wrong for these labels
  # whichever year it was resolved at, which is why it stays identity-free.
  expect_gt(sum(moved), 0L)
})

test_that("registered pins name real whep_inputs aliases", {
  pins <- .registered_of_kind("input_pin")

  expect_gt(nrow(pins), 0L)
  expect_true(all(pins$object %in% whep::whep_inputs$alias))
})

test_that("registered function outputs are functions this package exports", {
  outputs <- .registered_of_kind("function_output")

  expect_gt(nrow(outputs), 0L)
  purrr::walk(outputs$object, function(object) {
    expect_true(is.function(getExportedValue("whep", object)))
  })
})

test_that("the territory-key vocabulary matches no item or grouping code", {
  # `biomass_coefs$Code` is a crop code. A looser pattern -- a bare "code", or
  # anything ending in "_code" -- pulls it and several other item tables into
  # the register, which is how a register of territorial identity fills up with
  # rows about crops. A supra-national grouping is excluded on purpose: it
  # names a class of places, never a state.
  expect_false(any(whep:::.is_territory_key(names(whep::biomass_coefs))))
  expect_false(any(whep:::.is_territory_key(names(whep::items_prod))))
  expect_false(any(whep:::.is_territory_key(
    c("region", "gleam_region", "region_UN_sub", "continent")
  )))
  expect_true(all(whep:::.is_territory_key(c("area_code", "ISO", "iso3c"))))
})
