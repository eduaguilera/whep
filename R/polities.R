#' WHEP polities
#'
#' @description
#' Lists all polities defined in the WHEP project that are obtained merging
#' together information from several sources.
#' The term polity comes from _political entity_ and is a more general term
#' than country.
#'
#' A polity can be defined as a fixed territory over a continuous
#' period of time. Whenever a region changes its overall territory, it should
#' start being treated as a new polity. Likewise, if there's only a change in
#' official name for a country or region, but no territory change, it will
#' still be the same polity.
#'
#' In most cases a polity is included in this list as a consequence of it
#' having reported production or trade data individually, as opposed to this
#' data being included in a larger territory. In this sense, most colonies are
#' considered their own polities if there is data for them.
#'
#' @returns
#' An sf object on top of a tibble where each row represents one polity.
#' It has the following columns:
#' - `polity_name`: Natural language name that uniquely represents a polity.
#'   It must not necessarily match the official name of the polity at some
#'   point in time. As a consequence, some names have been shortened or adapted
#'   for easier understanding. Polity names might change in the future as
#'   long as they are still unique. If you need to perform checks for specific
#'   polities, use `polity_code` instead.
#' - `polity_code`: Internal code that uniquely represents each polity. It
#'    follows a specific format for easier understanding. The format is
#'    `"XXX-yyyy-YYYY"`, where:
#'    - `XXX` is a corresponding ISO3 code if one exists, or an artificial one
#'      otherwise, but trying to be meaningful if possible.
#'    - `yyyy` is the `start_year` of the polity.
#'    - `YYYY` is the `end_year` of the polity.
#'
#'    For polities that traditionally refer to the same country (just a
#'    territory change), the `XXX` part will typically be the same as a hint,
#'    so that only the year range will vary.
#' - `start_year`: The year in which the polity starts.
#' - `end_year`: The year in which the polity ends.
#' - `m49_code`: The M49 code for the polity if one exists, as defined by the
#'   [United Nations](https://unstats.un.org/unsd/methodology/m49/overview/).
#' - `iso3_code`: One valid
#'   [ISO 3166-1 alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)
#'   code, if such code exists. Note: One polity could have more than one such
#'   code. If this situation is found, this column might be moved to a different
#'   table.
#' - `iso2_code`: One valid
#'   [ISO 3166-1 alpha-2](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2)
#'   code, if such code exists. Note: One polity could have more than one such
#'   code. If this situation is found, this column might be moved to a different
#'   table.
#' - `display_code`: A short code that uniquely represents a polity and might be
#'   useful for visualization purposes. It's initially intended to have the
#'   value as `polity_code`, but could change if necessary, as opposed to the
#'   `polity_code` not changing.
#' - `geometry`: An sf `MULTIPOLYGON` with the territorial information for the
#'   polity. Ideally each polity should have a non empty geometry, but this
#'   might not hold while the table is a work in progress.
#'
#' @details
#' This dataset itself was built from various other datasets. The methodology
#' followed to get the final result is roughly like this:
#'
#' 1. Clean data coming from other sources and prepare a tibble with
#'   same columns for all of them, so that they're all homogeneous. Check
#'   `get_polity_sources()` to know more about each source.
#' 2. Put together in a single tibble all rows from the different sources that
#'   were prepared in the previous step.
#' 3. Assign a polity name for each entry in the combined tibble. This name is
#'   also referred to as a _common name_, in the sense that a polity could come
#'   from more than one source, each having different names, and we want a
#'   common one to be able to group these into a single entry. This step is
#'   performed using a join with a manually crafted set of mappings from a
#'   combination of both original name and source, to a common name.
#' 4. Aggregate specific source data into a single entry, grouping by the common
#'   name defined in previous step. For now, this common name will also be the
#'   final value of `polity_name`. The aggregations are these:
#'     - `start_year` and `end_year`: A simple minimum and maximum is calculated
#'     for `start_year` and `end_year` respectively, with two edge cases:
#'       - If an entry in the `whep` source is present for this polity, use its
#'         year range directly. This is a simple way of overwriting the year
#'         range in cases where the original years needed to be changed for some
#'         reason, and the min/max approach does not give the desired year.
#'       - If all values are `NA` for either `start_year` or `end_year`, a
#'         default value is used instead, which denotes the limits of the period
#'         we want to cover. These years are 1800 (earliest source used) and
#'         2025 (present day) respectively. This also applies for the edge case
#'         of using the `whep` source. If there is an `NA` value there, the
#'         default value is used.
#'     - `m49_code`, `iso2_code`, `iso3_code` and `geometry`: Check whether
#'       there is a single unique value, excluding `NA`s, and use that value as
#'       aggregate. Otherwise, fail. This can change in the future if there is
#'       something meaningful to merge but for now it's a useful way to make
#'       sure there is no information loss.
#' 5. Assign a unique code (`polity_code`) for each polity. This is also done
#'   with a manually crafted mapping, which is used to perform a join. The
#'   mapping table defines the polity code for each polity name, the common
#'   name created before, which also uniquely defines a polity. However, after
#'   this code is set, all subsequent operations on polities should be done
#'   using this code instead of the polity name, since it is expected for the
#'   code not to change in the future, but the polity name could, so it's less
#'   reliable.
#' 6. Create the `display_code`. As mentioned before, this is currently just
#'   the same as `polity_code`, but could change in the future if it seems a
#'   good idea to use other intuitive short names.
#'
#' @export
#'
#' @examples
#' get_polities()
get_polities <- function() {
  .merge_datasets() |>
    .set_polity_name_code() |>
    .build_display_code() |>
    .set_column_types() |>
    dplyr::arrange(polity_name) |>
    sf::st_as_sf() |>
    # TODO: think about m49 and iso codes after refactoring, here or elsewhere?
    dplyr::select(
      polity_name,
      polity_code,
      start_year,
      end_year,
      display_code
    )
}

#' WHEP polity sources
#'
#' @description
#' Lists all sources for each requested polity along with their specific data.
#'
#' @param polity_codes The values of `polity_code` for the requested polities.
#'   This argument accepts three different kinds of values:
#'   - A single string, the `polity_code` of the single requested polity.
#'   - A character vector, containing all the values of `polity_code` for the
#'     requested polities.
#'   - Default `NULL` value, which is equivalent to passing a character vector
#'     containing the codes of all polities present in `get_polities()`.
#'
#' @returns
#' A single tibble with the following columns:
#' - `original_name`: The natural language name for the region in that specific
#'   source, before defining a common name.
#' - `source`: The source this region was taken from. It can be one of the
#'   following:
#'   - `federico_tena`: [Federico-Tena list of polities
#'     ](https://edatos.consorciomadrono.es/dataset.xhtml?persistentId=doi:10.21950/3SK54X),
#'     covering the period 1800-1938. This is the earliest source used, being
#'     the only one that covers the whole nineteenth century. It includes no
#'     geometry.
#'   - `faostat`: [FAOSTAT country/region list
#'     ](https://www.fao.org/faostat/en/#definitions) that is used in their
#'     agricultural production and trade data, and covers from 1961 to
#'     present day. It includes no geometry.
#'   - `m49`: [United Nations list of countries
#'     ](https://unstats.un.org/unsd/methodology/m49/overview/) using the M49
#'     standard. It was first issued in 1970 and aims to cover present day
#'     countries, but here some historical codes no longer in use are also
#'     included. It includes no geometry.
#'   - `cshapes`: [CShapes 2.0](https://icr.ethz.ch/data/cshapes/) mapping
#'     of territorial changes in independent and dependent states. It covers
#'     the period 1886-2017. For now this is the only source that includes
#'     the geometry of the polity's territory.
#'   - `whep`: Custom made source for overwriting specific data. For now,
#'     it is only used to overwrite the year range for a polity when the
#'     one calculated automatically is not satisfying. Thus, it includes
#'     no geometry or other data, but this could change in the future if
#'     it becomes a necessity.
#' - `common_name`: The given name that will be shared by entries across
#'   sources if they represent the same polity. This will be `polity_name`
#'   in the main tibble obtained from `get_polities()`.
#' - `polity_code`: The unique code identifying the polity. Note that this
#'   is included as a helper but there might be more than one row with the
#'   same `polity_code` in this table if the polity appears in more than
#'   one source.
#' - `notes`: A natural language description of the polity, focused on
#'   clarifying the existence of this entry, possibly including its relation
#'   with other entries. In the case of `whep` source entries, it explains
#'   why that entry was created to overwrite the other sources' information.
#' - Other information specifically found in that source that might be
#'   aggregated in the final polity table. This includes `start_year`,
#'   `end_year`, `m49_code`, `iso2_code`, `iso3_code` and `geometry`. For
#'   more details about these, check `get_polities()`.
#'
#' @export
#'
#' @examples
#' get_polity_sources()
#' get_polity_sources("VEN-1821-2025")
#' get_polity_sources(c("VEN-1821-2025", "UGA-1962-2025"))
get_polity_sources <- function(polity_codes = NULL) {
  polities <- get_polities() |>
    tibble::as_tibble()

  if (is.null(polity_codes)) {
    polity_codes <- polities |>
      dplyr::select(polity_code)
  } else {
    polity_codes <- tibble::tibble(polity_code = polity_codes)
  }

  polity_names <- polities |>
    dplyr::inner_join(polity_codes, by = "polity_code") |>
    dplyr::select(common_name = polity_name, polity_code)

  .merge_datasets() |>
    .add_common_names() |>
    dplyr::inner_join(polity_names, by = "common_name") |>
    dplyr::arrange(original_name)
}

get_polity_code <- function(
  originals,
  code_mappings = k_polity_alias_table,
  open_mismatches = FALSE
) {
  result <- originals |>
    dplyr::left_join(code_mappings, c("original_name", "year"))

  .warn_if_no_code_match(result)

  if (open_mismatches) {
    .analize_polities_matching(result)
  }

  result
}

.analize_polities_matching <- function(result) {
  result |>
    .identify_issues() |>
    .group_years() |>
    pointblank::create_agent() |>
    pointblank::col_vals_equal(issue, "ok") |>
    pointblank::interrogate() |>
    print()
}

.group_years <- function(data) {
  data |>
    dplyr::group_by(original_name, polity_code, issue) |>
    dplyr::arrange(year) |>
    dplyr::mutate(group = cumsum(c(1, diff(year) > 1))) |>
    dplyr::group_by(group, .add = TRUE) |>
    dplyr::summarise(
      start_year = min(year),
      end_year = max(year),
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-group)
}

.identify_issues <- function(data) {
  data |>
    dplyr::mutate(
      any_year_match = any(!is.na(polity_code)),
      all_year_match = all(!is.na(polity_code)),
      issue = dplyr::case_when(
        any_year_match & !all_year_match ~ "some_years_missing",
        !any_year_match ~ "unmatched_polity",
        all_year_match ~ "ok",
        .default = "unexpected_issue"
      ),
      .by = "original_name"
    )
}

.expand_alias_table <- function(intermediate_alias) {
  intermediate_alias |>
    dplyr::group_by(original_name, polity_code) |>
    tidyr::expand(year = seq(start_year, end_year)) |>
    dplyr::ungroup() |>
    dplyr::select(original_name, year, polity_code)
}

# Intended to use only datasets that actually define polygons
# For now this is only CShapes
.merge_datasets <- function() {
  dplyr::bind_rows(
    .prepare_cshapes()
  )
}

.set_polity_name_code <- function(merged_datasets) {
  .error_polity_code_unmatched(merged_datasets, k_polity_names_codes)

  merged_datasets |>
    dplyr::inner_join(
      k_polity_names_codes,
      by = "original_name",
      unmatched = "error"
    )
}

.set_column_types <- function(polities) {
  int_cols <- c("start_year", "end_year")
  geom_cols <- c("geometry")
  chr_cols <- setdiff(names(polities), c(int_cols, geom_cols))

  polities |>
    dplyr::mutate(
      across(all_of(int_cols), as.integer),
      across(all_of(chr_cols), as.character)
    )
}

.prepare_historical_m49 <- function() {
  k_historical_m49 |>
    # TODO: Do this cleanly. M49 code 728 is reused, so only use for new country
    dplyr::mutate(
      m49_code = ifelse(m49_name == "Spanish North Africa", NA, m49_code)
    ) |>
    dplyr::mutate(source = k_source_m49) |>
    dplyr::select(
      original_name = m49_name,
      source,
      start_year,
      end_year,
      notes,
      m49_code
    )
}

.prepare_faostat <- function() {
  k_faostat_regions |>
    dplyr::mutate(source = k_source_faostat) |>
    dplyr::select(
      original_name = country_name,
      source,
      start_year,
      end_year,
      m49_code,
      iso2_code,
      iso3_code
    )
}

.prepare_federico_tena <- function() {
  k_federico_tena_polities |>
    dplyr::mutate(
      source = k_source_federico_tena,
      # Force first year because federico tena has earliest cover
      start_year = ifelse(is.na(start_year), k_polity_first_year, start_year)
    ) |>
    dplyr::select(
      original_name = polity_name,
      source,
      start_year,
      end_year,
      notes
    )
}

.prepare_cshapes <- function() {
  k_cshapes |>
    dplyr::mutate(
      source = k_source_cshapes,
      polity_name = .add_years_in_name(polity_name, start_year, end_year)
    ) |>
    dplyr::select(
      original_name = polity_name,
      source,
      start_year,
      end_year,
      geometry
    )
}

.prepare_whep_fixes <- function() {
  k_whep_polity_fixes |>
    dplyr::mutate(source = k_source_whep) |>
    dplyr::select(
      original_name = polity_name,
      source,
      start_year,
      end_year,
      notes
    )
}

# TODO: Think if worth to shorten, maybe another mapping table
.build_display_code <- function(polities) {
  polities |>
    dplyr::mutate(display_code = polity_code)
}

# Used for dataset generation in constants.R
.clean_faostat_regions <- function() {
  "faostat_regions" |>
    whep_read_file() |>
    dplyr::select(
      country_code = `Country Code`,
      country_name = Country,
      m49_code = `M49 Code`,
      iso2_code = `ISO2 Code`,
      iso3_code = `ISO3 Code`,
      start_year = `Start Year`,
      end_year = `End Year`
    )
}

.clean_unstats_m49 <- function() {
  "unstats_m49" |>
    whep_read_file() |>
    dplyr::select(
      m49_code = `M49 Code`,
      m49_name = `Country or Area`,
      region1_code = `Intermediate Region Code`,
      region1_name = `Intermediate Region Name`,
      region2_code = `Sub-region Code`,
      region2_name = `Sub-region Name`,
      region3_code = `Region Code`,
      region3_name = `Region Name`,
      region4_code = `Global Code`,
      region4_name = `Global Name`,
      iso2_code = `ISO-alpha2 Code`,
      iso3_code = `ISO-alpha3 Code`,
      least_developed = `Least Developed Countries (LDC)`,
      land_locked_developing = `Land Locked Developing Countries (LLDC)`,
      small_island_developing = `Small Island Developing States (SIDS)`
    ) |>
    dplyr::mutate(
      across(
        c(least_developed, land_locked_developing, small_island_developing),
        ~ !is.na(.x)
      )
    )
}

.clean_historical_m49 <- function() {
  "historical_m49" |>
    whep_read_file() |>
    dplyr::mutate(is_old = !is.na(end_year)) |>
    dplyr::select(-iso3166_code, -code_mismatch)
}

.clean_federico_tena_polities <- function() {
  "federico_tena_polities" |>
    whep_read_file() |>
    dplyr::select(
      polity_name = list_of_trading_polities,
      start_year = trading_polity_starting,
      end_year = trading_polity_end,
      population = population_1913_000,
      notes = notes
    ) |>
    # There was an empty row in the original dataset
    dplyr::filter(!is.na(polity_name)) |>
    # Remove virtual time boundaries due to dataset study range
    dplyr::mutate(
      start_year = ifelse(start_year == 1800, NA, start_year),
      end_year = ifelse(end_year >= 1938, NA, end_year)
    )
}

.clean_cshapes <- function() {
  .load_cshapes() |>
    .filter_relevant_area_changes() |>
    # Remove virtual time boundaries due to dataset study range
    dplyr::mutate(
      start_year = ifelse(start_year <= 1886, NA, start_year),
      end_year = ifelse(end_year >= 2019, NA, end_year)
    ) |>
    dplyr::select(
      polity_name = country_name,
      start_year,
      end_year,
      area
    ) |>
    dplyr::arrange(polity_name)
}

.load_cshapes <- function() {
  countries <- cshapes::cshp() |>
    sf::st_make_valid()

  countries |>
    dplyr::mutate(
      area = countries |>
        sf::st_area() |>
        units::set_units(km^2),
      start_year = lubridate::year(start),
      end_year = lubridate::year(end),
    )
}

.filter_relevant_area_changes <- function(countries) {
  zero_km2 <- units::set_units(0, km^2)

  countries |>
    dplyr::arrange(country_name, start_year) |>
    dplyr::group_by(country_name) |>
    # Remove same year changes
    dplyr::filter(dplyr::n() == 1 | start_year != end_year) |>
    # Aggregate adjacent entries with no area change
    dplyr::mutate(group_tmp = cumsum(c(0, diff(area) != zero_km2))) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      geometry = dplyr::first(geometry),
      area = dplyr::first(area),
      start_year = min(start_year),
      end_year = max(end_year),
      .by = c("country_name", "group_tmp")
    ) |>
    dplyr::select(-group_tmp)
}

.add_years_in_name <- function(name, start_year, end_year) {
  dplyr::case_when(
    is.na(end_year) ~ stringr::str_glue("{name}"),
    is.na(start_year) ~ stringr::str_glue("{name} (to {end_year})"),
    .default = stringr::str_glue("{name} ({start_year}-{end_year})"),
  )
}

.build_auto_polity_code <- function(name, start_year, end_year) {
  start_year <- ifelse(is.na(start_year), k_polity_first_year, start_year)
  end_year <- ifelse(is.na(end_year), k_polity_last_year, end_year)
  short <- name |>
    stringr::str_to_upper() |>
    stringr::str_sub(1, 3)

  stringr::str_glue("{short}-{start_year}-{end_year}")
}

# Use for debugging when you get failed test because of polity
# code not matching short name or year range.
.get_bad_code_polities <- function() {
  get_polities() |>
    tidyr::separate_wider_delim(
      polity_code,
      "-",
      names = c("code_iso", "code_start_year", "code_end_year"),
      cols_remove = FALSE
    ) |>
    dplyr::filter(
      as.integer(code_start_year) != start_year |
        as.integer(code_end_year) != end_year
    ) |>
    dplyr::select(-code_iso, -code_start_year, -code_end_year)
}

.error_polity_code_unmatched <- function(merged_datasets, polity_codes) {
  unmatched <- merged_datasets |>
    dplyr::anti_join(polity_codes, by = "original_name") |>
    dplyr::pull(original_name) |>
    purrr::map(.quotify)

  if (length(unmatched) > 0) {
    cli::cli_abort(
      "Polities with no defined polity_code: {unmatched}. Include them in polity_codes.csv."
    )
  }

  unmatched <- polity_codes |>
    dplyr::anti_join(merged_datasets, by = "original_name") |>
    dplyr::pull(original_name) |>
    purrr::map(.quotify)

  if (length(unmatched) > 0) {
    cli::cli_abort(
      "Polities with polity_code but missing in common names table: {unmatched}. Include them in common_names.csv."
    )
  }
}

.warn_if_no_code_match <- function(polities) {
  unmatched <- polities |>
    dplyr::filter(is.na(polity_code)) |>
    dplyr::select(-polity_code)

  if (nrow(unmatched) > 0) {
    unmatched_pairs <- stringr::str_glue(
      '"{unmatched$original_name}" in {unmatched$year}'
    )
    cli::cli_warn(c(
      "x" = "No polity code found for {length(unmatched_pairs)} {?entry/entries}:",
      "{unmatched_pairs}",
      "i" = "Call the function with the argument `open_mismatches = TRUE` for a more detailed interactive analysis."
    ))
  }
}

.quotify <- function(x) {
  stringr::str_glue('"{x}"')
}

# TODO: todos from removed old code:
# - Revise Edu's polities for special handlings
# - Introduce 'rest of continent'-like polities
#   "ROCE" "RAFR" "RASI" "REUR" "RLAM" "RNAM"
