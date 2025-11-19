# TODO: rewrite this outdated documentation

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
#' @format
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
#' @source TODO
"whep_polities"

#' Recode names to polity codes
#'
#' TODO: write docs
#'
#' @export
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
