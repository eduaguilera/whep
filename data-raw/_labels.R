# Shared label repairs for the vendored harmonization tables.
#
# Lives in its own file because TWO data-raw scripts read the same vendored CSVs:
# harmonization_tables.R builds regions_full and polities_cats from them, and
# table_mappings.R re-reads regions_full.csv directly as `regions_full_raw` to build the
# crosswalk. Repairing in one left the other corrupt — regions_full$name read "Curaçao"
# while polity_area_crosswalk$area_name still read "CuraÃ§ao", which is the same
# two-copies-of-one-fact shape this branch keeps closing, applied to a file read.

# Undo Latin-1-decoded UTF-8 in a character vector.
#
# The vendored tables carry the UTF-8 bytes for ç and ô decoded as two Latin-1 characters
# each: "CuraÃ§ao" for Curaçao, "CÃ´te d'Ivoire" for Côte d'Ivoire. iconv cannot fix it —
# "UTF-8" to "latin1" returns the string unchanged and the reverse double-encodes it
# further. Taking the raw bytes and re-reading them as UTF-8 is what works.
#
# Applied to every character column rather than a list of label columns. The first
# attempt named four label columns, fixed area 279's `name`, and left the same corruption
# in `iea`, `water_area` and `Lassaletta` — mojibake is never wanted in any string column,
# so the rule is the column's type.
#
# Not costing a join today, and that was checked rather than assumed: no alias exists for
# Curaçao in any spelling, so there is no correct label for the corrupt one to fail to
# match. It is a latent trap of the same shape as Eswatini, whose observed rows were filed
# under a name its area is not called — `areas_with_observed_data` pivots `name` and
# `FAOSTAT_name` and matches them against alias labels, and area 279's FAOSTAT_name is NA,
# so the corrupt `name` is the only label it has.
repair_mojibake <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  needs <- !is.na(x) & grepl("Ã", x)
  if (!any(needs)) {
    return(x)
  }
  x[needs] <- vapply(
    x[needs],
    function(v) {
      out <- tryCatch(
        rawToChar(iconv(v, "UTF-8", "latin1", toRaw = TRUE)[[1]]),
        error = function(e) v
      )
      Encoding(out) <- "UTF-8"
      out
    },
    character(1)
  )
  x
}

repair_table_labels <- function(x) {
  dplyr::mutate(x, dplyr::across(dplyr::where(is.character), repair_mojibake))
}

# "0" is not a country name and not a region.
#
# `polities_cats.csv` was exported with a literal "0" wherever a value is
# absent, in 13 character columns: the `eia` and `iea` country names and every
# `region_*` classification. `regions_full.csv`, which carries the SAME 40
# columns over a 198-row superset, uses blanks there and so reads as NA — which
# is how comparing the two tables turned this up. 17 of their 39 shared columns
# disagreed, and 13 of those disagreements were this sentinel rather than any
# difference of opinion.
#
# It matters because "0" reads as data. `!is.na(iea)` keeps all 198 rows instead
# of the 139 that have an IEA name, a join on `iea` matches the 59 zero rows to
# each other as though they were one country, and grouping by `region_UN` yields
# a "0" region. `excel_na` already normalises "", "NA", "#N/A", "#DIV/0!" and
# "#REF!" for exactly this reason; "0" simply was not in the list, and it cannot
# be added there because a numeric 0 is a real value in columns like `EU27` and
# `cbs`.
#
# So the substitution is restricted to character columns, where "0" cannot be
# meant.
blank_zero_sentinels <- function(x) {
  dplyr::mutate(
    x,
    dplyr::across(
      dplyr::where(is.character),
      \(v) dplyr::if_else(!is.na(v) & v == "0", NA_character_, v)
    )
  )
}
