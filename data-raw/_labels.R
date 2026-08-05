# Shared label repairs for the vendored harmonization tables.
#
# Lives in its own file because TWO data-raw scripts read the same vendored CSV:
# harmonization_tables.R builds regions_full and polities_cats from it, and
# table_mappings.R re-reads regions_full.csv directly as `regions_full_raw` to
# build polity_area_crosswalk. Repairing in one left the other corrupt --
# regions_full$name read the mended name while polity_area_crosswalk$area_name
# still read the corrupt one, two copies of one file read.

# Undo Latin-1-decoded UTF-8 in a character vector.
#
# The vendored tables carry the UTF-8 bytes for c-cedilla and o-circumflex
# decoded as two Latin-1 characters each, so Curacao and Cote d'Ivoire ship with
# a two-character U+00C3 pair where the accented letter belongs. iconv cannot
# mend it in one call -- "UTF-8" to "latin1" returns the string unchanged and the
# reverse double-encodes it further. Taking the Latin-1 bytes back out and
# re-reading them as UTF-8 is what works.
#
# The round trip is only accepted when the resulting bytes are valid UTF-8, so a
# string that merely happens to contain U+00C3 is left exactly as it was rather
# than turned into a different kind of garbage.
#
# Repaired here rather than in the CSV, which is a vendored harmonization table
# that would then diverge from its source -- the same override-on-read pattern
# `manual_area_prefixes` already uses in table_mappings.R.
repair_mojibake <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  # Every corrupt byte pair starts with the Latin-1 reading of a UTF-8 lead
  # byte, U+00C3 -- escaped rather than literal, as R/scrape_faostat.R does.
  needs <- !is.na(x) & grepl("\u00c3", x)
  if (!any(needs)) {
    return(x)
  }
  x[needs] <- vapply(
    x[needs],
    function(value) {
      bytes <- iconv(value, "UTF-8", "latin1", toRaw = TRUE)[[1]]
      if (is.null(bytes)) {
        return(value)
      }
      out <- rawToChar(bytes)
      Encoding(out) <- "UTF-8"
      if (!validUTF8(out)) {
        return(value)
      }
      out
    },
    character(1),
    USE.NAMES = FALSE
  )
  x
}

# Applied to every character column rather than to a hand-picked list of label
# columns: area 279's corruption also sits in `iea` and `Lassaletta`, and area
# 107's in `iea` and `water_area`, so the rule is the column's type.
repair_table_labels <- function(table) {
  dplyr::mutate(
    table,
    dplyr::across(dplyr::where(is.character), repair_mojibake)
  )
}
