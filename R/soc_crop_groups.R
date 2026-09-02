# Crop groups for the soil carbon balance (Module B).
#
# The grouping convention is Spain_Hist's, adopted here as a CONVENTION only
# (Spain_Hist is verification-only for WHEP; nothing is imported):
#
#   herbaceous crops pool into one group per irrigation regime, because they
#   rotate -- a wheat field this year is a barley field next year, so no
#   land-use-change event happens inside the pool;
#   woody crops keep their species, because an olive grove holds its land
#   from one year to the next;
#   irrigated and rainfed are separate groups throughout.
#
# The vocabulary is `items_prod_full`, which already carries `Herb_Woody`
# and the biomass species name per `item_prod_code` (128 herbaceous, 61
# woody among the primary crops; the three unclassified named items are
# derived products -- palm kernels, palm oil, cotton seed -- that carry no
# harvested area of their own).

#' Assign each crop to its soil-carbon crop group.
#'
#' @description
#' Map `item_prod_code` and an irrigation flag to the crop group the soil
#' carbon balance marches: `cropland_rainfed_herbaceous`,
#' `cropland_irrigated_herbaceous`, or one group per woody species such as
#' `cropland_rainfed_olive` (the species slug comes from `Name_biomass`, the
#' column Spain_Hist's own category is built from, so it is "Olive" rather
#' than the `names_cats` key "Olives"). Every label keeps the `cropland_`
#' prefix, which
#' is what lets the balance recognise a group as cropland for its soil-cover
#' curve, its C:N lookup and its water term without enumerating groups.
#'
#' @param item_prod_code Integer vector of FAOSTAT production item codes.
#' @param irrigated Logical vector, recycled: is the area irrigated?
#' @param vocabulary The crop vocabulary, a tibble with `item_prod_code`,
#'   `Herb_Woody` and `Name_biomass`. Defaults to [items_prod_full].
#' @return A character vector of group labels, one per input element. An
#'   item with no `Herb_Woody` classification aborts, naming it: an
#'   unclassified crop silently pooled into the wrong group would move
#'   carbon between groups with nothing recording it.
#' @export
#' @examples
#' soc_crop_group(c(15L, 260L, 260L), irrigated = c(FALSE, FALSE, TRUE))
soc_crop_group <- function(
  item_prod_code,
  irrigated = FALSE,
  vocabulary = whep::items_prod_full
) {
  .check_columns(
    vocabulary,
    c("item_prod_code", "Herb_Woody", "Name_biomass"),
    "vocabulary"
  )
  n <- max(length(item_prod_code), length(irrigated))
  code <- as.integer(rep_len(item_prod_code, n))
  irrig <- rep_len(as.logical(irrigated), n)
  # items_prod_full stores the code as CHARACTER and carries a "Fallow" row
  # (data-raw/harmonization_tables.R), so the key is coerced explicitly here
  # rather than left to match()'s implicit coercion.
  voc <- vocabulary |>
    dplyr::mutate(
      item_prod_code = suppressWarnings(as.integer(.data$item_prod_code))
    ) |>
    dplyr::filter(!is.na(.data$item_prod_code)) |>
    dplyr::distinct(.data$item_prod_code, .keep_all = TRUE)
  idx <- match(code, voc$item_prod_code)
  kind <- voc$Herb_Woody[idx]
  missing <- unique(code[is.na(idx) | is.na(kind) | kind == ""])
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{length(missing)} crop{?s} ha{?s/ve} no {.field Herb_Woody} class:
       {.val {missing}}.",
      i = "Every crop in the carbon-input layer must be classified; an
           unclassified crop silently pooled into a group would move carbon
           between groups with nothing recording it.",
      i = "Add the row to {.file inst/extdata/harmonization/names_cats.csv}
           and rebuild with {.file data-raw/harmonization_tables.R}."
    ))
  }
  regime <- dplyr::if_else(irrig, "irrigated", "rainfed")
  species <- .soc_species_slug(voc$Name_biomass[idx])
  kind_slug <- dplyr::if_else(kind == "Woody", species, "herbaceous")
  paste("cropland", regime, kind_slug, sep = "_")
}

# A species name as a label slug: lowercase, non-alphanumerics folded to one
# underscore, so "Peaches_nectarines" and "Peaches nectarines" agree and
# nothing downstream has to know the vocabulary's own spelling rules.
.soc_species_slug <- function(name) {
  name |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "")
}

# Is a land-use label a crop group (or plain cropland)? The balance's cropland
# predicates key on this rather than on `== "cropland"`, so a grouped run and
# an ungrouped one flow through the same seams.
.soc_is_cropland <- function(land_use) {
  stringr::str_detect(stringr::str_to_lower(land_use), "^cropland")
}
