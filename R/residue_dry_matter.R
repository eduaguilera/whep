# Dry-matter content of the residue the commodity balance feeds (whep#1215).
#
# The CBS residue items are composites. "Other crop residues" (2106) is the
# haulm, vines and pods of vegetables, melons, roots and pulses, whose residue
# dry-matter contents in `biomass_coefs` run from 0.13 (tomato) to 0.93
# (lentil). The feed allocator used to convert the whole item with the one
# `Product_kgDM_kgFM` of the `Name_biomass` "Other crop residues", 0.90, so a
# tonne of fresh tomato haulm was booked as 0.9 t of feed dry matter where its
# own coefficient gives 0.13. Measured on the pin at 2010, the crops behind
# 2106 average 0.50 weighted by residue mass and 0.46 weighted by the mass that
# is actually fed, so 2106 feed dry matter was overstated 1.94x; straw (2105)
# averages 0.885 against the 0.876 applied, so it barely moves.
#
# The crop behind each tonne is known where the CBS residue rows are made, and
# lost once they are summed into the item, so the content is computed here from
# the same split: each crop's feed share of its recovered residue
# (`.residue_recovered_split()`, the function `.read_crop_residues()` builds the
# CBS `feed` element with), converted with that crop's `Residue_kgDM_kgFM` and
# summed onto the CBS polity bucket exactly as `.read_crop_residues()` sums it.
# The result is the identity DM(feed) / FM(feed) for each CBS row, not a new
# coefficient.

# Per `(year, area_code, item_cbs_code)` dry-matter content of the residue feed
# element, `area_code` being the CBS polity bucket. `residues` is the output of
# `get_primary_residues()`, which carries both the fresh `value` and its
# `value_dm`.
.residue_feed_kgdm <- function(residues, feed_items = c(2105L, 2106L)) {
  if (!rlang::has_name(residues, "value_dm")) {
    cli::cli_abort(
      c(
        "The crop residues carry no {.field value_dm} column.",
        i = "Pass the output of {.fn get_primary_residues}."
      ),
      class = "whep_residue_no_dry_matter"
    )
  }
  feed <- residues |>
    dplyr::filter(
      as.integer(.data$item_cbs_code_residue) %in% feed_items,
      !is.na(.data$area_code)
    )
  if (nrow(feed) == 0L) {
    return(.empty_residue_kgdm())
  }
  feed |>
    .residue_recovered_split(warn = FALSE) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code_residue),
      # `feed_dm_t` is named for dry matter but carries the pin's fresh mass:
      # the destiny rates are unitless, so the split keeps the input's unit.
      feed_fm_t = .data$feed_dm_t,
      feed_true_dm_t = .data$feed_dm_t * .data$value_dm / .data$value
    ) |>
    dplyr::filter(.data$feed_fm_t > 0) |>
    .sum_residue_feed_to_polity()
}

# Sum the per-crop feed onto the CBS polity bucket, as `.read_crop_residues()`
# does for the balance itself, so each content row keys onto exactly one CBS
# row. A bucket whose crops include one without a dry-matter content gets no
# row: its CBS feed then reaches `.apply_residue_kgdm()` unmatched and is
# reported there.
.sum_residue_feed_to_polity <- function(feed) {
  dt <- .add_polity_columns_dt(
    data.table::as.data.table(feed),
    code_col = "area_code",
    year_col = "year",
    include_unmapped = FALSE
  )
  dt <- dt[!is.na(polity_code)]
  dt[,
    .(
      feed_fm_t = sum(feed_fm_t),
      feed_true_dm_t = sum(feed_true_dm_t)
    ),
    by = c("year", "polity_area_code", "item_cbs_code")
  ] |>
    tibble::as_tibble() |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$polity_area_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      residue_kgdm_kgfm = .data$feed_true_dm_t / .data$feed_fm_t
    ) |>
    dplyr::filter(!is.na(.data$residue_kgdm_kgfm))
}

.empty_residue_kgdm <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    item_cbs_code = integer(),
    residue_kgdm_kgfm = numeric()
  )
}

# Replace the item-level dry-matter content of the residue rows with their own
# crop-mix content, where one is supplied. `x` carries `year`, `area_code`,
# `item_cbs_code` and `product_kgdm_kgfm`. `residue_kgdm = NULL` leaves `x`
# untouched: a caller that supplies no residue table has no residue crop mix to
# apply, which is the case only for hand-built fixtures.
#
# A residue row the table does not cover keeps the item coefficient and is
# named, with its mass, when `mass_col` is given. It should not happen on the
# build path, where the table and the CBS rows come from the same residues.
.apply_residue_kgdm <- function(x, residue_kgdm, mass_col = NULL) {
  if (is.null(residue_kgdm)) {
    return(x)
  }
  out <- x |>
    dplyr::mutate(
      .year = as.integer(.data$year),
      .area_code = as.integer(.data$area_code),
      .item_cbs_code = as.integer(.data$item_cbs_code)
    ) |>
    dplyr::left_join(
      dplyr::rename(
        residue_kgdm,
        .year = "year",
        .area_code = "area_code",
        .item_cbs_code = "item_cbs_code"
      ),
      by = c(".year", ".area_code", ".item_cbs_code"),
      relationship = "many-to-one"
    )
  if (!is.null(mass_col)) {
    .warn_residue_kgdm_gap(out, mass_col)
  }
  out |>
    dplyr::mutate(
      product_kgdm_kgfm = dplyr::coalesce(
        .data$residue_kgdm_kgfm,
        .data$product_kgdm_kgfm
      )
    ) |>
    dplyr::select(
      -".year",
      -".area_code",
      -".item_cbs_code",
      -"residue_kgdm_kgfm"
    )
}

# Residue feed the crop-mix table did not cover keeps the item coefficient;
# name how much, so a desync between the table and the CBS is visible.
.warn_residue_kgdm_gap <- function(
  out,
  mass_col,
  feed_items = c(2105L, 2106L)
) {
  gap <- out$.item_cbs_code %in% feed_items & is.na(out$residue_kgdm_kgfm)
  if (!any(gap)) {
    return(invisible(NULL))
  }
  mass_t <- round(sum(out[[mass_col]][gap], na.rm = TRUE))
  n_rows <- sum(gap)
  cli::cli_warn(c(
    "!" = "{n_rows} residue feed rows ({mass_t} t fresh) have no crop-mix
       dry-matter content, so they keep the item's single coefficient.",
    "i" = "That coefficient is the one whep#1215 replaced; for
       {.val Other crop residues} it overstates dry matter about 1.9x."
  ))
  invisible(NULL)
}
