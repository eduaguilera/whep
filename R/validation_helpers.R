# Internal helpers backing the inst/scripts validation figures. Kept as
# pure, stateless functions so the conservation-scoring and join logic can
# be unit-tested independently of the plotting scripts (which need large
# gridded parquet inputs to run).

# Relative error (%) of a gridded total against a reference total, with
# correct handling of the zero-reference case. A country the reference
# says has no animals/area but that carries spurious gridded mass is a
# genuine conservation failure, not a perfect match, so it scores `Inf`.
# Only a true zero-against-zero pair scores `0`.
.conservation_rel_error <- function(gridded, reference) {
  dplyr::case_when(
    reference > 0 ~ abs(gridded - reference) / reference * 100,
    gridded > 0 ~ Inf,
    .default = 0
  )
}

# Join gridded totals to their country reference keeping *every* country
# from both sides (`full_join`), so a reference country with no gridded
# output -- a total mass leak, the most severe conservation failure -- is
# retained and flagged instead of silently dropped by an inner join.
# Numeric columns named in `fill` have their post-join `NA`s (the absent
# side) set to `0` so downstream error scoring treats them as leaks.
.join_conservation <- function(gridded, reference, by, fill = NULL) {
  joined <- dplyr::full_join(gridded, reference, by = by)
  if (!is.null(fill)) {
    joined <- joined |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(fill), \(x) tidyr::replace_na(x, 0))
      )
  }
  joined
}
