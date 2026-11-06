# The palm-kernel estimator uses the last available old-vintage observations,
# followed by the new vintage's annual oil-production growth (whep#1624).
# A production build's incidental year margin cannot define that support.
# The 2013/2014 boundary below is the existing `.fix_palm_kernels()`
# vintage boundary; it does not truncate the earlier old-vintage anchors.
.cbs_palm_support <- function(inputs, years) {
  if (max(years) <= 2013L) {
    return(NULL)
  }
  list(
    fbs_old = .cbs_palm_source(inputs$fbs_old, "faostat-fbs-old", NULL),
    fbs_new = .cbs_palm_source(
      inputs$fbs_new,
      "faostat-fbs-new",
      2014L:max(years)
    )
  )
}

.cbs_palm_source <- function(data, pin_alias, years) {
  query <- attr(data, "cbs_extract_query")
  covered <- !is.null(query) &&
    identical(query$pin_alias, pin_alias) &&
    is.null(query$elements) &&
    (isTRUE(query$all_years) ||
      (!is.null(years) && all(years %in% query$years)))
  if (!covered) {
    data <- .extract_cb(pin_alias, years = years, keep_members = TRUE)
  }
  # Retain a private copy: filtering or filling must not mutate the caller's
  # cached production object. Query provenance records even an empty read.
  .filter_years(data.table::copy(data), years) |>
    tibble::as_tibble()
}

.cbs_palm_coverage <- function(inputs, method) {
  support <- inputs$palm_support
  if (is.null(support)) {
    return(inputs)
  }
  queries <- purrr::map(support, \(data) {
    list(query = attr(data, "cbs_extract_query"), rows_read = nrow(data))
  })
  support <- .cbs_coverage_inputs(support, method)
  inputs$palm_support <- support
  attr(inputs, "palm_kernel_support") <- list(
    sources = queries,
    rows_used = purrr::map_int(support, nrow),
    coverage_log = attr(support, "coverage_log")
  )
  inputs
}
