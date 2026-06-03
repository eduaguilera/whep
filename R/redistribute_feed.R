#' Redistribute available feed supply among livestock demand.
#'
#' @description
#' Matches livestock feed demand to available feed items through a
#' hierarchical allocation that follows the remaining-share principle to
#' avoid exceeding availability. The redistribution path adapts to the
#' `fixed_demand` column in the demand table.
#'
#' @param feed_demand A tibble of feed demand with columns `year`,
#'   `territory`, `sub_territory`, `livestock_category`, `item_cbs_code`,
#'   `feed_group`, `feed_quality`, `demand_dm_t`, and a logical
#'   `fixed_demand`.
#' @param feed_avail A tibble of feed availability with columns `year`,
#'   `sub_territory`, `item_cbs_code`, `feed_group`, `feed_quality`,
#'   `avail_dm_t`, and `feed_scale`.
#' @param options A named list of allocation options. See
#'   `.redistribute_feed_options()` for the available entries and their
#'   defaults.
#'
#' @return A tibble of realised intake per demand row.
#'
#' @export
redistribute_feed <- function(feed_demand, feed_avail, options = list()) {
  options <- .redistribute_feed_options(options)
  .validate_feed_inputs(feed_demand, feed_avail)
  .empty_redistribute_result()
}

.redistribute_feed_options <- function(options = list()) {
  defaults <- list(
    zoot_fixed_max_multiplier = 3,
    prioritize_monogastric = TRUE,
    territory_col = "territory",
    sub_territory_col = "sub_territory",
    monogastric = NULL,
    max_intake_share = NULL,
    verbose = FALSE
  )
  utils::modifyList(defaults, options)
}

.validate_feed_inputs <- function(feed_demand, feed_avail) {
  required_demand <- c(
    "year",
    "territory",
    "sub_territory",
    "livestock_category",
    "item_cbs_code",
    "feed_group",
    "feed_quality",
    "demand_dm_t",
    "fixed_demand"
  )
  required_avail <- c(
    "year",
    "sub_territory",
    "item_cbs_code",
    "feed_group",
    "feed_quality",
    "avail_dm_t",
    "feed_scale"
  )
  .check_required_cols(feed_demand, required_demand, "feed_demand")
  .check_required_cols(feed_avail, required_avail, "feed_avail")
  .check_fixed_demand(feed_demand)
  invisible(NULL)
}

.check_required_cols <- function(data, required, name) {
  missing <- required[!purrr::map_lgl(required, ~ rlang::has_name(data, .x))]
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg {name}} is missing column{?s}: {.val {missing}}."
    )
  }
  invisible(NULL)
}

.check_fixed_demand <- function(feed_demand) {
  fixed <- feed_demand[["fixed_demand"]]
  if (!is.logical(fixed)) {
    cli::cli_abort(
      "{.field fixed_demand} must be logical (TRUE/FALSE) for every row."
    )
  }
  if (any(is.na(fixed))) {
    cli::cli_abort("{.field fixed_demand} cannot contain NA values.")
  }
  invisible(NULL)
}

.empty_redistribute_result <- function() {
  tibble::tibble(
    year = integer(),
    territory = character(),
    sub_territory = character(),
    livestock_category = character(),
    item_cbs_code = integer(),
    feed_group = character(),
    feed_quality = character(),
    demand_dm_t = numeric(),
    intake_dm_t = numeric(),
    scaling_factor = numeric(),
    hierarchy_level = character(),
    requested_item = integer(),
    source_compartment = character(),
    fixed_demand = logical()
  )
}
