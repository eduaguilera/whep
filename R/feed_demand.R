.assign_bouwman_feed_class <- function(
  feed_demand,
  animals_codes = whep::animals_codes
) {
  demand <- tibble::as_tibble(feed_demand)
  live_anim_col <- .find_feed_class_col(
    demand,
    c("live_anim", "Live_anim"),
    "feed_demand"
  )
  live_anim_code_col <- .find_feed_class_col(
    demand,
    c("live_anim_code", "Live_anim_code"),
    "feed_demand"
  )
  item_prod_code_col <- .find_feed_class_col(
    demand,
    c("item_prod_code", "item_code_prod"),
    "feed_demand"
  )

  animals <- tibble::as_tibble(animals_codes) |>
    dplyr::transmute(
      .feed_live_anim = as.character(.data$item_cbs),
      .feed_live_anim_code = .normalise_feed_class_code(.data$item_cbs_code),
      item_bouwman = .data$item_bouwman
    ) |>
    dplyr::distinct(
      .data$.feed_live_anim,
      .data$.feed_live_anim_code,
      .keep_all = TRUE
    )

  demand |>
    dplyr::mutate(
      .feed_live_anim = as.character(.data[[live_anim_col]]),
      .feed_live_anim_code = .normalise_feed_class_code(
        .data[[live_anim_code_col]]
      ),
      .feed_item_prod_code = .normalise_feed_class_code(
        .data[[item_prod_code_col]]
      )
    ) |>
    dplyr::select(-dplyr::any_of("item_bouwman")) |>
    dplyr::left_join(
      animals,
      by = c(".feed_live_anim", ".feed_live_anim_code")
    ) |>
    dplyr::mutate(
      item_bouwman = dplyr::case_when(
        .data$.feed_live_anim_code == "946" &
          .data$.feed_item_prod_code %in% .buffalo_dairy_product_codes() ~
          "Dairy cattle",
        TRUE ~ .data$item_bouwman
      )
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        ".feed_live_anim",
        ".feed_live_anim_code",
        ".feed_item_prod_code"
      ))
    )
}

.find_feed_class_col <- function(data, candidates, data_name) {
  matches <- candidates[rlang::has_name(data, candidates)]
  if (length(matches) == 0) {
    cli::cli_abort(
      "{.arg {data_name}} is missing column{?s}: {.val {candidates}}."
    )
  }
  matches[[1L]]
}

.normalise_feed_class_code <- function(x) {
  stringr::str_remove(as.character(x), "\\.0+$")
}

.buffalo_dairy_product_codes <- function() {
  c("951", "952", "953")
}
