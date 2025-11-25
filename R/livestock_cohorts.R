#' Calculate Cohorts and Systems Distribution
#'
#' @description
#' Distributes livestock head counts into cohorts and production systems using provided shares.
#' This step is essential for applying GLEAM 3.0 methodology, which differentiates emissions
#' factors by animal cohort (e.g., adult females, calves) and production system (e.g., grassland, mixed).
#'
#' @param data A dataframe containing at least `year`, `geog_id`, `species`, and `heads`.
#' @param cohort_shares A dataframe with columns `species`, `cohort`, `share`.
#' @param system_shares A dataframe with columns `species`, `system`, `share` (and optionally `geog_id`).
#'
#' @return A dataframe with added columns `cohort`, `system`, and updated `heads`.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Dummy data
#'   data <- tibble::tibble(
#'     year = 2020,
#'     geog_id = "USA",
#'     species = "Cattle",
#'     heads = 1000
#'   )
#'   cohort_shares <- tibble::tibble(
#'     species = "Cattle",
#'     cohort = c("Adult Female", "Adult Male", "Calf"),
#'     share = c(0.5, 0.3, 0.2)
#'   )
#'   system_shares <- tibble::tibble(
#'     species = "Cattle",
#'     system = c("Grassland", "Mixed"),
#'     share = c(0.6, 0.4)
#'   )
#'   calculate_cohorts_systems(data, cohort_shares, system_shares)
#' }
#'
#' @importFrom dplyr left_join mutate select filter group_by summarize rename
calculate_cohorts_systems <- function(data, cohort_shares, system_shares) {
  
  # 1. Join System Shares
  if ("geog_id" %in% names(system_shares)) {
    data_sys <- data |>
      dplyr::left_join(system_shares, by = c("geog_id", "species"))
  } else {
    data_sys <- data |>
      dplyr::left_join(system_shares, by = "species")
  }
  
  data_sys <- data_sys |>
    dplyr::mutate(heads_system = heads * share) |>
    dplyr::select(-heads, -share) |>
    dplyr::rename(heads = heads_system)
  
  # 2. Join Cohort Shares
  data_cohort <- data_sys |>
    dplyr::left_join(cohort_shares, by = "species", relationship = "many-to-many")
  
  data_final <- data_cohort |>
    dplyr::mutate(heads_cohort = heads * share) |>
    dplyr::select(-heads, -share) |>
    dplyr::rename(heads = heads_cohort)
  
  return(data_final)
}
