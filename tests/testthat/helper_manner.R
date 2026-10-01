# Synthetic-path MANNER ef for one row of driver values (used by the
# temperature-bound sweep in test_manner_model.R).
.manner_grid_ef <- function(fertiliser, ...) {
  drivers <- c(list(...), irrigated = FALSE)
  whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = fertiliser,
    drivers = drivers
  )$ef
}
