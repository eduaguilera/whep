# Shared fixtures for the gridded balance root that
# inst/scripts/run_nitrogen_balance.R writes (R/nbd_march.R) and the SJOS-N
# driver reads (R/sjos_n_run.R).

# The identity of a balance run: a clean tree at `sha`, grid resolution.
.nbd_march_test_identity <- function(
  sha = strrep("c", 40),
  regime = "yield_split"
) {
  whep:::.nbd_march_identity(
    list(sha = sha, clean = TRUE, diff_sha256 = NA_character_),
    list(resolution = "grid", regime = regime, skip_heavy = FALSE)
  )
}

# A warning whose message is NULL. .nbd_capture_conditions() records it as a
# zero-row condition -- it is lost from the capture -- while its handler still
# counts it: a lossy capture, as the balance run can actually produce one.
.nbd_unreadable_warning <- function() {
  warning(structure(
    class = c("warning", "condition"),
    list(message = NULL, call = NULL)
  ))
}

# A stage report as nbd_stage() builds it: each stage's expression run through
# .nbd_capture_conditions() and recorded with .nbd_stage_row(), including its
# own warning and message counts.
.nbd_march_test_report <- function(stages) {
  purrr::imap(stages, \(expr, label) {
    captured <- whep:::.nbd_capture_conditions(rlang::eval_tidy(expr))
    whep:::.nbd_stage_row(
      label,
      "ok",
      0.5,
      1L,
      NA_character_,
      captured$conditions,
      captured[c("warnings", "messages")]
    )
  }) |>
    unname() |>
    dplyr::bind_rows()
}

# The uniform-spread warning of the balance (.n_warn_unmatched()) for `crops`
# polity-crops carrying `n_t` tonnes in all.
.nbd_warn_uniform <- function(crops, n_t) {
  whep:::.n_warn_unmatched(tibble::tibble(
    n_t = rep(n_t / crops, crops),
    item_cbs_code = 2500L + seq_len(crops)
  ))
}
