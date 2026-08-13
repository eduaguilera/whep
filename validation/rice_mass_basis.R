# Real-data guard for the rice mass basis (#751).
#
# FAOSTAT publishes rice on two mass bases depending on vintage: the historic
# Food Balance Sheets carry item 2805 "Rice (Milled Equivalent)", while the new
# Food Balances carry item 2807 "Rice and products" in PADDY (rough-rice)
# equivalent. WHEP's contract for the item is milled equivalent throughout, so
# after `.fix_item_codes()` the two vintages must agree in the years where they
# overlap.
#
# That agreement is the check. When the conversion is missing or mis-keyed, the
# two vintages disagree by the extraction rate (median ratio 1.498 = 1/0.667
# before the fix, 1.004 after), and the CBS harmonisation silently rescales the
# whole historic series onto the wrong basis to reconcile them
# (`.clamp_fbs_scale_ratio()` passes anything in [0.2, 5]). Wheat is carried as
# a control: it uses one basis in both vintages, so its ratio must not move.
#
# This is not part of the test suite: it reads the `faostat-fbs-new` and
# `faostat-fbs-old` pins, which CI never fetches.
#
# Run:  Rscript validation/rice_mass_basis.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))

# The FBS_Old -> FBS_New harmonisation in .prepare_cbs_wide() takes its scaling
# ratio from this window, so it is the window that matters.
overlap_years <- 2010:2013
tolerance <- 0.05

cli::cli_h1(
  "Rice mass basis, FBS overlap {min(overlap_years)}-{max(overlap_years)}"
)

.vintage_ratio <- function(new, old, item_code) {
  key <- c("year", "area_code", "item_cbs_code", "element")
  new <- new[item_cbs_code == item_code, c(key, "value"), with = FALSE]
  old <- old[item_cbs_code == item_code, c(key, "value"), with = FALSE]
  data.table::setnames(new, "value", "fbs_new")
  data.table::setnames(old, "value", "fbs_old")
  both <- merge(new, old, by = key)
  both <- both[!is.na(fbs_new) & !is.na(fbs_old) & fbs_old != 0]
  list(
    median = stats::median(both$fbs_new / both$fbs_old),
    n = nrow(both)
  )
}

# Both pins are read once: together they are well over a gigabyte.
fbs_new <- data.table::setDT(
  whep:::.extract_cb("faostat-fbs-new", years = overlap_years)
)
fbs_old <- data.table::setDT(
  whep:::.extract_cb("faostat-fbs-old", years = overlap_years)
)

rice <- .vintage_ratio(fbs_new, fbs_old, 2807L)
wheat <- .vintage_ratio(fbs_new, fbs_old, 2511L)

cli::cli_inform(c(
  "*" = "rice  2807: median FBS_New/FBS_Old {round(rice$median, 4)}
         over {rice$n} row{?s}",
  "*" = "wheat 2511: median FBS_New/FBS_Old {round(wheat$median, 4)}
         over {wheat$n} row{?s} (control)"
))

# WHEP converts at a global 0.67 where FAO's own implied rate is 0.667, so the
# vintages agree to about 0.4%, not exactly.
if (abs(rice$median - 1) > tolerance) {
  cli::cli_abort(c(
    "Rice vintages disagree on mass basis: ratio {round(rice$median, 4)}.",
    "i" = "Expected within {tolerance} of 1 once new-FBS rice is converted to
           milled equivalent.",
    "i" = "A ratio near {round(1 / whep:::.rice_milled_extraction_rate(), 3)}
           means the paddy-to-milled conversion is not firing (#751)."
  ))
}

cli::cli_alert_success("Rice is on one mass basis across both FBS vintages.")
invisible(list(rice = rice, wheat = wheat))
