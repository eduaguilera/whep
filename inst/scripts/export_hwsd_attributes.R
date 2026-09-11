# -----------------------------------------------------------------------
# export_hwsd_attributes.R
#
# Exports the HWSD (Harmonized World Soil Database) map-unit x
# texture-class attribute table from a local HWSD.SQLite database to the
# hwsd_data.csv that whep::read_soil_ph() expects (see
# .read_hwsd_attributes_local() in R/soil_ph.R).
#
# This is a ONE-OFF, real-data-dependent script. It is NOT sourced by any
# build pipeline and NOT run during R CMD check or CI: it needs a local
# WHEP_HWSD_DIR holding HWSD.SQLite (real HWSD attribute database), which
# is not available in the package sandbox. Run this manually whenever
# hwsd_data.csv needs to be (re)generated from the source database.
#
# SOURCE SCHEMA: HWSD.SQLite's "hwsd_data" table holds one row per HWSD
# soil map unit (mu_global) x texture-class share, with a topsoil column
# named t_usda_tex_class. whep::read_soil_ph() expects that same column
# under the name t_usda_tex (matching the HWSD2 SQLite schema used by
# inst/scripts/download/download_hwsd.R), so it is renamed on export.
#
# t_oc, t_bulk_density, t_ref_bulk_density and t_gravel are exported for
# read_hwsd_topsoil_soc(), the observed 0-30 cm carbon benchmark. They sit
# in this same table one column across from t_clay, which is why WHEP could
# read the clay driving its carbon model while having no observed carbon to
# check that model against.
## t_clay (HWSD field T_CLAY, "Topsoil Clay Fraction, % wt.") is exported
# too: the soil-carbon per-cell clay driver reads it (.cb_hwsd_clay() in
# R/carbon_balance.R), and an extract written without it made that reader
# fail with a dplyr missing-column error (whep#596). The whole column set
# the readers require is named in R/soil_ph.R (.hwsd_ph_columns(),
# .hwsd_texture_columns(), .hwsd_clay_columns()); keep this query a
# superset of it.
#
# Reference:
# t_sand and t_silt are exported so this producer emits the same texture
# columns as download_hwsd.R. Century's texture terms need silt (fTxtr and Es
# are both functions of clay + silt) and shipped a placeholder 45% because
# silt was reachable only on the other path (whep#345).
#
#   FAO/IIASA/ISRIC/ISSCAS/JRC (2012) "Harmonized World Soil Database
#   version 1.2" -- attribute database field list, T_USDA_TEX_CLASS,
#   T_PH_H2O and T_CLAY (Topsoil Clay Fraction, % wt.).

hwsd_dir <- Sys.getenv("WHEP_HWSD_DIR")
if (!nzchar(hwsd_dir)) {
  stop(
    "Set WHEP_HWSD_DIR to a local directory holding HWSD.SQLite before ",
    "running this script."
  )
}

sqlite_path <- file.path(hwsd_dir, "HWSD.SQLite")
if (!file.exists(sqlite_path)) {
  stop("HWSD.SQLite not found at: ", sqlite_path)
}

db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
hwsd_attr <- DBI::dbGetQuery(
  db,
  "SELECT mu_global, t_usda_tex_class AS t_usda_tex, share, t_clay, t_ph_h2o,
          t_sand, t_silt, t_oc, t_bulk_density, t_ref_bulk_density,
          t_gravel
   FROM hwsd_data"
)
DBI::dbDisconnect(db)

hwsd_attr <- tibble::as_tibble(hwsd_attr)

# Stamp the provenance INTO the artifact. Two scripts write this filename --
# this one from HWSD v1.2, whose topsoil is 0-30 cm, and
# inst/scripts/download/download_hwsd.R from HWSD2, whose D1 layer is
# 0-20 cm. Same path, same name, different depth, and until now nothing in
# the file said which had run. Every HWSD reader silently got whichever was
# last (whep#851).
hwsd_attr$source <- "hwsd_v1.2"
hwsd_attr$topsoil_depth_cm <- 30

readr::write_csv(hwsd_attr, file.path(hwsd_dir, "hwsd_data.csv"))
cli::cli_alert_success("hwsd_data.csv: {nrow(hwsd_attr)} rows")
