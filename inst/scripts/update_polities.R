# Please read comments before running

# Download GeoJSON polities from SACO
"whep_polities" |>
  whep_read_file(type = "geojson") |>
  sf::write_sf("choose/some/path")

# Edit: simple text editor for just attributes or ArcGIS/QGIS for features
# This step is essentially manual work

# Upload new version to SACO
# Use scripts from whep-inputs repository

# Set new version in whep_inputs.csv (manually)
# Now update whep_inputs package data running this:
source("data-raw/whep_inputs.R")
devtools::load_all()

# Build new whep_polities package data:
source("data-raw/polities.R")
devtools::load_all()

# Run polities tests to make sure info is still consistent
testthat::test_file("tests/testthat/test_polities.R")
