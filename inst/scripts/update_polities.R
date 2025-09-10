# Please read comments before running

# Manual update steps

# (optional) If you need to forcefully write year ranges, add manual entry to
# the file data-raw/polities_inputs/whep_fixes.csv

# Add or update entries in data-raw/polities_inputs/common_names.csv.
# If you did previous step, also make sure to add entry for whep source.

# Add or update entries in data-raw/polities_inputs/polity_codes.csv

# Update package constants with your changes
source("data-raw/constants.R")

# Load code changes
devtools::load_all()

# Try getting the polities table to make sure it doesn't error
get_polities()

# Run polities tests to make sure info is still consistent
testthat::test_file("tests/testthat/test_polities.R")
