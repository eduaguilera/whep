# Test harness for Section 10 (crop spatialization) only.
#
# Requires that all Section 1–9 outputs already exist in
#   <l_files_dir>/whep/inputs/
#
# Usage:
#   Rscript autoresearch/prepare_spatialize/test_crop_spatialize.R

l_files_dir <- "LPJmL_inputs"

# -----------------------------------------------------------------------
# Load functions
# -----------------------------------------------------------------------
source("inst/scripts/prepare_spatialize_all.R")

year_range  <- 2000:2010  # change to whatever you want to test

run_dir    <- file.path(l_files_dir, "whep")
input_dir  <- file.path(l_files_dir, "whep", "inputs")

if (!dir.exists(run_dir))   dir.create(run_dir, recursive = TRUE)
if (!dir.exists(input_dir)) stop("input_dir not found: ", input_dir)

# -----------------------------------------------------------------------
# Run only Section 10
# -----------------------------------------------------------------------
gc(reset = TRUE)
run_crop_spatialize(run_dir, input_dir, year_range)
