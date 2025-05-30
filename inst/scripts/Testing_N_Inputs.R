# Define input directory
inputs_dir <- "C:/PhD/GRAFS/Production Boxes/Final Files/Inputs"

# Load datasets
data <- .load_inputs(inputs_dir)

# Calculate N inputs and manure
n_inputs_prepared <- .calculate_n_inputs(data$N_balance_ygpit_all, data$Codes_coefs)

# Summarise inputs
n_inputs_summary <- .summarise_inputs(n_inputs_prepared)

# Summarise production
n_inputs_combined <- .summarise_production(data$GRAFS_Prod_Destiny, n_inputs_summary)

# Calculate NUE
nue <- .calculate_nue(n_inputs_combined)

# Create plots
.plot_n_inputs_production_cropland_semi_natural_agroecosystems(data$GRAFS_Prod_Destiny, n_inputs_combined)
.plot_nue_spain(nue)
.plot_n_inputs_production_cropland(data$GRAFS_Prod_Destiny, n_inputs_combined)
.plot_n_inputs_production_provinces(data$GRAFS_Prod_Destiny, n_inputs_combined)

# Return the loaded datasets
list(
  N_Inputs_combined = N_Inputs_combined,
  NUE = NUE
)
