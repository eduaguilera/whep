# Package index

## Build datasets

Build full datasets from raw FAOSTAT inputs. These functions orchestrate
reading, gap-filling, historical extension, and balance validation.

- [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
  : Build primary production dataset
- [`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
  : Build commodity balance sheets
- [`build_processing_coefs()`](https://eduaguilera.github.io/whep/reference/build_processing_coefs.md)
  : Build processing coefficients

## Tidy datasets

Get cleaned dataframes with project related data.

### Production

Production, supply/use, and processing datasets.

- [`build_supply_use()`](https://eduaguilera.github.io/whep/reference/build_supply_use.md)
  : Supply and use tables
- [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md)
  : Livestock feed intake
- [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  : Primary items production
- [`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md)
  : Crop residue items
- [`get_processing_coefs()`](https://eduaguilera.github.io/whep/reference/get_processing_coefs.md)
  : Processed products share factors
- [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  : Commodity balance sheet data

### Trade

Bilateral trade datasets.

- [`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md)
  : Bilateral trade data

### Nitrogen cycles

Nitrogen inputs, outputs, production, and destinies for Spain.

- [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md)
  : GRAFS Nitrogen (N) flows
- [`create_n_soil_inputs()`](https://eduaguilera.github.io/whep/reference/create_n_soil_inputs.md)
  : Nitrogen (N) soil inputs for Spain
- [`create_n_production()`](https://eduaguilera.github.io/whep/reference/create_n_production.md)
  : N production for Spain
- [`calculate_nue_crops()`](https://eduaguilera.github.io/whep/reference/calculate_nue_crops.md)
  : N soil inputs and Nitrogen Use Efficiency (NUE) for crop
- [`calculate_nue_livestock()`](https://eduaguilera.github.io/whep/reference/calculate_nue_livestock.md)
  : NUE for Livestock
- [`calculate_system_nue()`](https://eduaguilera.github.io/whep/reference/calculate_system_nue.md)
  : System NUE
- [`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md)
  : GRAFS Nitrogen (N) flows at Spain national level

## Download large input datasets

Fetch large files from external sources and cache them.

- [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
  : Download, cache and read files
- [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  : External inputs
- [`whep_list_file_versions()`](https://eduaguilera.github.io/whep/reference/whep_list_file_versions.md)
  : Input file versions

## Code namings

Add name columns in dataframes from their codes or viceversa.

### Polities

- [`polities`](https://eduaguilera.github.io/whep/reference/polities.md)
  : Polities
- [`polities_cats`](https://eduaguilera.github.io/whep/reference/polities_cats.md)
  : Polity categories and regional classifications
- [`regions_full`](https://eduaguilera.github.io/whep/reference/regions_full.md)
  : Full polity and region reference table
- [`add_area_code()`](https://eduaguilera.github.io/whep/reference/add_area_code.md)
  : Get area codes from area names
- [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md)
  : Get area names from area codes

### Commodity balance sheet items

- [`items_cbs`](https://eduaguilera.github.io/whep/reference/items_cbs.md)
  : Commodity balance sheet items
- [`items_full`](https://eduaguilera.github.io/whep/reference/items_full.md)
  : Full CBS item table
- [`cbs_trade_codes`](https://eduaguilera.github.io/whep/reference/cbs_trade_codes.md)
  : CBS to trade item code mapping
- [`add_item_cbs_code()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_code.md)
  : Get commodity balance sheet item codes from item names
- [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md)
  : Get commodity balance sheet item names from item codes

### Primary production items

- [`items_prod`](https://eduaguilera.github.io/whep/reference/items_prod.md)
  : Primary production items
- [`items_prim`](https://eduaguilera.github.io/whep/reference/items_prim.md)
  : Primary production items linked to CBS
- [`items_prod_full`](https://eduaguilera.github.io/whep/reference/items_prod_full.md)
  : Full production item table
- [`add_item_prod_code()`](https://eduaguilera.github.io/whep/reference/add_item_prod_code.md)
  : Get production item codes from item names
- [`add_item_prod_name()`](https://eduaguilera.github.io/whep/reference/add_item_prod_name.md)
  : Get production item names from item codes

## Reference tables

Lookup and coefficient tables used internally across the pipeline.

- [`animals_codes`](https://eduaguilera.github.io/whep/reference/animals_codes.md)
  : Animal codes and classifications
- [`biomass_coefs`](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
  : Biomass coefficients for crops and livestock products
- [`cb_processing`](https://eduaguilera.github.io/whep/reference/cb_processing.md)
  : Commodity balance sheet processing fractions
- [`crops_eurostat`](https://eduaguilera.github.io/whep/reference/crops_eurostat.md)
  : Eurostat crop classification codes
- [`liv_lu_coefs`](https://eduaguilera.github.io/whep/reference/liv_lu_coefs.md)
  : Livestock unit coefficients
- [`primary_double`](https://eduaguilera.github.io/whep/reference/primary_double.md)
  : Items with double-counting in production statistics

## FAOSTAT raw data

Download FAOSTAT data as is.

- [`get_faostat_data()`](https://eduaguilera.github.io/whep/reference/get_faostat_data.md)
  : Scrapes activity_data from FAOSTAT and slightly post-processes it

## Data sources

Get a tidy dataframe with the found sources for different data.

- [`expand_trade_sources()`](https://eduaguilera.github.io/whep/reference/expand_trade_sources.md)
  : Trade data sources

## Gap filling functions

Functions to fill gaps (NA values) in time-dependent variables using
different methods.

- [`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md)
  : Fill gaps by linear interpolation, or carrying forward or backward.
- [`fill_sum()`](https://eduaguilera.github.io/whep/reference/fill_sum.md)
  : Fill gaps summing the previous value of a variable to the value of
  another variable.
- [`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)
  : Fill gaps using growth rates from proxy variables

## Decomposition analysis

Functions for index decomposition analysis.

- [`calculate_lmdi()`](https://eduaguilera.github.io/whep/reference/calculate_lmdi.md)
  : Calculate LMDI decomposition.

## Harmonization

Functions to harmonize time series items according to specified mapping.

- [`harmonize_simple()`](https://eduaguilera.github.io/whep/reference/harmonize_simple.md)
  : Harmonize rows labeled "simple" by summing values
- [`harmonize_interpolate()`](https://eduaguilera.github.io/whep/reference/harmonize_interpolate.md)
  : Harmonize advanced cases with interpolation for 1:N groups
