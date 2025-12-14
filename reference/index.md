# Package index

## Tidy datasets

Get cleaned dataframes with project related data.

- [`build_supply_use()`](https://eduaguilera.github.io/whep/reference/build_supply_use.md)
  : Supply and use tables
- [`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md)
  : Bilateral trade data
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
- [`add_area_code()`](https://eduaguilera.github.io/whep/reference/add_area_code.md)
  : Get area codes from area names
- [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md)
  : Get area names from area codes

### Commodity balance sheet items

- [`items_cbs`](https://eduaguilera.github.io/whep/reference/items_cbs.md)
  : Commodity balance sheet items
- [`add_item_cbs_code()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_code.md)
  : Get commodity balance sheet item codes from item names
- [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md)
  : Get commodity balance sheet item names from item codes

### Primary production items

- [`items_prod`](https://eduaguilera.github.io/whep/reference/items_prod.md)
  : Primary production items
- [`add_item_prod_code()`](https://eduaguilera.github.io/whep/reference/add_item_prod_code.md)
  : Get production item codes from item names
- [`add_item_prod_name()`](https://eduaguilera.github.io/whep/reference/add_item_prod_name.md)
  : Get production item names from item codes

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

- [`linear_fill()`](https://eduaguilera.github.io/whep/reference/linear_fill.md)
  : Fill gaps by linear interpolation, or carrying forward or backward.
- [`proxy_fill()`](https://eduaguilera.github.io/whep/reference/proxy_fill.md)
  : Fill gaps using a proxy variable
- [`sum_fill()`](https://eduaguilera.github.io/whep/reference/sum_fill.md)
  : Fill gaps summing the previous value of a variable to the value of
  another variable.
