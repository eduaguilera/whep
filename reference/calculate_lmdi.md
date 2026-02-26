# Calculate LMDI decomposition.

Performs LMDI (Log Mean Divisia Index) decomposition analysis with
flexible identity parsing, automatic factor detection, and support for
multiple periods and groupings. Supports sectoral decomposition using
bracket notation for both summing and grouping operations.

## Usage

``` r
calculate_lmdi(
  data,
  identity,
  identity_labels = NULL,
  time_var = year,
  periods = NULL,
  periods_2 = NULL,
  .by = NULL,
  rolling_mean = 1,
  output_format = "clean",
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame containing the variables for decomposition. Must include
  all variables specified in the identity, time variable, and any
  grouping variables.

- identity:

  Character. Decomposition identity in format
  `"target:factor1*factor2*..."`. The target appears before the colon,
  factors after, separated by asterisks. Supports explicit ratios with
  `/` and structural decomposition with `[]`.

- identity_labels:

  Named character vector. Custom labels for factors to use in output
  instead of variable names. Default: NULL uses variable names as-is.

- time_var:

  Unquoted name of the time variable column in the data. Default:
  `year`. Must be numeric or coercible to numeric.

- periods:

  Numeric vector. Years defining analysis periods. Each consecutive pair
  defines one period. Default: NULL uses all available years.

- periods_2:

  Numeric vector. Additional period specification for complex
  multi-period analyses. Default: NULL.

- .by:

  Character vector. Grouping variables for performing separate
  decompositions. Default: NULL (single decomposition for all data).

- rolling_mean:

  Numeric. Window size for rolling mean smoothing applied before
  decomposition. Default: 1 (no smoothing).

- output_format:

  Character. Format of output data frame. Options: `"clean"` (default)
  or `"detailed"`.

- verbose:

  Logical. If TRUE (default), prints progress messages during
  decomposition.

## Value

A tibble with LMDI decomposition results containing:

- Time variables and grouping variables (if specified).

- `additive`: Additive contributions (sum equals total change in
  target).

- `multiplicative`: Multiplicative indices (product equals target
  ratio).

- `multiplicative_log`: Log of multiplicative indices.

- Period identifiers and metadata.

## Details

The LMDI method decomposes changes in a target variable into
contributions from multiple factors using logarithmic mean weights. This
implementation supports:

**Flexible identity specification:**

- Automatic factor detection from identity string.

- Support for ratio calculations (implicit division).

- Sectoral aggregation with `[]` notation.

- Sectoral grouping with [`{}`](https://rdrr.io/r/base/Paren.html)
  notation.

**Period analysis:** The function can decompose changes over single or
multiple periods. Periods are defined by consecutive pairs in the
`periods` vector.

**Grouping capabilities:** Use `.by` to perform separate decompositions
for different groups (e.g., countries, regions) while maintaining
consistent factor structure.

## Identity Syntax

The identity parameter uses a special syntax to define decomposition:

**Basic format:** `"target:factor1*factor2*factor3"`

**Simple decomposition (no sectors):**

- Basic: `"emissions:gdp*(emissions/gdp)"`

- Complete: `"emissions:(emissions/gdp)*(gdp/population)*population"`

**Understanding bracket notation:**

Square brackets `[]` specify variables to sum across categories,
enabling structural decomposition. The bracket aggregates values BEFORE
calculating ratios.

**Single-level structural decomposition:**

- `"emissions:activity*(activity[sector]/activity)*(emissions[sector]/activity[sector])"`

- Creates 3 factors: Activity level, Sectoral structure, Sectoral
  intensity.

**Multi-level structural decomposition:**

- Two levels:
  `"emissions:activity*(activity[sector]/activity)*(activity[sector+fuel]/activity[sector])*(emissions[sector+fuel]/activity[sector+fuel])"`

- Creates 4 factors: Activity level, Sector structure, Fuel structure,
  Sectoral-fuel intensity.

## Data Requirements

The input data frame must contain:

- All variables mentioned in the identity.

- The time variable (default: "year").

- Grouping variables if using `.by`.

- No missing values in key variables for decomposition periods.

## Examples

``` r
# Note: In these examples, 'activity' is a measure of scale (e.g., GDP in million USD, production in tonnes, or population in millions),
# and 'intensity' is the target variable per unit activity (e.g., emissions per million USD, emissions per tonne, etc.).
# The units are illustrative; adapt to your context.
# --- Shared sample data ---
data_simple <- tibble::tribble(
  ~year, ~activity, ~intensity, ~emissions,
  2010,  1000,      0.10,       100,   # e.g., 1000 million USD, 0.10 tCO2/million USD, 100 tCO2
  2011,  1100,      0.12,       132,
  2012,  1200,      0.09,       108,
  2013,  1300,      0.10,       130
)

# --- 1. Year-over-year decomposition (default) ---
# Decompose annual emission changes into activity and intensity effects.
# The additive column sums to the total change in emissions each period.
calculate_lmdi(
  data_simple,
  identity = "emissions:activity*intensity",
  time_var = year,
  verbose = FALSE
) |>
  dplyr::select(
    period,
    component_type,
    factor_label,
    additive,
    multiplicative
  )
#> # A tibble: 9 × 5
#>   period    component_type factor_label additive multiplicative
#>   <chr>     <chr>          <chr>           <dbl>          <dbl>
#> 1 2010-2011 factor         activity        11.0           1.1  
#> 2 2010-2011 factor         intensity       21.0           1.2  
#> 3 2010-2011 target         emissions       32             1.32 
#> 4 2011-2012 factor         activity        10.4           1.09 
#> 5 2011-2012 factor         intensity      -34.4           0.75 
#> 6 2011-2012 target         emissions      -24             0.818
#> 7 2012-2013 factor         activity         9.50          1.08 
#> 8 2012-2013 factor         intensity       12.5           1.11 
#> 9 2012-2013 target         emissions       22             1.20 

# --- 2. Single baseline-to-end period ---
# Pass a two-element periods vector to get a single cumulative period
# instead of year-over-year results.
calculate_lmdi(
  data_simple,
  identity = "emissions:activity*intensity",
  time_var = year,
  periods = c(2010, 2013),
  verbose = FALSE
) |>
  dplyr::select(
    period,
    component_type,
    factor_label,
    additive,
    multiplicative
  )
#> # A tibble: 3 × 5
#>   period    component_type factor_label additive multiplicative
#>   <chr>     <chr>          <chr>           <dbl>          <dbl>
#> 1 2010-2013 factor         activity           30            1.3
#> 2 2010-2013 factor         intensity           0            1  
#> 3 2010-2013 target         emissions          30            1.3

# --- 3. Year-over-year AND one cumulative summary period ---
# Use periods_2 to append an extra comparison period alongside the
# year-over-year results.
calculate_lmdi(
  data_simple,
  identity = "emissions:activity*intensity",
  time_var = year,
  periods = c(2010, 2011, 2012, 2013),
  periods_2 = c(2010, 2013),
  verbose = FALSE
) |>
  dplyr::select(
    period,
    component_type,
    factor_label,
    additive,
    multiplicative
  )
#> # A tibble: 12 × 5
#>    period    component_type factor_label additive multiplicative
#>    <chr>     <chr>          <chr>           <dbl>          <dbl>
#>  1 2010-2011 factor         activity        11.0           1.1  
#>  2 2010-2011 factor         intensity       21.0           1.2  
#>  3 2010-2011 target         emissions       32             1.32 
#>  4 2011-2012 factor         activity        10.4           1.09 
#>  5 2011-2012 factor         intensity      -34.4           0.75 
#>  6 2011-2012 target         emissions      -24             0.818
#>  7 2012-2013 factor         activity         9.50          1.08 
#>  8 2012-2013 factor         intensity       12.5           1.11 
#>  9 2012-2013 target         emissions       22             1.20 
#> 10 2010-2013 factor         activity        30             1.3  
#> 11 2010-2013 factor         intensity        0             1    
#> 12 2010-2013 target         emissions       30             1.3  

# --- 4. Per-country decomposition with .by ---
# Separate LMDI runs per country; results are stacked with a country column.
data_countries <- tibble::tribble(
  ~year, ~country, ~activity, ~intensity, ~emissions,
  2010, "ESP", 1000, 0.10, 100,
  2011, "ESP", 1100, 0.11, 121,
  2012, "ESP", 1200, 0.10, 120,
  2010, "FRA", 2000, 0.05, 100,
  2011, "FRA", 2200, 0.05, 110,
  2012, "FRA", 2400, 0.05, 120
)

calculate_lmdi(
  data_countries,
  identity = "emissions:activity*intensity",
  time_var = year,
  .by = "country",
  verbose = FALSE
) |>
  dplyr::select(
    country,
    period,
    component_type,
    factor_label,
    additive,
    multiplicative
  )
#> # A tibble: 12 × 6
#>    country period    component_type factor_label additive multiplicative
#>    <chr>   <chr>     <chr>          <chr>           <dbl>          <dbl>
#>  1 ESP     2010-2011 factor         activity         10.5          1.1  
#>  2 ESP     2010-2011 factor         intensity        10.5          1.1  
#>  3 ESP     2010-2011 target         emissions        21            1.21 
#>  4 ESP     2011-2012 factor         activity         10.5          1.09 
#>  5 ESP     2011-2012 factor         intensity       -11.5          0.909
#>  6 ESP     2011-2012 target         emissions        -1            0.992
#>  7 FRA     2010-2011 factor         activity         10            1.1  
#>  8 FRA     2010-2011 factor         intensity         0            1    
#>  9 FRA     2010-2011 target         emissions        10            1.1  
#> 10 FRA     2011-2012 factor         activity         10            1.09 
#> 11 FRA     2011-2012 factor         intensity         0            1    
#> 12 FRA     2011-2012 target         emissions        10            1.09 

# --- 5. Ratio notation ---
# Express factors as explicit ratios (e.g. intensity = emissions/activity).
# Factor labels in the output preserve the ratio form for clarity.
calculate_lmdi(
  data_simple,
  identity = "emissions:(emissions/activity)*activity",
  time_var = year,
  verbose = FALSE
) |>
  dplyr::select(
    period,
    component_type,
    factor_label,
    additive,
    multiplicative
  )
#> # A tibble: 9 × 5
#>   period    component_type factor_label       additive multiplicative
#>   <chr>     <chr>          <chr>                 <dbl>          <dbl>
#> 1 2010-2011 factor         emissions/activity    21.0           1.2  
#> 2 2010-2011 factor         activity              11.0           1.1  
#> 3 2010-2011 target         emissions             32             1.32 
#> 4 2011-2012 factor         emissions/activity   -34.4           0.75 
#> 5 2011-2012 factor         activity              10.4           1.09 
#> 6 2011-2012 target         emissions            -24             0.818
#> 7 2012-2013 factor         emissions/activity    12.5           1.11 
#> 8 2012-2013 factor         activity               9.50          1.08 
#> 9 2012-2013 target         emissions             22             1.20 

# --- 6. Structural (sectoral) decomposition with [] notation ---
# Decomposes emissions into:
#   total_activity * sector_structure * sector_intensity
# [] sums the bracketed variable across sector before forming ratios,
# enabling proper structural decomposition.
data_sectors <- tibble::tribble(
  ~year, ~sector,      ~activity, ~emissions,
  2010, "industry",   600,        60,
  2010, "transport",  400,        40,
  2011, "industry",   700,        63,
  2011, "transport",  500,        55
) |>
  dplyr::group_by(year) |>
  dplyr::mutate(total_activity = sum(activity)) |>
  dplyr::ungroup()

calculate_lmdi(
  data_sectors,
  identity = paste0(
    "emissions:",
    "total_activity*",
    "(activity[sector]/total_activity)*",
    "(emissions[sector]/activity[sector])"
  ),
  time_var = year,
  verbose = FALSE
) |>
  dplyr::select(
    period,
    component_type,
    factor_label,
    additive,
    multiplicative
  )
#> # A tibble: 4 × 5
#>   period    component_type factor_label                  additive multiplicative
#>   <chr>     <chr>          <chr>                            <dbl>          <dbl>
#> 1 2010-2011 factor         total_activity                  19.8            1.20 
#> 2 2010-2011 factor         activity[sector]/total_activ…    0.191          1.00 
#> 3 2010-2011 factor         emissions[sector]/activity[s…   -1.99           0.982
#> 4 2010-2011 target         emissions                       18              1.18 

# --- 7. Custom factor labels ---
# Replace raw variable names with readable labels for reporting.
# Supply one label per term (target first, then each factor in order).
calculate_lmdi(
  data_simple,
  identity = "emissions:activity*intensity",
  identity_labels = c(
    "Total Emissions",
    "Activity Effect",
    "Intensity Effect"
  ),
  time_var = year,
  verbose = FALSE
) |>
  dplyr::select(
    period,
    component_type,
    factor_label,
    additive,
    multiplicative
  )
#> # A tibble: 9 × 5
#>   period    component_type factor_label     additive multiplicative
#>   <chr>     <chr>          <chr>               <dbl>          <dbl>
#> 1 2010-2011 factor         Activity Effect     11.0           1.1  
#> 2 2010-2011 factor         Intensity Effect    21.0           1.2  
#> 3 2010-2011 target         Total Emissions     32             1.32 
#> 4 2011-2012 factor         Activity Effect     10.4           1.09 
#> 5 2011-2012 factor         Intensity Effect   -34.4           0.75 
#> 6 2011-2012 target         Total Emissions    -24             0.818
#> 7 2012-2013 factor         Activity Effect      9.50          1.08 
#> 8 2012-2013 factor         Intensity Effect    12.5           1.11 
#> 9 2012-2013 target         Total Emissions     22             1.20 

# --- 8. Rolling mean smoothing before decomposition ---
# A 3-year rolling mean reduces noise in volatile series before
# computing LMDI weights. Edge years use partial windows (fewer
# than k observations) so no periods are lost.
data_smooth <- tibble::tibble(
  year      = 2010:2020,
  activity  = seq(1000, 2000, length.out = 11),
  intensity = rep(0.1, 11),
  emissions = seq(1000, 2000, length.out = 11) * 0.1
)

calculate_lmdi(
  data_smooth,
  identity = "emissions:activity*intensity",
  time_var = year,
  rolling_mean = 3,
  verbose = FALSE
) |>
  dplyr::select(
    period,
    component_type,
    factor_label,
    additive,
    multiplicative
  )
#> # A tibble: 30 × 5
#>    period    component_type factor_label additive multiplicative
#>    <chr>     <chr>          <chr>           <dbl>          <dbl>
#>  1 2010-2011 factor         activity            5           1.05
#>  2 2010-2011 factor         intensity           0           1   
#>  3 2010-2011 target         emissions           5           1.05
#>  4 2011-2012 factor         activity           10           1.09
#>  5 2011-2012 factor         intensity           0           1   
#>  6 2011-2012 target         emissions          10           1.09
#>  7 2012-2013 factor         activity           10           1.08
#>  8 2012-2013 factor         intensity           0           1   
#>  9 2012-2013 target         emissions          10           1.08
#> 10 2013-2014 factor         activity           10           1.08
#> # ℹ 20 more rows
```
