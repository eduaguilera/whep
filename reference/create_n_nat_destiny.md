# GRAFS Nitrogen (N) flows at Spain national level

Provides N flows of the Spanish agro-food system on a national level
between 1860 and 2020. This dataset is the national equivalent of the
provincial GRAFS model and represents Spain as a single system without
internal trade between provinces. All production, consumption and soil
inputs are aggregated nationally before calculating trade with the
outside.

## Usage

``` r
create_n_nat_destiny(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A final tibble containing national N flow data by origin and destiny. It
includes the following columns:

- `year`: The year in which the recorded event occurred.

- `item`: The item which was produced, defined in `names_biomass_cb`.

- `irrig_cat`: Irrigation form (irrigated or rainfed)

- `box`: One of the GRAFS model systems: cropland, Semi-natural
  agroecosystems, Livestock, Fish, or Agro-industry.

- `origin`: The origin category of N: Cropland, Semi-natural
  agroecosystems, Livestock, Fish, Agro-industry, Deposition, Fixation,
  Synthetic, People (waste water), Livestock (manure).

- `destiny`: The destiny category of N: population_food,
  population_other_uses, livestock_mono, livestock_rum (feed), export,
  Cropland (for N soil inputs).

- `mg_n`: Nitrogen amount in megagrams (Mg).

- `province_name`: Set to "Spain" for all national-level rows.

## Examples

``` r
create_n_nat_destiny(example = TRUE)
#> # A tibble: 10 × 8
#>     year item                irrig_cat box   origin destiny   mg_n province_name
#>    <dbl> <chr>               <chr>     <chr> <chr>  <chr>    <dbl> <chr>        
#>  1  1863 Hard Fibres, Other  Irrigated NA    Lives… Cropla… 5.21e1 Spain        
#>  2  2012 Nuts and products   Irrigated NA    People Cropla… 2.12e2 Spain        
#>  3  1955 Pulses, Other and … Irrigated Crop… Cropl… export  5.02e2 Spain        
#>  4  1976 Sorghum and produc… Irrigated Crop… Cropl… popula… 2.47e1 Spain        
#>  5  1901 Pulses, Other and … Irrigated Crop… Cropl… livest… 3.54e1 Spain        
#>  6  1922 Nuts and products   Rainfed   NA    Fixat… Cropla… 2.48e3 Spain        
#>  7  1993 Oats                Irrigated NA    People Cropla… 5.65e0 Spain        
#>  8  1874 Fodder mix          Rainfed   Crop… Cropl… livest… 6.05e2 Spain        
#>  9  1983 Oranges, Mandarines Irrigated NA    Lives… Cropla… 5.29e3 Spain        
#> 10  1997 Barley and products Rainfed   Crop… Cropl… livest… 1.75e4 Spain        
```
