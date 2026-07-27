# Meisinger and Randall topsoil denitrification share matrix.

Fraction of nitrogen surplus lost through topsoil denitrification, as a
function of fertiliser category, soil organic matter content, drainage
class and climate. The full matrix is keyed by fertiliser category
(synthetic versus manure), tillage regime, soil organic matter class,
climate category and drainage class. Where drainage is `"None"`
(waterlogged), the entire surplus is denitrified (share 1.00).

## Usage

``` r
meisinger_denitrification
```

## Format

A tibble with columns:

- fert_cat:

  Fertiliser category: `"Synthetic"` or `"Manure"`.

- tillage:

  Tillage regime: `"Tillage"`, `"No_tillage"` or `"Not_specified"`
  (manure).

- som_content:

  Soil organic matter class: `"Low"`, `"Medium"` or `"High"`.

- climate_cat:

  Climate category label used in the source matrix: `"Semiarid"` or
  `"Humid"`.

- drainage_rate:

  Drainage class: `"Very_high"`, `"High"`, `"Medium"`, `"Low"`,
  `"Very_low"` or `"None"`.

- denit_share:

  Fraction of nitrogen surplus denitrified in the topsoil.

- climate:

  Climate zone the row applies to: `"MED"` or `"ATL"`.

## Source

Meisinger, J. J. & Randall, G. W. (1991). Estimating nitrogen budgets
for soil-crop systems. In R. F. Follett, D. R. Keeney & R. M. Cruse
(Eds.), *Managing Nitrogen for Groundwater Quality and Farm
Profitability* (pp. 85-124). Soil Science Society of America.
[doi:10.2136/1991.managingnitrogen.c5](https://doi.org/10.2136/1991.managingnitrogen.c5)
. Values transcribed from the Spain historical nitrogen coefficient
workbook (`N_coefficients.xlsx`, sheet `Denitrification_Meisinger`).

## Examples

``` r
meisinger_denitrification
#> # A tibble: 108 × 7
#>    fert_cat  tillage   som_content climate_cat drainage_rate denit_share climate
#>    <chr>     <chr>     <chr>       <chr>       <chr>               <dbl> <chr>  
#>  1 Synthetic Tillage   Low         Semiarid    Very_high            0.02 MED    
#>  2 Synthetic Tillage   Medium      Semiarid    Very_high            0.03 MED    
#>  3 Synthetic Tillage   High        Semiarid    Very_high            0.04 MED    
#>  4 Synthetic No_tilla… Low         Semiarid    Very_high            0.03 MED    
#>  5 Synthetic No_tilla… Medium      Semiarid    Very_high            0.04 MED    
#>  6 Synthetic No_tilla… High        Semiarid    Very_high            0.06 MED    
#>  7 Manure    Not_spec… Low         Semiarid    Very_high            0.04 MED    
#>  8 Manure    Not_spec… Medium      Semiarid    Very_high            0.06 MED    
#>  9 Manure    Not_spec… High        Semiarid    Very_high            0.08 MED    
#> 10 Synthetic Tillage   Low         Semiarid    High                 0.03 MED    
#> # ℹ 98 more rows
```
