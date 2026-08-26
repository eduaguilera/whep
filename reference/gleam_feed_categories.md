# GLEAM feed categories.

Feed classification used in GLEAM 3.0.

## Usage

``` r
gleam_feed_categories
```

## Format

A tibble with `feed_category`, `feed_type`, `description`.

## Source

Not traced to a GLEAM document. These values are hardcoded in
`generate_gleam_pdf_tables()` in `data-raw/livestock_coefficients.R`,
not read from the GLEAM 3.0 Supplement S1 workbook, and no table of that
workbook contains them. The attribution to MacLeod et al. (2018) they
carried was wrong: that is the *Animal* position paper on GLEAM
([doi:10.1017/S1751731117001847](https://doi.org/10.1017/S1751731117001847)
), which publishes no such table. Treat the values as unverified
placeholders; tracked in whep#881.

## Examples

``` r
gleam_feed_categories
#> # A tibble: 6 × 3
#>   feed_category   feed_type  description             
#>   <chr>           <chr>      <chr>                   
#> 1 Grass           Pasture    Grazed grass and fodder 
#> 2 Crop residues   Residues   Straw, stovers, husks   
#> 3 Concentrates    Crops      Grains, oilseeds, pulses
#> 4 Fodder crops    Crops      Cultivated fodder       
#> 5 Processed feeds Industrial Brans, meals, cakes     
#> 6 Animal products Animal     Milk, fish meal         
```
