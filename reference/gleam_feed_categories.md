# GLEAM feed categories.

Feed classification used in GLEAM 3.0.

## Usage

``` r
gleam_feed_categories
```

## Format

A tibble with `feed_category`, `feed_type`, `description`.

## Source

MacLeod et al. (2018) GLEAM 3.0.

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
