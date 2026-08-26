# GLEAM feed categories.

Feed classification used in GLEAM 3.0.

## Usage

``` r
gleam_feed_categories
```

## Format

A tibble with `feed_category`, `feed_type`, `description`.

## Source

Not traced to any GLEAM document (whep#881). Searched and ruled out: the
GLEAM 3.0 Supplement S1 workbook (no sheet holds it); FAO. 2022. *Model
Description, Version 3.0*, Table 3.1/3.3 (ruminants, 27 feed materials
grouped as Roughages / Cereals / By-products / Concentrates, pp. 32-37)
and Table 3.5 (monogastrics, 42 materials grouped as Swill and
scavenging / Locally-produced / Non-local, p. 45); and Tables 3.2, 3.3
and 3.14 of the Version 2.0 Revision 5 description. None of those
groupings is the six-way Grass / Crop residues / Concentrates / Fodder
crops / Processed feeds / Animal products split shipped here, and
"Animal products - Milk, fish meal" has no counterpart in GLEAM's
ruminant material list at all. Treat the classification as a WHEP-local
convenience taxonomy, not a GLEAM table. No function in `R/` reads it.

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
