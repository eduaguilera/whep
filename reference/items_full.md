# Full CBS item table

Extended item reference table covering all CBS items, including their
process and commodity codes, feed type classifications, and default
material flow destinations.

## Usage

``` r
items_full
```

## Format

A tibble where each row corresponds to one CBS item. It contains the
following columns:

- `item_cbs`: Name of the CBS item.

- `item_cbs_code`: Numeric CBS item code.

- `comm_code`: Commodity code used in process-based modelling (may
  contain `"#N/A"` when not applicable).

- `proc_code`: Process code (may contain `"#N/A"` when not applicable).

- `proc`: Process name (may contain `"#N/A"` when not applicable).

- `unit`: Measurement unit (typically `"tonnes"`).

- `group`: Broad item group. Common values include `"Additives"`,
  `"Crop products"`, `"Crop residues"`, `"Draught"`, `"Fish"`,
  `"Forestry"`, `"Grass"`, `"Livestock"`, and others.

- `feedtype_graniv`: Feed type classification for granivores (e.g.,
  `"additives"`, `"concentrates"`, `"roughages"`).

- `feedtype_grazers`: Feed type classification for grazers.

- `comm_group`: Sub-group of the commodity (e.g., `"Additives"`,
  `"Alcohol"`, `"Ethanol"`, `"Oil cakes"`,
  `"Other processing residues"`).

- `Cat_1`: Primary category label used in material flow accounting.

- `Name_biomass`: Corresponding item name in `biomass_coefs`, enabling
  joins with the biomass coefficient table.

- `dbMFA_items`: Item identifier used in the material flow analysis
  database.

- `FEDNA`: Item name used in FEDNA feed composition tables.

- `default_destiny`: Default CBS use category for this item. One of
  `"Feed"`, `"Food"`, `"Other_uses"`, `"Processing"`, or `NA`.

## Source

Derived from [FAOSTAT data](https://www.fao.org/faostat/en/#data/FBS)
and internal commodity classification work.

## Examples

``` r
head(items_full)
#> # A tibble: 6 × 15
#>   item_cbs   item_cbs_code comm_code proc_code proc  unit  group feedtype_graniv
#>   <chr>              <dbl> <chr>     <chr>     <chr> <chr> <chr> <chr>          
#> 1 Choline c…          4007 NA        NA        NA    tonn… Addi… additives      
#> 2 Lysine              4010 NA        NA        NA    tonn… Addi… additives      
#> 3 MgO                 4012 NA        NA        NA    tonn… Addi… additives      
#> 4 Minerals            4013 NA        NA        NA    tonn… Addi… additives      
#> 5 Pesticides          4015 NA        NA        NA    tonn… Addi… additives      
#> 6 SSP                 4021 NA        NA        NA    tonn… Addi… additives      
#> # ℹ 7 more variables: feedtype_grazers <chr>, comm_group <chr>, Cat_1 <chr>,
#> #   Name_biomass <chr>, dbMFA_items <chr>, FEDNA <chr>, default_destiny <chr>
```
