# FAOSTAT crop to LPJmL crop functional type (CFT) mapping

Maps FAOSTAT primary-production item codes to WHEP's granular 33-class
crop functional type taxonomy and the coarser LPJmL-compatible parent
class. Used by
[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
and
[`run_spatialize()`](https://eduaguilera.github.io/whep/reference/run_spatialize.md)
to aggregate spatialized crop-level output into named crop functional
types.

## Usage

``` r
cft_mapping
```

## Format

A tibble with one row per mapped FAOSTAT item. Columns:

- `item_prod_code`: Integer FAOSTAT item code.

- `item_prod_name`: Human-readable FAOSTAT item name.

- `cft_name`: Granular WHEP CFT name (33 classes, e.g.
  `"temperate_cereals"`, `"coffee"`, `"oil_crops_oilpalm"`).

- `cft_lpjml`: LPJmL-compatible parent class; one of the 12 LPJmL v6
  named crop CFTs or `"others"`.

- `luh2_type`: LUH2 crop functional type (`c3ann`, `c4ann`, `c3per`, or
  `c3nfx`).

## Source

Adapted from LandInG's `crop_types_FAOSTAT_LPJmL_default.csv` (Ostberg
et al. 2023) with WHEP granular extensions.

## Examples

``` r
head(cft_mapping)
#> # A tibble: 6 × 5
#>   item_prod_code item_prod_name cft_name          cft_lpjml         luh2_type
#>            <int> <chr>          <chr>             <chr>             <chr>    
#> 1             15 Wheat          temperate_cereals temperate_cereals c3ann    
#> 2             27 Rice           rice              rice              c3ann    
#> 3             44 Barley         temperate_cereals temperate_cereals c3ann    
#> 4             56 Maize (corn)   maize             maize             c4ann    
#> 5             71 Rye            temperate_cereals temperate_cereals c3ann    
#> 6             75 Oats           temperate_cereals temperate_cereals c3ann    
```
