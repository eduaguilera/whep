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

## Details

Only the items listed here reach the grid, so each crop is listed on the
code that carries its harvested area. Where
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
books the area of several co-products on one item
([primary_double](https://eduaguilera.github.io/whep/reference/primary_double.md)),
that item is the one listed: Coconuts (248), not coconuts in shell
(249); Linum (772), not linseed (333) or flax (773); Hemp (776), not
hempseed (336) or true hemp (777); Kapok fruit (310), not kapok fibre
(778). Linum takes linseed's `cft_name` and Hemp takes true hemp's,
after the product with the larger FAOSTAT harvested area (2010: linseed
2.35 Mha against flax 0.22 Mha; true hemp 0.051 Mha against hempseed
0.005 Mha).

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
