# Safe-and-just nitrogen classification levels and colours.

The ordered levels of the SJOS-N 2-way classification: the
reactive-nitrogen boundary axis (`Within_boundary` versus `Exceedance`)
crossed with the nourishment axis (`Under`, `Adequate`, `Over`), with a
plotting colour per combined level. Used to classify and colour polities
on the safe-and-just nitrogen space.

## Usage

``` r
sjos_levels
```

## Format

A tibble with columns:

- level:

  Combined classification level (e.g. `"Within_boundary Under"`,
  `"Exceedance Over"`).

- order:

  Integer plotting/factor order (1 to 6).

- colour:

  Plotting colour (an R colour name).

## Source

Boundary axis from the agricultural reactive-nitrogen boundary framework
of Schulte-Uebbing, L. F., Beusen, A. H. W., Bouwman, A. F. & de Vries,
W. (2022).
[doi:10.1038/s41586-022-05158-2](https://doi.org/10.1038/s41586-022-05158-2)
and de Vries, W. et al. (2013).
[doi:10.1016/j.cosust.2013.07.004](https://doi.org/10.1016/j.cosust.2013.07.004)
; nourishment axis from the nourishment thresholds (see
[nourishment_thresholds](https://eduaguilera.github.io/whep/reference/nourishment_thresholds.md)).
The level labels and colours are ported by value from
`afsetools::load_vectors()` (`SJOS_levels` and `SJOS_colours`); verify
against Zotero before any manuscript use.

## Examples

``` r
sjos_levels
#> # A tibble: 6 × 3
#>   level                    order colour       
#>   <chr>                    <int> <chr>        
#> 1 Within_boundary Under        1 lightseagreen
#> 2 Within_boundary Adequate     2 lightgreen   
#> 3 Within_boundary Over         3 burlywood3   
#> 4 Exceedance Under             4 mediumpurple 
#> 5 Exceedance Adequate          5 salmon1      
#> 6 Exceedance Over              6 indianred3   
```
