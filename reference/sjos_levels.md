# Safe-and-just nitrogen classification levels and colours.

The ordered levels of the SJOS-N 2-way classification: the
reactive-nitrogen boundary axis (`Within_boundary` versus `Exceedance`)
crossed with the nourishment axis (`Under`, `Adequate`, `Over`), with a
plotting colour per combined level. Used to classify and colour polities
on the safe-and-just nitrogen space. The rows are in the realised
`afsetools::load_vectors()` `SJOS_levels` order, which that source
reverses: `"Exceedance Over"` first, `"Within_boundary Under"` last.
That is the factor-level order Global's SJOS-N figures stack and legend
on, so a figure built from these levels reproduces theirs.

## Usage

``` r
sjos_levels
```

## Format

A tibble with columns:

- level:

  Combined classification level (e.g. `"Exceedance Over"`,
  `"Within_boundary Under"`).

- order:

  Integer plotting/factor order (1 to 6), matching the row order.

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
The level labels, their order and the colours are ported by value from
`afsetools::load_vectors()` (`SJOS_levels`, which is wrapped in
[`rev()`](https://rdrr.io/r/base/rev.html) there, and `SJOS_colours`);
verify against Zotero before any manuscript use.

## Examples

``` r
sjos_levels
#> # A tibble: 6 × 3
#>   level                    order colour       
#>   <chr>                    <int> <chr>        
#> 1 Exceedance Over              1 indianred3   
#> 2 Exceedance Adequate          2 salmon1      
#> 3 Exceedance Under             3 mediumpurple 
#> 4 Within_boundary Over         4 burlywood3   
#> 5 Within_boundary Adequate     5 lightgreen   
#> 6 Within_boundary Under        6 lightseagreen
```
