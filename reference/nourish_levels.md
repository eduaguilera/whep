# Nourishment classification levels and colours.

The ordered levels of the nourishment ("just") axis (`Over`, `Adequate`,
`Under`) with a plotting colour per level, used to classify and colour
polities by per-capita nourishment adequacy.

## Usage

``` r
nourish_levels
```

## Format

A tibble with columns:

- level:

  Nourishment level: `"Over"`, `"Adequate"` or `"Under"`.

- order:

  Integer plotting/factor order (1 to 3).

- colour:

  Plotting colour (an R colour name).

## Source

Level labels and colours ported by value from
`afsetools::load_vectors()` (`Nour_levels` and `Nourish_colours`). The
nourishment adequacy framing follows Springmann, M. et al. (2018).
Options for keeping the food system within environmental limits.
*Nature*, 562, 519-525.
[doi:10.1038/s41586-018-0594-0](https://doi.org/10.1038/s41586-018-0594-0)
; verify against Zotero before any manuscript use.

## Examples

``` r
nourish_levels
#> # A tibble: 3 × 3
#>   level    order colour
#>   <chr>    <int> <chr> 
#> 1 Over         1 red   
#> 2 Adequate     2 green 
#> 3 Under        3 blue  
```
