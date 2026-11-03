# FAO-56 maximum effective rooting depth by production item.

Ranges of maximum effective rooting depth Zr as printed in FAO-56 Table
22 (Allen, Pereira, Raes & Smith 1998, FAO Irrigation and Drainage Paper
56, Chapter 8), mapped to WHEP production items. Footnote 1 of the table
says that smaller values may be used for irrigation scheduling and
larger ones for modelling soil water stress or rainfed conditions. Items
without a row of their own in the table carry a stated proxy
(`method_root_depth` `"proxy"`), and non-crop items (processed, animal,
aggregate or non-plant products) have missing depths.

## Usage

``` r
fao56_root_depth
```

## Format

A tibble with columns:

- item_prod_code:

  Production item code, as in
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md).

- fao56_crop:

  FAO-56 Table 22 crop name(s) the item maps to, separated by `"; "`
  when several rows are used; missing for non-crop items.

- depth_min_m:

  Lower end of the Table 22 range, in metres; missing for non-crop
  items.

- depth_max_m:

  Upper end of the Table 22 range, in metres; missing for non-crop
  items.

- method_root_depth:

  How the item maps to Table 22: `"direct"` (one table row),
  `"direct_multi"` (several table rows), `"proxy"` (a stated substitute
  crop) or `"none"` (no root depth applies).

- rationale:

  Why a proxy was chosen, or why no depth applies; missing for some
  direct matches.

- source:

  Locator of the value in FAO-56 (Chapter 8, Table 22 and the section(s)
  of the table); missing when no depth applies.

## Source

Allen, R. G., Pereira, L. S., Raes, D. & Smith, M. (1998). *Crop
evapotranspiration: Guidelines for computing crop water requirements*.
FAO Irrigation and Drainage Paper 56, Chapter 8, Table 22.
<https://www.fao.org/4/x0490e/x0490e0e.htm>.

## Examples

``` r
fao56_root_depth
#> # A tibble: 305 × 7
#>    item_prod_code fao56_crop depth_min_m depth_max_m method_root_depth rationale
#>             <dbl> <chr>            <dbl>       <dbl> <chr>             <chr>    
#>  1            809 Banana - …         0.5         0.9 proxy             abaca is…
#>  2            800 Sisal              0.5         1   proxy             agave fi…
#>  3            221 Almonds            1           2   direct            NA       
#>  4            711 Carrots            0.5         1   proxy             umbellif…
#>  5            515 Apples, C…         1           2   direct            pome fru…
#>  6            526 Apricots,…         1           2   direct            NA       
#>  7            226 Palm Trees         0.7         1.1 direct            generic …
#>  8            366 Artichokes         0.6         0.9 direct            NA       
#>  9            367 Asparagus          1.2         1.8 direct            NA       
#> 10            572 Avocado            0.5         1   direct            NA       
#> # ℹ 295 more rows
#> # ℹ 1 more variable: source <chr>
```
