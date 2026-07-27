# AMG humification coefficient by carbon input type.

Lookup table giving the AMG humification coefficient `h` (the fraction
of carbon inputs entering the active pool) as a function of the carbon
input type. Rows are matched in ascending `match_order` against the
lowercased input type using the `pattern` regular expression; the last
row (input type `"default"`, with a missing pattern) is the fallthrough.

## Usage

``` r
amg_h_by_input_type
```

## Format

A tibble with columns:

- match_order:

  Integer matching priority (lower is tried first).

- input_type:

  Canonical input type label (e.g. `"green_manure"`, `"manure"`,
  `"residue"`, `"default"`).

- pattern:

  Regular expression matched against the lowercased input type; `NA` for
  the fallthrough row.

- h:

  Humification coefficient (fraction of input carbon stabilised).

## Source

Saffih-Hdadi, K. & Mary, B. (2008). Modeling consequences of straw
residues export on soil organic carbon. *Soil Biology and Biochemistry*,
40(3), 594-607.
[doi:10.1016/j.soilbio.2007.08.022](https://doi.org/10.1016/j.soilbio.2007.08.022)
.

## Examples

``` r
amg_h_by_input_type
#> # A tibble: 7 × 4
#>   match_order input_type     pattern                                        h
#>         <dbl> <chr>          <chr>                                      <dbl>
#> 1           1 green_manure   "green.?manure"                             0.2 
#> 2           2 mineral_manure "mineral.*manure|manure.*mineral"           0.3 
#> 3           3 none           "none|no_straw_no_fym|no.straw.no.fym"      0.1 
#> 4           4 manure         "fym|manure|compost|organic.?amend|slurry"  0.4 
#> 5           5 residue        "residue|straw_returned|straw\\.returned"   0.13
#> 6           6 mineral        "mineral"                                   0.13
#> 7           7 default         NA                                         0.15
```
