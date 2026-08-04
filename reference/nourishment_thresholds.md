# Nourishment protein and energy thresholds.

Protein and dietary-energy floors and targets for the SJOS-N nourishment
("just") axis, plus the waste-and-inequality factor that lifts the raw
per-capita protein floor and target to the supply-side thresholds and
the normalised-score class cutoffs. Raw protein floor and target (46 and
63 g/cap/day) are scaled by the 1.35 waste-and-inequality factor to the
supply-side protein floor and target (62.1 and 85.05 g/cap/day). The
energy floor and target (2300 and 2900 kcal/cap/day) are carried for the
SJOS food-energy cross-check, not for the nitrogen classification
itself. The class cutoffs (1 and 2) split a normalised nourishment score
into the Under, Adequate and Over classes.

## Usage

``` r
nourishment_thresholds
```

## Format

A tibble in long form with columns:

- metric:

  Metric name: `"protein_raw"`, `"protein"`, `"energy"`,
  `"waste_inequality"` or `"class"`.

- bound:

  Which bound the value is: `"floor"`, `"target"`, `"factor"`, `"under"`
  or `"over"`.

- value:

  Numeric threshold value.

- unit:

  Unit of the value (`"g/cap/day"`, `"kcal/cap/day"`, `"ratio"` or
  `"score"`).

## Source

Dietary protein and energy adequacy and the food-system
environmental-limits framing: Springmann, M. et al. (2018). Options for
keeping the food system within environmental limits. *Nature*, 562,
519-525.
[doi:10.1038/s41586-018-0594-0](https://doi.org/10.1038/s41586-018-0594-0)
. The specific protein floor and target, the energy floor and target,
the 1.35 waste-and-inequality factor and the score class cutoffs are
transcribed from the Global SJOS-N analysis; verify against Zotero
before any manuscript use.

## Examples

``` r
nourishment_thresholds
#> # A tibble: 9 × 4
#>   metric           bound    value unit        
#>   <chr>            <chr>    <dbl> <chr>       
#> 1 protein_raw      floor    46    g/cap/day   
#> 2 protein_raw      target   63    g/cap/day   
#> 3 protein          floor    62.1  g/cap/day   
#> 4 protein          target   85.1  g/cap/day   
#> 5 energy           floor  2300    kcal/cap/day
#> 6 energy           target 2900    kcal/cap/day
#> 7 waste_inequality factor    1.35 ratio       
#> 8 class            under     1    score       
#> 9 class            over      2    score       
```
