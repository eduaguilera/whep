# Nourishment protein and energy thresholds.

Protein and dietary-energy floors and ceilings for the SJOS-N
nourishment ("just") axis, plus the waste-and-inequality factor that
lifts the raw per-capita protein bounds to supply-side thresholds and
the normalised-score class cutoffs. Raw protein floor and ceiling (46
and 63 g/cap/day) are scaled by the 1.35 waste-and-inequality factor to
the supply-side protein floor and ceiling (62.1 and 85.05 g/cap/day).
The class cutoffs (1 and 2) split a normalised nourishment score into
the Under, Adequate and Over classes.

**Only one of these numbers is sourced.** `provenance` records which,
per row, so nothing here can look verified when it is not: the protein
floor of 46 g/cap/day is WHO/FAO/UNU TRS 935 Table 46, the safe intake
of a 55 kg adult at a PDCAAS of 1.0 — and that safe level is a
97.5th-percentile *individual* value, which TRS 935 (p.41) says is
incorrect to apply to a population. The 63 ceiling, the 2300 and 2900
energy bounds and the 1.35 factor are `inherited_unsourced`: they come
from the Global SJOS-N analysis and no source has been produced for
them, the author having confirmed 1.35 was a preliminary presentation
figure (whep#753).

The energy bounds are **not** a second axis WHEP publishes. Nothing in
the package reads them, and WHEP's own energy column is gross combustion
energy where a dietary kcal threshold is metabolisable, so the two are
not comparable as they stand. See
[`normalize_nourishment()`](https://eduaguilera.github.io/whep/reference/normalize_nourishment.md).

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

  Which bound the value is: `"floor"`, `"ceiling"`, `"factor"`,
  `"under"` or `"over"`. The upper protein and energy bound is named
  `"ceiling"` because
  [`normalize_nourishment()`](https://eduaguilera.github.io/whep/reference/normalize_nourishment.md)
  uses it as the top of the Adequate band, above which a country is
  classified Over — not as something to aim at (whep#753).

- value:

  Numeric threshold value.

- unit:

  Unit of the value (`"g/cap/day"`, `"kcal/cap/day"`, `"ratio"` or
  `"score"`).

- provenance:

  Where the value comes from: `"trs935_table46_55kg_safe_level"`,
  `"derived_raw_times_waste_inequality"`, `"inherited_unsourced"` or
  `"definition"`.

## Source

The protein floor, and only the protein floor: WHO/FAO/UNU (2007).
*Protein and amino acid requirements in human nutrition*, WHO Technical
Report Series 935, Table 46.

This table previously cited Springmann, M. et al. (2018),
[doi:10.1038/s41586-018-0594-0](https://doi.org/10.1038/s41586-018-0594-0)
, for its values. That attribution was **wrong** and has been removed:
two independent full-text searches of the paper and its supplementary
information found none of 46, 63, 62.1, 85.05, 1.35, 2300 or 2900 in it.
Springmann's protein figures appear only as diet composition. Every
value other than the floor is transcribed from the Global SJOS-N
analysis with no source established, and carries
`provenance == "inherited_unsourced"`; do not cite one without finding a
source first.

## Examples

``` r
nourishment_thresholds
#> # A tibble: 9 × 5
#>   metric           bound     value unit         provenance                      
#>   <chr>            <chr>     <dbl> <chr>        <chr>                           
#> 1 protein_raw      floor     46    g/cap/day    trs935_table46_55kg_safe_level  
#> 2 protein_raw      ceiling   63    g/cap/day    inherited_unsourced             
#> 3 protein          floor     62.1  g/cap/day    derived_raw_times_waste_inequal…
#> 4 protein          ceiling   85.1  g/cap/day    derived_raw_times_waste_inequal…
#> 5 energy           floor   2300    kcal/cap/day inherited_unsourced             
#> 6 energy           ceiling 2900    kcal/cap/day inherited_unsourced             
#> 7 waste_inequality factor     1.35 ratio        inherited_unsourced             
#> 8 class            under      1    score        definition                      
#> 9 class            over       2    score        definition                      
```
