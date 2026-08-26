# Grassland share of synthetic nitrogen by country and year

Country-level time series of the share of synthetic nitrogen applied to
grassland (versus cropland). Used to split national N totals between
land use types in the WHEP nitrogen pipeline.

The series is the per-country grassland proportion that Lassaletta et
al. (2014) subtracted from FAO national fertiliser use to obtain
nitrogen input to cropland: "FAO data on annual per country synthetic
fertilizer use refer to total use in agriculture and is provided without
distinction between arable and grassland. We therefore had to subtract
from these figures the proportion used for grassland fertilization,
which in some European countries such as Ireland and the Netherlands
accounts for a significant proportion." The proportions in that paper
rest in turn on Richard (1951), Power and Alessi (1971), Anonymous
(1992), FAO (2006) and Heffer (2013), detailed in its supplementary
material S1.

## Usage

``` r
lassaletta_grassland_share
```

## Format

A tibble with one row per country-year combination containing:

- `Country`: Country name, in the FAO-era vocabulary of the source
  (`"Belgium-Luxemburg"`, `"Czechoslovakia"`, `"Ethiopia PDR"`,
  `"Yugoslav SFR"`, `"FSU"`).

- `year`: Year (numeric), 1961-2009 for every label.

- `grass_share`: Share of synthetic N applied to grassland, as a
  fraction in 0-1. The tracked CSV holds the same numbers as
  percentages.

141 labels x 49 years = 6,909 rows. 4,798 of them are exactly 0 and only
53 labels are ever non-zero; the largest shares are Ireland (0.88) and
the Netherlands (0.71), the two countries the source singles out. Many
series are linear ramps from 0 in 1961 up to the first documented value,
so a value is not an observation for its own year.

The label set is **not** a partition: it carries a historical entity and
its modern successors side by side for the whole span, so `Sudan`,
`Sudan (former)` and `South Sudan` all appear for 1961-2009, with
`Sudan` and `Sudan (former)` carrying identical values. A consumer that
resolves these labels to area codes therefore needs a duplicate-key
rule.

## Source

Lassaletta, L., Billen, G., Grizzetti, B., Anglade, J., & Garnier, J.
(2014). 50 year trends in nitrogen use efficiency of world cropping
systems: the relationship between yield and nitrogen input to cropland.
*Environmental Research Letters*, 9(10), 105011.
[doi:10.1088/1748-9326/9/10/105011](https://doi.org/10.1088/1748-9326/9/10/105011)
. The per-country values are that paper's supplementary material S1 and
its Annex 1 CSV.

The attribution is evidential, not byte-verified. The tracked CSV
arrived from an upstream project that labelled it with this DOI; its
span (1961-2009), its country vocabulary and its two extreme countries
all agree with the paper, and at 19,531 bytes the wide original is
within 2% of the 19.2 KB IOP reports for Annex 1. IOP blocks automated
download of that file, so the two were never compared row by row, and
the duplicated Sudan labels may be the paper's or a later addition.

## Examples

``` r
head(lassaletta_grassland_share)
#> # A tibble: 6 × 3
#>   Country      year grass_share
#>   <chr>       <dbl>       <dbl>
#> 1 Afghanistan  1961           0
#> 2 Afghanistan  1962           0
#> 3 Afghanistan  1963           0
#> 4 Afghanistan  1964           0
#> 5 Afghanistan  1965           0
#> 6 Afghanistan  1966           0
```
