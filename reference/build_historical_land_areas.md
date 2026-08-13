# Build a pre-1962 land series measured on each year's own borders

Emit the land table the pre-1962 production back-cast consumes – `year`,
`area_code`, `Cropland`, `Pasture` and `agriland`, all in Mha – with the
hectares summed from gridded LUH2 inside the polygon of the polity that
`area_code` resolved to in that year, instead of inside present-day
borders.

A cell's land is shared among the polities whose polygons cover it, in
proportion to the covered fraction renormalised to one per cell, which
is the rule `build_cell_polity_fraction()` already uses. Renormalising
matters: LUH2's state fractions are fractions of the whole cell and
already discount open water, so weighting them by a raw coastal cell's
land share would discount it twice and lose 12-15% of the land of an
island or heavily coastal country.

[`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)
consumes only this series' year-on-year ratios, so a change of territory
can only reach the back-cast as a ratio. What that ratio should be is a
real choice, and `boundary_step` makes it:

- `"relink"` (default) re-measures the previous year inside the
  **incoming** polity's polygon before taking the ratio, so only
  within-territory growth is ever used and annexing a province never
  moves the back-cast. On Ethiopia in 1952, when Eritrea joins, that is
  +1.9% instead of +8.0%.

- `"level_step"` takes the ratio between the two polygons as measured,
  so the territorial change passes through as a level step and the 1850
  row is scaled to the smaller empire it is labelled with. That is the
  reframing the whole method exists for; it is also the option most
  exposed to a bad polygon, because an artefact of the polity database
  then compounds down the back-cast exactly as a real annexation would.

Measured over 1850-1961 against the present-day series, 18.0% of
back-cast crop tonnage at 1850 sits between the two rules, falling to
0.07% by 1960.

This reads gridded LUH2 for every requested year and is
minutes-to-tens-of- minutes of work, so it belongs in a `data-raw/`
materialisation step, not in a test or an example.

## Usage

``` r
build_historical_land_areas(
  years = 1850:1961,
  boundary_step = c("level_step", "relink"),
  data = NULL,
  example = FALSE
)
```

## Arguments

- years:

  Integer vector of calendar years to measure. Defaults to `1850:1961`,
  the span the back-cast uses.

- boundary_step:

  How a year-on-year ratio is taken across a change of territory:
  `"level_step"` (default) or `"relink"`. They answer different
  questions and differ by up to 18% of back-cast tonnage, so the choice
  is the method, not a tuning knob – see the description.

  `"level_step"` lets the series step when the territory changes,
  because a different polity is a different thing being measured. That
  is what a per-polity series means, and it is why this function exists:
  on Ethiopia it puts 1850 cropland at 1.52 Mha against the present-day
  3.22, dropping the land Menelik annexed in the 1880s-90s that the area
  never held in 1850.

  `"relink"` re-measures the previous year inside the incoming polity's
  polygon so a change of territory never appears as growth. That suits a
  FIXED-territory series, where the step is an artefact. It is NOT the
  conservative choice here: because
  [`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)
  consumes only ratios, suppressing that channel also suppresses the
  correction, and Ethiopia's 1850 comes back to 3.24 Mha – within 0.6%
  of the present-day figure this method exists to replace (whep#761).

- data:

  Named list of pre-loaded inputs bypassing the readers, for tests:
  `polity_areas` (`year`, `area_code`, `polity_code`), `cover`
  (`polity_code`, `lon`, `lat`, `frac`) and `cell_areas` (`year`, `lon`,
  `lat`, `land_use`, `area_ha`). Each falls back to its reader when
  absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `year`, `area_code`, `polity_code`, `Cropland`,
`Pasture` and `agriland`. `area_code` is the `polity_area_code`
aggregation bucket, the same key `.read_land_areas()` emits, so the
result is a drop-in for it at the back-cast seam. `polity_code` names
the territory each year was measured on, and is semicolon-separated
where a bucket holds more than one polity in a year.

## Examples

``` r
build_historical_land_areas(example = TRUE)
#> # A tibble: 10 × 6
#>     year area_code polity_code    Cropland Pasture agriland
#>    <int>     <int> <chr>             <dbl>   <dbl>    <dbl>
#>  1  1961        15 BLX-1850-1999     0.606   0.445     1.05
#>  2  1961        51 F51-1947-1993     5.35    1.81      7.16
#>  3  1900       203 ESP-1800-2025    16.2     8.20     24.4 
#>  4  1961       228 F228-1945-1991  238.    332.      570.  
#>  5  1850       238 ETH-1800-1889     3.24    9.61     12.9 
#>  6  1900       238 ETH-1897-1902     6.42   16.5      22.9 
#>  7  1951       238 ETH-1941-1952     9.73   24.3      34.0 
#>  8  1952       238 ETH-1952-1993     9.92   24.8      34.7 
#>  9  1961       238 ETH-1952-1993    12.0    29.6      41.5 
#> 10  1961       248 F248-1947-1991    8.40    6.46     14.9 
```
