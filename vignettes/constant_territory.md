# Constant-territory time series

`build_constant_territory_series()` estimates a time series of a quantity over a
**fixed** set of boundaries — the polities active in a chosen `ref_year` — from
data that was reported under the *changing* historical boundaries of each data
year.

## The problem

Borders move. A 1900 wheat figure for "Austria-Hungary" is not a figure for
present-day Austria, so a constant-present-day-territory series cannot be read
off the raw data — it has to be *estimated*. (WHEP's existing pre-1962 country
extension already produces a constant-modern-territory series a different way:
it anchors on each modern country's later value and walks it back along that
country's LUH cropland trajectory. This function is the complementary
**spatial** route — useful once real data for genuinely historical polities,
whose borders differ from any modern country, is brought in.)

## The method (dasymetric areal interpolation)

For each data `year`:

1. **Disaggregate** — each source polity's reported value is spread across a
   regular equal-area grid covering *that polity's own extent for that year*,
   in proportion to a `covariate` density (gridded cropland for crop output,
   population for demographic series, livestock density for animals). With
   `covariate = NULL` the weight is cell area (plain areal interpolation).
2. **Re-aggregate** — the grid is summed into the `ref_year` target boundaries;
   a target's `covered` value is the grid mass inside it.
3. **Impute the gap** — target territory not covered by any source-with-data
   that year still carries covariate mass, so it is filled at a donor intensity
   (`donor = "regional"`: the region-wide value per unit covariate). The
   covariate fraction that had to be imputed is returned as `imputed_share` — an
   honest confidence signal (0 = fully observed, 1 = fully extrapolated).

The estimate is only as good as the covariate; supply the same gridded surface
used elsewhere in WHEP spatialization for consistency.

## Why a covariate, not just area

Production/population is not spread uniformly over a territory. Weighting by
cropland (or population) puts the source's value where the activity actually
was, so when a modern boundary cuts the historical source, each side gets its
*real* share — not its area share. `covariate = NULL` (area) is the fallback
when no density surface is available.

## How the gap is handled

Because the imputation rides on a spatially **complete** covariate (e.g. an LUH
reconstruction has no holes), a slice of the target that *no historical source
reported* is still estimated: its own covariate mass × a transferred intensity.
There is no silent zero. Watch `imputed_share` — a few percent is solid; a large
fraction means the year/territory is mostly modeled and should be treated as
such.

## Usage

```r
series <- build_constant_territory_series(
  data     = reported,          # tibble: year, polity_code, value
  ref_year = 2020,              # target boundaries = polities active in 2020
  covariate = cropland_density, # function(centroids_sf, year) -> density; NULL = area
  resolution = 25000            # grid cell size in metres
)
# series: target_polity_code, year, value, covered, imputed, imputed_share, n_sources
```

Boundaries come from [get_polity_geometries()] by default; pass `polities` to
use a subset (and to keep the gridded region compact — a continent-spanning
target trips the `max_cells` guard).

## Worked example

Reallocating a 1900 Austria-Hungary total of 1000 to present-day boundaries
(`ref_year = 2020`, area weighting) gives, for a few successors:

| present-day state | covered | imputed_share | note |
|---|--:|--:|---|
| Hungary | 136 | 0.00 | wholly inside the empire |
| Austria | 123 | 0.00 | wholly inside |
| Czechia | 115 | 0.02 | wholly inside |
| Slovakia | 73 | 0.00 | wholly inside |
| Romania | 159 | 0.57 | only Transylvania was in A-H |
| Poland | 53 | 0.89 | only Galicia/Silesia fringe |
| Italy | 22 | 0.95 | only South Tyrol / Trieste |

`sum(covered) ≈ 1000` (mass conserved). Read `covered` as the faithful
territorial reallocation; treat a high `imputed_share` (Romania, Poland, Italy)
as "mostly modeled, not observed."

A present-day-Austria series assembled from differently-bounded sources —
empire-wide in 1850/1900, modern Austria from 1925 — yields the territorial
slice in the empire years (96.6 in 1850, 123 in 1900, `imputed_share 0`) and the
reported value once the source already is modern Austria.
