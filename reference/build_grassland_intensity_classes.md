# Classify grassland cells as intensive or extensive through time

Moves the IMAGE 2010 intensive/extensive grassland map behind the
Schulte-Uebbing et al. (2022) critical-nitrogen archive (Zenodo
doi:10.5281/zenodo.6395016) through time, one row per 0.5-degree cell
and year. It is the class table the `land_use = "all"` comparison of
[`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md)
consumes (issue \#1285).

Each cell belongs to its 2010 country: the crosswalk area code holding
the largest share of the cell (ties to the smallest code). The national
grazing density of a polity in a year is the livestock units of grazer
species it reported that year per hectare of WHEP grassland (pasture
plus rangeland) inside its territory. A cell's `density_ratio` is the
density of the polity its 2010 country belonged to in the year, over the
density of that country in 2010. Each 2010 country's intensive share of
IMAGE grassland is scaled by the area-weighted mean ratio of its cells
and clamped to `[0, 1]`; cells are then switched by rank of 2010 manure
N per hectare until the intensive area is nearest the target (demotion
from the lowest-manure intensive cell up, promotion from the
highest-manure extensive cell down). At 2010 the ratio is 1 and the
classes equal the IMAGE map.

Livestock units are reported under the polity that existed in the year –
the USSR, not Russia, in 1961 – while the crosswalk knows present-day
area codes only. Each present-day code is resolved to its reporting
polity in the year with
[`resolve_polity_lineage()`](https://eduaguilera.github.io/whep/reference/resolve_polity_lineage.md),
against the polities the livestock units are actually reported under
that year. A lineage answer that is a different interval of a polity
another present-day code reports under itself is refused (it would book,
say, Belgium on the Netherlands). A code still unresolved is placed on a
reporting polity whose published successors reach it (the Baltic states
and Belgium-Luxembourg), when exactly one does, after WHEP's own
`polity_area_code` fold has had its chance (Sudan and South Sudan are
reported folded into area 206 from 2012). Grassland of a code that
resolves to no reporting polity enters no denominator, and its cells
keep their IMAGE class.

A country's density is its own reporting polity's density in the years
it reports on its own (`density_basis = "own"`), and the shared bucket's
density in the years it is reported folded into one after a split
(`"bucket"`, Sudan and South Sudan in area 206 from 2012). For the years
before it reported on its own, the country keeps its own level and
borrows only the trend of the polity its cells reported under
(`"chained_predecessor_trend"`): with `t0` the first later year the
country has a positive density and `t0_pred` the last year up to
`t0 - 1` the predecessor has one,
`D_k(t) = D_k(t0) * D_pred(t) / D_pred(t0_pred)`. Chains of successions
are linked one step at a time from the present back (Yugoslavia, then
Serbia and Montenegro, then Serbia). A missing link leaves the density,
and the ratio, undefined. Densities are therefore read for every year
from the earliest requested one to the last year both the livestock
units and the grassland surface cover, not only the requested years.
Kazakhstan shows why: in 1961 the USSR's national density is six times
Kazakhstan's own 2010 density, so taking it as Kazakhstan's level would
promote most of Kazakhstan's grassland to intensive.

Declared assumptions, each a constructed choice rather than a
measurement:

- IMAGE's classes are livestock production systems (mixed systems
  intensive, pastoral systems extensive), not a threshold on grazing
  density. The national density trend is a proxy for the trend in that
  split.

- The ranked reclassification has no published precedent (issue \#1285).

- WHEP grassland in a cell with no IMAGE grassland is extensive, stamped
  `"no_image_grassland"` (maintainer decision 2026-09-24, issue \#1285).
  Its allowance is applied downstream.

- Livestock units are the `"LU"` rows of
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  for the `animals_codes` "Grazers", from `liv_lu_coefs`, whose source
  is generic ("standard livestock unit definitions from FAO and European
  agricultural statistics"). Only density ratios are used, so the
  coefficients matter through changes in species mix alone.

- Chaining a successor onto its predecessor's trend is a constructed
  rule (maintainer decision 2026-09-24, issue \#1285): it assumes the
  successor's density moved with its predecessor's before it reported on
  its own.

- A cell whose 2010 country has no finite, positive 2010 density, or
  whose polity has no density in the year, keeps its IMAGE class and is
  stamped `"image2010_fixed_no_density"`. So is an IMAGE grassland cell
  the crosswalk assigns to no country.

## Usage

``` r
build_grassland_intensity_classes(years, data = list(), example = FALSE)
```

## Arguments

- years:

  Integer vector of years to classify. 2010 is always read as the base
  of the density ratio; it is returned only when requested.

- data:

  Optional named list of injected inputs, each replacing its real read:

  - `stock_lu`:
    [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)-shaped
    rows with `year`, `area_code`, `item_prod_code`, `unit` and `value`.
    Default: that function from the earliest requested year (or 2010) to
    the last year of the grassland surface.

  - `gridded_pasture`: `lon`, `lat`, `year`, `pasture_ha`,
    `rangeland_ha`. Default: the parquet at `WHEP_GRIDDED_PASTURE_PATH`
    when set, else the `spatialize-gridded-pasture` pin.

  - `cell_polity`: `lon`, `lat`, `area_code`, `polity_frac`. Default:
    the `spatialize-cell-polity-fraction` pin
    (`WHEP_POLITY_FRACTION_PATH` overrides it).

  - `grassland_layers`: one row per IMAGE cell with `cell_id`, `lon`,
    `lat`, `a_crop_ha`, `a_gr_int_ha`, `a_gr_ext_ha`, `manure_int_n_kg`,
    `manure_ext_n_kg`, `image_class_2010` and `image_region`. Default:
    the critical-nitrogen archive, resolved as
    [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
    resolves it.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble, one row per cell and year, sorted by `cell_id` and `year`.
Cells are those with IMAGE grassland plus those with WHEP grassland in
any requested year. Columns:

- `cell_id`: canonical 0.5-degree cell key.

- `lon`, `lat`: cell centre, degrees.

- `year`: calendar year.

- `country_2010`: crosswalk area code holding the largest share of the
  cell; `NA` for a cell the crosswalk does not cover.

- `image_region`: IMAGE region (1-26); `NA` outside the archive.

- `a_crop_ha`: IMAGE 2010 cropland area, ha (0 outside the archive).

- `grass_ha_image`: IMAGE 2010 intensive plus extensive grassland, ha (0
  where IMAGE has none).

- `whep_grass_ha`: WHEP pasture plus rangeland in the year, ha (0 where
  the surface carries none).

- `image_class_2010`: `"intensive"`, `"extensive"`, or `NA` where IMAGE
  has no grassland.

- `grassland_class`: `"intensive"` or `"extensive"`.

- `density_ratio`: the cell's grazing-density ratio to 2010; `NA` where
  undefined.

- `target_share`: the 2010 country's target intensive share; `NA` where
  undefined and for cells with no IMAGE grassland.

- `method_grassland_split`: `"image2010_density_rank"`,
  `"image2010_fixed_no_density"` or `"no_image_grassland"`.

- `density_basis`: how the 2010 country's density in the year was
  formed: `"own"`, `"chained_predecessor_trend"` or `"bucket"`; `NA`
  where the cell has no 2010 country or its code resolves to no
  reporting polity that year.

## Examples

``` r
build_grassland_intensity_classes(example = TRUE)
#> # A tibble: 12 × 15
#>    cell_id   lon   lat  year country_2010 image_region a_crop_ha grass_ha_image
#>      <int> <dbl> <dbl> <int>        <int>        <int>     <dbl>          <dbl>
#>  1   50131  45.2  55.2  1961          185           15      5000          20000
#>  2   50131  45.2  55.2  2010          185           15      5000          20000
#>  3   50132  45.8  55.2  1961          185           15      2000          60000
#>  4   50132  45.8  55.2  2010          185           15      2000          60000
#>  5  179520 -60.2 -34.8  1961            9            5     10000          40000
#>  6  179520 -60.2 -34.8  2010            9            5     10000          40000
#>  7  179521 -59.8 -34.8  1961            9            5      8000          30000
#>  8  179521 -59.8 -34.8  2010            9            5      8000          30000
#>  9  179522 -59.2 -34.8  1961            9            5         0          50000
#> 10  179522 -59.2 -34.8  2010            9            5         0          50000
#> 11  179523 -58.8 -34.8  1961            9           NA         0              0
#> 12  179523 -58.8 -34.8  2010            9           NA         0              0
#> # ℹ 7 more variables: whep_grass_ha <dbl>, image_class_2010 <chr>,
#> #   grassland_class <chr>, density_ratio <dbl>, target_share <dbl>,
#> #   method_grassland_split <chr>, density_basis <chr>
```
