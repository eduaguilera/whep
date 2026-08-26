# GLEAM animal weights.

Typical live weights by region, species, system, and cohort.

## Usage

``` r
gleam_animal_weights
```

## Format

A tibble with `region`, `species`, `system`, `cohort`, `weight_kg`.

## Source

The shipped values are unsourced placeholders, but the real GLEAM table
has been located (whep#881): FAO. 2018. *Global Livestock Environmental
Assessment Model, Model description, Version 2.0, Revision 5*. Rome,
FAO, Supplement S1, "Live weights (kg)" block of Tables 2.4 (dairy
cattle), 2.5 (beef cattle), 2.6 (feedlot cattle), 2.7 (buffaloes), 2.10
(sheep), 2.11 (goats) and 2.14-2.16 (backyard, intermediate and
industrial pigs), workbook
<https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Supplement_S1.xlsx>
(md5 `72fd2ea477dfe8b30cd3657b2baa4af1`, retrieved 2026-08-26). The
Version 3.0 description drops those regional herd-parameter tables,
which is why the committed GLEAM 3.0 Supplement S1 workbook has no sheet
for them.

The shipped values do **not** match that source and have not been
replaced here, because doing so is a science decision that moves
published numbers: `.join_weights()` in `R/livestock_energy.R` uses this
table as the Tier 2 live weight, and gross energy scales as
`weight^0.75` for maintenance and `weight` for activity. Measured per
cohort against the GLEAM values (system-averaged the way
`.join_weights()` averages them), gross energy – hence Tier 2 enteric
CH4 – would move by: Global cattle adult female 400 -\> 457 kg, +6.9%;
Global cattle fattening 300 -\> 399 kg, +14.2%; Western Europe cattle
adult male 1000 -\> 732 kg, -16.4%; North America cattle adult female
615 -\> 700 kg, +7.3%; Global sheep adult female 45 -\> 53 kg, +7.2%.
GLEAM also publishes no live weight for the Replacement cohorts (its
herd module derives them), so a re-ingest cannot remove every
assumption. Separately, the `"Latin America"` rows are unreachable:
`.gleam_region_of()` emits GLEAM 3.0 region labels, in which that region
is `"Central & South America"`, so those rows silently take the Global
weights.

## Examples

``` r
gleam_animal_weights
#> # A tibble: 32 × 5
#>    region             species system cohort       weight_kg
#>    <chr>              <chr>   <chr>  <chr>            <dbl>
#>  1 Western Europe     Cattle  Dairy  Adult Female       650
#>  2 Western Europe     Cattle  Dairy  Adult Male        1000
#>  3 Western Europe     Cattle  Beef   Adult Female       600
#>  4 Western Europe     Cattle  Beef   Fattening          400
#>  5 North America      Cattle  Dairy  Adult Female       680
#>  6 North America      Cattle  Dairy  Adult Male        1000
#>  7 North America      Cattle  Beef   Adult Female       550
#>  8 North America      Cattle  Beef   Fattening          450
#>  9 Sub-Saharan Africa Cattle  All    Adult Female       250
#> 10 Sub-Saharan Africa Cattle  All    Adult Male         350
#> # ℹ 22 more rows
```
