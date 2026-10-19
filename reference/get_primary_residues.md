# Crop residue items

Get type and amount of residue produced for each crop production item.

## Usage

``` r
get_primary_residues(
  example = FALSE,
  cereal_residue = c("ipcc", "ensemble", "ratio", "wirsenius"),
  oil_palm_residue = c("per_product", "per_hectare", "pin"),
  non_cereal_residue = c("ipcc", "ensemble", "ratio", "pin")
)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

- cereal_residue:

  How cereal residue is estimated. One of:

  - `"ipcc"` (default): the IPCC yield-dependent line, above-ground
    residue dry matter = slope x grain dry-matter yield + intercept,
    through
    [`calculate_crop_residues()`](https://eduaguilera.github.io/whep/reference/calculate_crop_residues.md)
    with its modern-variety correction.

  - `"ensemble"`: the same estimator's mean of that line and the static
    [biomass_coefs](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
    ratio, as the soil carbon and nitrogen chain uses it.

  - `"ratio"`: the static
    [biomass_coefs](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
    ratio alone.

  - `"wirsenius"`: the predecessor's model, Wirsenius (2000) Table 3.16
    regional ratios, applied directly on Wirsenius's own region
    membership.

  See the "Cereal residue" section. Other crops are not affected.

- oil_palm_residue:

  How oil palm residue (fronds and trunks) is obtained. One of:

  - `"per_product"` (default): 60 Mt DM / 83.09 Mt of fresh fruit
    bunches = 0.722 t DM per tonne of fruit, Malaysia's 2010 fronds and
    trunks over its 2010 FAOSTAT production. It keeps the pin's
    production-times-ratio form and is the cautious rule where the
    per-hectare one is weakest: low-yield, semi-wild palm systems such
    as Nigeria's (2.6 t of fruit per hectare against Malaysia's ~17),
    which `"per_hectare"` would give several times the residue. West
    African semi-wild systems stay uncertain under either rule and would
    need their own source. Maintainer decision (whep#1424).

  - `"per_hectare"`: 60 Mt DM / 4.85 Mha = 12.37 t DM per harvested
    hectare, the same 2010 anchor. It treats frond and trunk production
    as a property of the standing palm, and the rate barely moves
    between the strategy's two Malaysian years (12.26 t DM/ha at 2012,
    from its "over 83 million dry tonnes" times "about 75 percent" on
    FAOSTAT's 5.08 Mha), where the per-tonne ratio drifts (0.656 at
    2012).

  - `"pin"`: the pin's own oil palm residue, with the recovery rate
    undone and the ratio re-keyed like every other non-cereal crop.

  See the "Oil palm residue" section.

- non_cereal_residue:

  How the residue of a non-cereal crop with its own line in IPCC (2006)
  Table 11.2 is estimated: soybean, groundnut, dry beans, peas,
  chickpeas, lentils and the other pulses, potato, and, through IPCC's
  "Tubers" line, cassava, sweet potato, yams and the other roots. One of
  `"ipcc"` (default), `"ensemble"` and `"ratio"`, the same estimators as
  for `cereal_residue`, or `"pin"`, the pin's ratio as for every other
  crop. See the "Non-cereal residue" section.

## Value

A tibble with the crop residue data. It contains the following columns:

- `year`: The year in which the recorded event occurred.

- `area_code`: The code of the country where the data is from. For code
  details see e.g.
  [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

- `item_cbs_code_crop`: FAOSTAT internal code for each commodity balance
  sheet item. This is the crop that is generating the residue.

- `item_cbs_code_residue`: FAOSTAT internal code for each commodity
  balance sheet item. This is the obtained residue. In the commodity
  balance sheet, this can be three different items right now:

  - `2105`: `Straw`

  - `2106`: `Other crop residues`

  - `2107`: `Firewood`

  These are actually not FAOSTAT defined items, but custom defined by
  us. When necessary, FAOSTAT codes are extended for our needs.

- `value`: The amount of residue produced – all of it, before any is
  recovered from the field – in tonnes of **fresh matter**, like every
  other commodity-balance quantity.

- `value_dm`: The same residue in tonnes of **dry matter**: each crop's
  fresh residue times its own residue dry-matter content,
  `Residue_kgDM_kgFM` in
  [biomass_coefs](https://eduaguilera.github.io/whep/reference/biomass_coefs.md),
  summed per row. `NA` where a crop with residue mass carries no such
  coefficient, so the gap stays visible rather than reading as zero.

- `method_residue`: how the row's residue was estimated: the
  `cereal_residue` method for a cereal, the `non_cereal_residue` method
  for a non-cereal crop with its own IPCC line,
  `"malaysia_nbs_per_product"` or `"malaysia_nbs_per_hectare"` for oil
  palm unless `oil_palm_residue` is `"pin"`, and `"pin"` for every other
  crop.

The pin's residue quantities are fresh matter. Across its crops the
ratio of pinned residue to product tracks the fresh-matter
residue:product ratio `kg_residue_kg_product_FM` of
[biomass_coefs](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
(about 0.8 of it for nearly every crop), not the residue's dry-matter
content, which runs from 0.13 (tomato) to 1.0 (rapeseed) (whep#1215).
Use `value_dm` wherever a quantity is defined per unit of dry matter,
such as a residue nitrogen content.

The pin does not hold the residue produced but the share of it recovered
from the field: the predecessor pipeline multiplied each residue row by
the legacy recovery rate of its region and crop category
(`recovery_rates` in `residue_recovery.csv`, see
[`calculate_residue_destinies()`](https://eduaguilera.github.io/whep/reference/calculate_residue_destinies.md))
before writing it. Every reader of `value` treats it as the whole
residue and applies a recovery rate of its own, so `value` is the pin
divided by that same rate (whep#1195). The division is exact: WHEP ships
the rate the predecessor used, and on the real pin the result equals the
predecessor's gross residue on all 427,584 rows where both exist. Where
that rate is 0 (fodder crops in seven of the eight regions; roots and
tubers, cassava, sugar beet and dry beans in West Europe and in North
America and Oceania) the pin holds 0 and the gross residue cannot be
recovered from it. Fodder crops and sugar beet carry no residue there,
as before; roots and tubers, cassava and dry beans are estimated from
yield (see "Non-cereal residue") and so carry the residue they produce,
of which the recovery rate of 0 downstream recovers nothing.

The ratio behind the pin's gross residue is the fresh-matter
residue:product ratio of
[biomass_coefs](https://eduaguilera.github.io/whep/reference/biomass_coefs.md),
scaled by the region's residue:product ratio relative to West Europe
(`residue_dm_product_dm` in the same table, Wirsenius (2000) Table 3.16)
and by a harvest-index change factor per region and year. That factor is
what makes the ratio vary by year. The predecessor's table carries it at
eight anchor years from 1910 to 2000, interpolates linearly between them
and holds 2000 constant after it, falling for instance from 1.10 in 1962
to 1.00 in 2000 in East Europe. Its code attributes the table to
Krausmann et al. (2013), *PNAS* 110:10324, Table M2 (assumed,
unverified), and WHEP does not ship it.

The predecessor looked the regional ratio up by
`regions_full$region_HANPP`, which carries Wirsenius's eight region
names but not his membership (Wirsenius 2000, Table 3.1): Southeast
Asia, Russia, Belarus, the Caucasus and Sudan took another region's
ratio (whep#1430). For every crop that reads the pin, the ratio is
re-keyed here on Wirsenius's own membership, through the (HANPP region,
UN M49 sub-region) pairs listed in `residue_feed_regions.csv`. That
lowers world non-cereal residue dry matter by 0.2% in 2010.

## Cereal residue

Cereals are the one part of the base with a published global series:
Smerald, Rahimi & Scheer (2023), *A global dataset for the production
and usage of cereal residues in the period 1997-2021*, Scientific Data
10:685,
[doi:10.1038/s41597-023-02587-0](https://doi.org/10.1038/s41597-023-02587-0)
, the mean of three methods (constant regional ratios, and two
yield-dependent ones). The pin's model put world cereal residue 16.1%
above that mean over 1997-2021 (3899 against 3357 Tg dry matter), and
25% above their constant-ratio method in every year with the same grain
production (whep#1448). Besides the membership above, two things caused
it. The West Europe anchor presumes that
[biomass_coefs](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
holds the West Europe ratio, and for cereals it does not (wheat 1.34,
barley 1.18, sorghum 1.70 and maize 0.96 kg dry matter per kg grain dry
matter, against Wirsenius's 1.0, 1.0, 1.2 and 1.2). And Wirsenius's
ratios are early-1990s harvest indices, held constant after 2000 while
yields rose.

Cereal residue is therefore estimated from the pin's own production and
harvested area (its `Product` rows, which equal the `primary_prod` pin)
with
[`calculate_crop_residues()`](https://eduaguilera.github.io/whep/reference/calculate_crop_residues.md),
the estimator the soil carbon and nitrogen chain already uses. Its
modern-variety harvest-index correction applies, keyed on the HANPP
region its adoption table is written in. The `"ipcc"` line is IPCC
(2019), *2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories*, Vol. 4, Ch. 11, Table 11.2 (p. 11.19),
whose cereal slopes and intercepts are those of IPCC (2006) Table 11.2.
Measured against Smerald et al. (`validation/residue_base_dm.R`), world
cereal residue in dry matter over 1997-2021 and in 2010 is:

|                  |                    |            |                    |          |
|------------------|--------------------|------------|--------------------|----------|
| `cereal_residue` | 1997-2021 mean, Tg | vs Smerald | years outside band | 2010, Tg |
| `"ipcc"`         | 3385               | +0.8%      | 0 of 25            | 3333     |
| `"ensemble"`     | 3059               | -8.9%      | 5 of 25            | 3008     |
| `"ratio"`        | 2732               | -18.6%     | 25 of 25           | 2684     |
| `"wirsenius"`    | 3656               | +8.9%      | 9 of 25            | 3598     |
| the pin's model  | 3899               | +16.1%     | 16 of 25           | 3826     |

The band is the spread of Smerald et al.'s three methods, widened by 5%;
their 2010 mean is 3334 Tg. `"ipcc"` matches the world total, but per
crop it books maize about 19% and sorghum about 20% below their mean and
wheat about 23% above it (2010 and 2020). `"wirsenius"` keeps the pin's
harvest-index factor and corrects the anchor by applying Table 3.16
directly: residue dry matter is grain dry matter times the ratio of the
area's Wirsenius region.

## Non-cereal residue

The West Europe anchor is not right for non-cereals either, and the
error runs both ways (whep#1518). Against the line IPCC (2006) Table
11.2 gives each crop (Vol. 4, Ch. 11, pp. 11.17-11.18), the pin's ratio
put world residue dry matter in 2010 at 1.42 times the line for soybean,
1.36 for potato, 1.95 for cassava and 1.88 for sweet potato, but 0.53
for dry beans, 0.41 for the other pulses and 0.34 for chickpeas.
Applying Wirsenius (2000) Table 3.16 directly does not cure it: soybean
would fall to 0.86 of the line.

So the crops that Table 11.2 gives a line of their own are estimated
from the pin's production and harvested area exactly as cereals are,
with
[`calculate_crop_residues()`](https://eduaguilera.github.io/whep/reference/calculate_crop_residues.md)
and its modern-variety correction:

|  |  |  |  |
|----|----|----|----|
| crops | Table 11.2 row | slope | intercept, t DM/ha |
| soybean | Soyabean | 0.93 | 1.35 |
| groundnut (with shell) | Peanut (w/pod) | 1.07 | 1.54 |
| dry beans, broad beans | Dry bean | 0.36 | 0.68 |
| peas, chickpeas, lentils, other pulses | Beans & pulses | 1.13 | 0.85 |
| potato | Potato | 0.10 | 1.06 |
| cassava, sweet potato, yams, other roots | Tubers | 0.10 | 1.06 |

The 2019 Refinement's Table 11.2 (updated, p. 11.19) repeats these
values. "Tubers" is IPCC's own crop class, "modelled after potatoes";
the crops reach it through `ipcc_crop_mapping.csv`, as in the soil
carbon and nitrogen chain. The other non-cereal lines in
`ipcc_residue_coefs.csv` (rapeseed, sunflower, sugar beet, sugar cane,
cotton) are not in Table 11.2, so those crops, tree crops and vegetables
keep the pin's ratio, as does every crop under
`non_cereal_residue = "pin"`.

World residue dry matter of the crops estimated from yield, Tg, the
pin's ratio against `"ipcc"` (the default):

|                        |               |               |               |
|------------------------|---------------|---------------|---------------|
|                        | 1961          | 2010          | 2020          |
| soybean                | 51 -\> 63     | 500 -\> 385   | 673 -\> 494   |
| groundnut              | 33 -\> 48     | 90 -\> 93     | 112 -\> 110   |
| dry beans              | 7 -\> 23      | 14 -\> 34     | 15 -\> 37     |
| peas                   | 8 -\> 17      | 14 -\> 17     | 20 -\> 22     |
| other pulses           | 18 -\> 57     | 31 -\> 79     | 40 -\> 97     |
| potato                 | 26 -\> 35     | 36 -\> 28     | 42 -\> 27     |
| cassava                | 19 -\> 16     | 65 -\> 37     | 79 -\> 46     |
| sweet potato           | 24 -\> 20     | 20 -\> 12     | 19 -\> 11     |
| all non-cereal residue | 533 -\> 625   | 2159 -\> 2070 | 2755 -\> 2595 |
| all residue            | 2672 -\> 2764 | 5492 -\> 5402 | 6622 -\> 6462 |

Soybean, groundnut and potato then sit at 1.05-1.11 of their IPCC line
in 2010 and 2020 (`validation/residue_base_dm.R`): the modern-variety
correction and the product dry-matter content of `bio_coefs`. No
published global non-cereal series exists to check them against
independently (whep#1399).

## Oil palm residue

Oil palm residue is replaced rather than taken from the pin (whep#1424).
Read as gross residue, the pin's oil palm residue is far above an oil
palm plantation's whole above-ground net primary production, fruit
included: 81% of 17.3 Mg C, so about 30 t DM at the 0.47 kg C per kg DM
of
[biomass_coefs](https://eduaguilera.github.io/whep/reference/biomass_coefs.md),
per hectare and year in the industrial plantation of Wakhid, Hirano,
Dariah & Agus (2022), *Mires and Peat* 28:02,
[doi:10.19189/map.2021.snpg.sta.2288](https://doi.org/10.19189/map.2021.snpg.sta.2288)
. In Malaysia it is several times the country's whole solid oil palm
biomass, mill residues included.

The anchor is Malaysia's field residue: fronds (46 Mt) and trunks (14
Mt) of dry matter on 4.85 Mha planted in 2010, Exhibit 1 and p. 10 of
Agensi Inovasi Malaysia (2013), *National Biomass Strategy 2020*,
version 2.0 (FAOLEX mal228571). Mill residues (empty fruit bunches,
fibre, shells) are part of the fruit bunch and are not field residue, so
they are left out. The anchor is a gross residue, so it replaces the
pin's figure after the recovery rate has been undone and the ratio
re-keyed; neither touches it. The anchored dry matter is converted to
fresh mass with the oil palm `Residue_kgDM_kgFM` of
[biomass_coefs](https://eduaguilera.github.io/whep/reference/biomass_coefs.md),
so `value_dm` is the anchor exactly. That coefficient is 0.396, the dry
matter of fresh oil palm fronds (39.6% as fed, n = 3) in Heuzé, Tran,
Sauvant & Lebas (2015), *Oil palm fronds and oil palm crop residues*,
Feedipedia, <https://www.feedipedia.org/node/6916> (whep#1516). Fronds
are 46 of the 60 Mt; trunks, whose sap is 75-80% of their weight, are
wetter, so the fresh mass is if anything understated. It sets the fresh
mass booked to `Firewood`, not the dry matter.

## The batch pin on the build path

Two further aliases were published in that same 2025-07-14 batch —
`crop_residues` and `bilateral_trade`, alongside the four above, between
12:33:43Z and 12:33:50Z. Unlike the four, these two are read on the
default build path, which is why no warning is attached to them: every
commodity balance build reads them, so a warning at the read is noise
rather than information. `bilateral_trade` has since been regenerated by
this package (#1122), so `crop_residues` is the only one still
registered at its 2025-07-14 version.

They did **not** share a provenance despite sharing a timestamp. Each
was established separately (#1054), because the timestamp alone
establishes nothing.

### `crop_residues` is predecessor output

Read by `get_primary_residues()`, and from there by
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md).
All 475,688 of its `Product` rows equal the `primary_prod` pin's
`tonnes` values exactly — no key unmatched on either side, no value
differing at a relative tolerance of 1e-6 — so it is a downstream
artifact of the same predecessor run, carrying that run's production
series into the commodity balance.

Its residue quantities are those production numbers times a
residue-to-product ratio that varies by year, and times a **recovery
rate**: the pin holds the residue the predecessor recovered from the
field, not the residue produced (#1195). The predecessor's code
(`Global/R/crop_npp.r`, then `afsetools::residue_use()`) writes each row
as

`product * kg_residue_kg_product_FM * HI_changes(region, year) * residue_dm_product_dm(region, category) / residue_dm_product_dm("West Europe", category) * recovery_rates(region, category)`,

with region and category keyed as in `residue_recovery.csv`. Rebuilt
from the predecessor's own coefficient tables, that formula reproduces
all 425,934 pin rows whose region it can key, at a relative tolerance of
1e-6. The year variation is `HI_changes` alone: a harvest-index change
factor per region, given at eight anchor years from 1910 to 2000, linear
in between and flat after 2000 (East Europe 1.10 in 1962 to 1.00 in
2000), which the predecessor attributes to Krausmann et al. (2013),
*PNAS* 110:10324, Table M2 (assumed, unverified). It is a regional
historical adjustment, not a yield response, and after 2000 the ratio of
every region and category is constant. That is why
`biomass_coefs$kg_residue_kg_product_FM` alone reproduces only 3,189 of
the 472,790 comparable keys. Of the 33,411 keys that carry a residue of
exactly 0, 30,620 are cells whose recovery rate is 0 (fodder crops in
seven of the eight regions; roots and tubers, cassava, sugar beet and
dry beans in West Europe and in North America and Oceania) and 1,919 are
crops whose `biomass_coefs` ratio is 0.

WHEP ships the recovery rate and the regional ratio the predecessor used
(both equal in all 160 cells), but not the `HI_changes` table. So
`get_primary_residues()` undoes the recovery exactly, dividing by the
same rate, but cannot recompute the predecessor's residue from
production. It then re-keys the regional ratio on Wirsenius's own region
membership, and estimates cereal residue (#1448), and that of the
non-cereal crops with their own IPCC Table 11.2 line (#1518), from the
pin's production and harvested area instead of reading it.

It is not a small input. The pin holds 7.63 Gt of recovered residue for
2010 (Straw 3.60 Gt, Other crop residues 2.49 Gt, Firewood 1.54 Gt) and
327.7 Gt over 1961–2021, which `get_primary_residues()` turns into 7.64
Gt (Straw 3.92 Gt, Other crop residues 2.82 Gt, Firewood 0.90 Gt) and
366 Gt of residue produced. 3,998 of its 249,095 output rows carry `NA`
polity columns because the pin is name-keyed.

Driving the same residue model off a fresh
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
would move those numbers. Measured for 2010 at (`area_code`,
`item_prod`): 1.958 Gt of current production sits on 6,425 keys the pin
never sees (809 Mt of it primary crops, including the modelled
temporary-grassland item), 64.8 Mt on 296 keys is pin-only, and of the
8,010 shared keys 538 disagree, putting 1.102 Gt — 11.01% of the pin's
shared-key mass — more than 1% apart. The largest class is rice, 673.9
Mt in the pin against 463.3 Mt fresh, a ratio of 0.6876: current code
puts rice on a milled-equivalent basis while the pin's is paddy, which
is a basis difference rather than an error, since straw scales with the
field crop. The second is fodder, where the current build is close to
twice the pin on every forage and silage item, the pin predating that
work. Replacing the pin therefore means choosing a residue model, which
is a science decision and not a refresh. For cereals that model is
chosen by `get_primary_residues(cereal_residue = )`, and for the
non-cereals with an IPCC line by `non_cereal_residue =`; both still run
on the pin's production.

### `bilateral_trade` is built by [`build_detailed_trade()`](https://eduaguilera.github.io/whep/reference/build_detailed_trade.md)

Read by
[`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md)
and by the live-animal branch of
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md).
The registered version, `20261001T135651Z-ca7b3`, is
[`build_detailed_trade()`](https://eduaguilera.github.io/whep/reference/build_detailed_trade.md)'s
output verbatim, with its defaults and `extend_time = FALSE`, run on the
`faostat-trade-bilateral` pin `20260407T095142Z-b3f81` (#1122):
10,038,220 rows over 1986–2021, polity-keyed, carrying 64,450,133,319
tonnes and 87,840,869,640 head.

The 2025-07-14 version it replaces, `20250714T123347Z-2c392`, held the
same FAOSTAT Detailed Trade Matrix values in the predecessor pipeline's
name-keyed schema — against the raw pin over 2010, all 296,642 shared
`tonnes` keys and all 5,102 shared `Head` keys agree exactly — but
without FAOSTAT's `1000 Head` rows: 76,141,882 thousand head of live
broiler chicken, turkey, duck, rabbit and goose trade over 1986–2021,
which PR \#1113 recovered in the producer. The regenerated pin carries
7.50x its 11,707,083,640 head and 22.5% less than its 83,147,095,412
tonnes, all of that tonnage difference in CBS items 5001-5003, which no
commodity balance carries: item 1293's unbacked tonnage, screened by
`method_unbacked_quantity` (#1023), and three CBS names with no
`item_cbs_code`. On the tonnes that do reach a trade matrix the two
versions differ by 0.056%. `.clean_bilateral_trade()` reads either
schema.

## Examples

``` r
get_primary_residues(example = TRUE)
#> # A tibble: 10 × 11
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                
#>  1  1986       138              138 MEX-1848-2025         Mexico               
#>  2  1979       170              170 PER-1942-2025         Peru                 
#>  3  1963        84               84 GRC-1947-2025         Greece (1947-2025)   
#>  4  2002       138              138 MEX-1848-2025         Mexico               
#>  5  2020       143              143 MAR-1979-2025         Morocco              
#>  6  1991       183              183 ROU-1947-2025         Romania (1947-2025)  
#>  7  2006       131              131 MYS-1965-2025         Malaysia             
#>  8  2002        28               28 MMR-1885-2025         Myanmar              
#>  9  1965        29               29 BDI-1962-2025         Burundi              
#> 10  1966       156              156 NZL-1840-2025         New Zealand          
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_cbs_code_crop <dbl>, item_cbs_code_residue <dbl>, value <dbl>,
#> #   value_dm <dbl>, method_residue <chr>
```
