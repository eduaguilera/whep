# Build commodity balance sheets

Construct commodity balance sheets (CBS) from raw FAOSTAT data. This is
a convenience wrapper that chains the three pipeline steps:

1.  `.read_cbs()` — read & reformat FAOSTAT CBS data.

2.  `.fix_cbs()` — processing calibration, trade imputation, destiny
    filling, and final balancing.

3.  `.qc_cbs()` — flag data-quality anomalies.

## Usage

``` r
build_commodity_balances(
  primary_all,
  start_year = 1850,
  end_year = 2023,
  smooth_carry_forward = FALSE,
  example = FALSE,
  historical_data = NULL,
  format = c("long", "wide"),
  trade_recovery = c("none", "net_import"),
  trade_zero = .cbs_trade_zero_choices(),
  share_overflow = .cbs_share_overflow_choices(),
  negative_supply = .cbs_negative_supply_choices(),
  hist_trade_scale = .hist_trade_scale_choices(),
  export_share_overflow = .cbs_export_overflow_choices(),
  export_share_basis = .cbs_export_basis_choices(),
  seed_backcast = .cbs_seed_backcast_choices(),
  unmatched_processing = .cbs_unmatched_proc_choices(),
  silk_basis = .silk_basis_choices(),
  tobacco_leaf_use = .tobacco_leaf_use_choices(),
  .fixed_data = NULL
)
```

## Arguments

- primary_all:

  A tibble of primary production, as returned by
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md).

- start_year:

  Integer. First year to include. Default `1850`.

- end_year:

  Integer. Last year to include. Default `2023`.

- smooth_carry_forward:

  Logical. If `TRUE`, carry-forward tails are replaced with a linear
  trend. Default `FALSE`.

- example:

  Logical. If `TRUE`, return a small hardcoded example tibble instead of
  reading remote data. Default `FALSE`.

- historical_data:

  Optional harmonized historical CBS or production rows to add before
  the CBS historical extension. May be a data frame or a path to a
  parquet/csv file. CBS-shaped rows should provide `year`, `value`, one
  of `area_code` or `polity_area_code`, one of `item_cbs_code` or
  `item_prod_code`, and preferably `element`. Production-shaped rows
  without `element` are accepted as `production` when their unit is
  tonnes. **Rice supplied here is assumed to be on a paddy (rough-rice)
  basis** and is multiplied by the paddy-to-milled extraction rate,
  matching
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md);
  pre-divide by that rate if the series is already milled. Default
  `NULL`.

- format:

  One of `"long"` (default) or `"wide"`. `"long"` returns one row per
  element. `"wide"` pivots the elements into columns, adds the
  live-animal rows that the FAO sheet omits, and checks the supply-use
  identity. Both are the same dataset; `"wide"` is what the IO model and
  the extensions consume.

- trade_recovery:

  One of `"none"` (default) or `"net_import"`, selecting what happens to
  a traded item the CBS has no row for. The trade record is joined onto
  the CBS, so it can only fill a row that already exists; `"none"` keeps
  that, and the import is dropped. `"net_import"` first creates the
  missing rows from the trade record, restricted to tonnes-denominated
  items (live-animal trade is in heads and arrives through
  [`get_livestock_cbs()`](https://eduaguilera.github.io/whep/reference/get_livestock_cbs.md)),
  to net importers, and to areas the CBS already covers in that year.
  Selecting it **moves published values** — at 2010 it adds 1,154 keys
  and 53.7 Mt of imports (re-measured on the current build; 1,164 keys
  when whep#864 landed), and reclassifies three areas on the nourishment
  axis. `NEWS.md` states the rest, and whep#762 keeps the remaining
  decisions open.
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md),
  [`get_processing_coefs()`](https://eduaguilera.github.io/whep/reference/get_processing_coefs.md)
  and
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
  take the same argument and pass it into the shared build chain, each
  method under its own cache slot, so a downstream build can be run
  either way.

- trade_zero:

  One of `"prefer_record"` (default) or `"keep"`, selecting what happens
  when the CBS carries a **zero** import or export and the trade record
  for the same `(year, area_code, item_cbs_code)` carries a positive
  quantity. The trade record is filled in with
  [`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html),
  which replaces only a missing value, so the zero used to stand and the
  trade was discarded. `"prefer_record"` takes the positive trade
  quantity instead, because such a zero is not an observation: measured
  at 2010, every one of them carries FAO flag `"I"` (imputed) or the
  legacy `"S"` (standardized), and neither food-balance vintage carries
  a single official `"A"` on a trade row. A non-zero CBS value is never
  overwritten and a zero trade record never overwrites anything, so the
  fill can only add trade. `"keep"` restores the pre-whep#866 behaviour.
  **The default moves published values** — at 2010 it raises 4,493
  import keys by 9.70 Mt and 3,771 export keys by 10.27 Mt, moving
  26,538 published rows over 180 areas; see `NEWS.md`. The conflict
  count is reported by every build under either setting.

- share_overflow:

  One of `"report"` (default), `"clamp"`, `"drop"` or `"abort"`,
  selecting what happens when a pre-1962 destiny share exceeds 1 — a
  destiny larger than the `domestic_supply` it is apportioned from
  (whep#980). Measured on a real 1950–1965 build, 108 of 207,816 rows
  do: `other_uses` 70, `processing_primary` 19, `food` 15, `feed` 4.
  They are 1.03% of the 1961 `other_uses` mass and 0.17% of the `food`
  mass. The cause is not this arithmetic: 89 of them are FAOSTAT's own
  1961 balances not closing (the non-food Commodity Balances, which
  carry tobacco, hides and skins, silk, wool and fibres as `other_uses`
  and which no better-ranked source overwrites), and the other 19 are
  hops in net-exporting years, whose `processing_primary` is the whole
  production by construction — a trade ratio rather than an
  apportionment, which no setting can move, so the warning names that
  subset separately. `"report"` therefore keeps every value as measured
  and only warns, so it **moves no published value**; it names the
  count, the split by destiny and the three largest. `"clamp"` caps the
  share at 1, `"drop"` sets it to `NA` so the key is filled from a
  neighbouring year instead (and booked as 0 where the violating year is
  the only observation), and `"abort"` refuses to build. The share is an
  intermediate, not a published number: `.cbs_fill_destinies()` later
  re-derives each destiny as `domestic_supply` times a share normalised
  to sum to one. Re-measured under the current defaults
  (`negative_supply = "floor"`, `hist_trade_scale = "correct"`), no
  published row carries a destiny above its supply or a negative use
  under any setting, and every difference between the settings sits at
  1960 or earlier. Against `"report"`, `"clamp"` moves 766 rows
  (`other_uses` −0.91 Mt, `food` −1.00 Mt, `domestic_supply` −1.90 Mt)
  and `"drop"` moves 557 (`other_uses` −3.15 Mt, `domestic_supply` −3.18
  Mt), against 15,898 Mt of 1950–1960 `other_uses` and 15,411 Mt of
  `food`: at most 0.02%. The +23 Mt and +825 Mt once recorded here were
  whep#1065's negative supplies, which the `"floor"` default now
  removes. The reporting default is the one that invents nothing.

- negative_supply:

  One of `"floor"` (default), `"report"` or `"abort"`, selecting what
  happens when a pre-1962 row has no observed `domestic_supply` and the
  `production + import - export` reconstruction that replaces it comes
  out below zero (whep#1065). Every destiny of such a row is apportioned
  from that supply, so a negative one makes every destiny negative –
  including `other_uses`, which is not a quantity that can be negative.

  Measured on a real 1950–1965 build, 151 rows reconstruct a negative
  supply totalling −1,015.70 Mt, all in 1950–1960. The cause is
  upstream: US tobacco 1951 carries a 117,504,000 t export against
  728,949 t of production, 115,136,000 t of it
  `historical-trade-exports` item 831 (whep#1085). With
  `hist_trade_scale = "drop"` the count falls to 83 rows worth −6.87 Mt,
  some of which a stock draw can describe.

  `"floor"` clamps the reconstruction at zero, the treatment an observed
  negative supply already receives, and so books the exported mass the
  supply cannot source as a stock withdrawal: `stock_variation` is
  recomputed downstream as
  `production + import - export - domestic_supply`. It leaves no
  negative mass anywhere in the published balance. `"report"` keeps the
  negative supply and its destinies as computed, which is what every
  build before this default did; against `"floor"` it moves 3,911 rows,
  all in 1950–1960, and the published balance then carries 1,155
  negative masses worth −1,164.2 Mt, 147 of them `other_uses` worth
  −883.94 Mt (−3.9% of the 1950–1965 net `other_uses` total of 22,749
  Mt), with `stock_variation` 916 Mt smaller in magnitude. `"abort"`
  refuses to build any range starting before 1961.

  Neither `"floor"` nor `"report"` makes a 97× export physical;
  `"floor"` is the default because it keeps every published use
  non-negative and puts the imbalance in the balancing item. Both stay
  visible: the build warns with the count, the total and the largest
  rows; the values WHEP estimated for such a key carry the `source`
  `"historical_fill_floored_supply"` (or
  `"historical_fill_negative_supply"` under `"report"`); and any
  negative mass that reaches the published balance, from this or any
  other cause, is reported by a separate warning of class
  `whep_negative_cbs_value`.

- hist_trade_scale:

  One of `"correct"` (default), `"report"`, `"drop"` or `"abort"`,
  selecting what happens when a pre-1961 row of the `historical-trade-*`
  pins carries a quantity no mass unit can express (whep#1085). The
  screen bounds a single reporter's flow by the largest **world** flow
  FAOSTAT records for the same trade item, summed over reporters, over
  the FAOSTAT years the build already reads — a measured bound, not a
  chosen cap, because world trade in these commodities grew through the
  twentieth century. Re-measured on the real pins at 1850–2023 for
  whep#1117, 1,693 rows exceed it, carrying 3,874.4 Mt, 21.0% of the
  pins' whole 18,455.4 Mt; a further 10,243 rows have no FAOSTAT
  reference and go unchecked. **97.2% of the flagged mass is the USA**,
  whose block over roughly 1900–1960 is inflated by a factor of ten,
  interleaved with correct values in the same series (cotton lint 767
  and raw sugar imports 162 alternate correct and ten-fold values year
  to year; whep#1117), and by far more on item 831, "Tobacco products
  nes", published at 115.1 Mt for 1951 — 159x the largest world flow of
  that item FAOSTAT has ever recorded and 32x the entire 1961 world
  tobacco crop. `"report"` keeps every value and only warns, so it
  **moves no published value** (verified: the screened read is
  [`identical()`](https://rdrr.io/r/base/identical.html) to the
  unscreened one); it names the count, the mass, the reporters and the
  three largest. `"drop"` removes the flagged rows, taking 3,874.2 Mt
  out of the historical trade input and with it the impossible pre-1962
  exports behind whep#1065's negative `domestic_supply`. `"abort"`
  refuses to build. There is deliberately no clamp: the defect is in the
  pin's producer and no conversion factor recovers the true value, so a
  clamped tonnage would be a fabricated one.

  `"correct"`, the default (a maintainer decision), judges each flagged
  row on its own evidence from the two pins and repairs only a proven
  ten-fold slip (whep#1117): a row is divided by 10 when that brings it
  within the world bound, when as published it exceeds the whole partner
  side of the pins (the opposite flow of every other reporter, same item
  and year) but divided by 10 does not, and when it sits one power of
  ten above the clean neighbours of its own series within 5 years (the
  window is assumed, unverified). A row no partner books, or still above
  the partner side after dividing by 10, is dropped as not a mass (item
  831's signature); every other flagged row is dropped as unexplained.
  No factor other than 10 is ever applied. Re-measured on current `main`
  against FAOSTAT 1961–2023 (the bound a 1850–2023 build uses), 1,659
  rows carrying 3,958.8 Mt are flagged: 175 rows (1,002.0 Mt as
  published, 100.2 Mt used) are divided by 10, 1,126 (2,662.3 Mt) are
  dropped as not a mass and 358 (294.5 Mt) as unexplained; 99.9% of the
  corrected mass is the USA. On a real 1950–1960 build it leaves the
  same 83 negative reconstructed supplies (−6.87 Mt) as `"drop"`,
  against 151 (−1,015.70 Mt) under `"report"`, while keeping 103.4 Mt of
  repaired trade that `"drop"` discards. The output of every setting
  that builds carries a `hist_trade_scale_log` attribute (absent with
  `.fixed_data`): one row per flagged pin row, with the published and
  the used value, the evidence (`world_max`, `mirror`, `neighbour`), the
  class and the action.

  The warning also carries a second, informational class: rows larger
  than any flow FAOSTAT records for the **same reporter**, item and
  element. That bound is far tighter — 19,375 pre-1961 rows carrying
  9,154.0 Mt, against the world bound's 1,693 — and it is what makes
  whep#1117's ten-fold block visible at all. USA raw sugar imports
  (item 162) are the clearest case: 37 rows, 35–43 Mt over 1955–1959
  against the pins' own 4.7 Mt at 1960 and FAOSTAT's 3.7 Mt at 1961, and
  **not one of them is above the world bound**, so today nothing reports
  them. It is **never dropped and never aborts**, under any setting,
  because it provably mixes two populations that no offline anchor
  separates: the pins' 1961 layer is FAOSTAT verbatim (all 18,531
  overlapping rows agree to machine precision), FAOSTAT itself starts in
  1961, so no pre-1961 row has a per-row anchor, and nineteenth-century
  Britain genuinely imported more flax fibre, cotton lint, linseed and
  cheese than modern Britain does. A ÷10 repair is a change to the pin's
  producer, not to this reader.

- export_share_overflow:

  One of `"report"` (default), `"drop"` or `"abort"`, selecting what
  happens when the global export share the second processed-products
  round apportions a new product with exceeds 1 (whep#1086). The share
  is world `export / (production + import)` for the `(year, item_cbs)`
  key, multiplied by a country's newly created processed production, so
  a share above 1 books more export than that country produced. Measured
  on a real 1950–1965 build, 77 of 2,012 keys exceed 1 and the largest
  is 443 (Soyabean Cake 1956) — but **none of them is applied**: every
  one is pre-1961 and the round emits rows from 1961 on only, so
  `"report"` and `"drop"` give identical output on that range. **That
  does not hold after 2013** (whep#1177). On a real 2011–2023 build, 60
  keys above 1 are applied, every one of them in 2014–2023 (oilseed
  cakes and molasses, up to 15.7 for Sesameseed Cake 2016): the round
  books 2,297 rows with a negative `domestic_supply`, −18.33 Mt in
  total, and the finished balance under `"report"` carries 2.4–4.4 Mt
  more `export` a year than under `"drop"`, 33.6 Mt over 2014–2023,
  which `"drop"` books mostly as `feed`. Of the 77, 50 have no world
  production in the denominator at all (the oils and cakes, whose
  production is what this round is about to create) and the other 27 are
  the `historical-trade-exports` defect of whep#1085. `"report"` keeps
  every share as measured and names the count, the largest, and how many
  are actually applied. `"drop"` sets a violating share to `NA`, which
  is booked as no export at all. `"abort"` refuses to build. There is
  deliberately no clamp, unlike `share_overflow`: a destiny cannot
  exceed the supply it is apportioned from, so 1 is a true bound there,
  while here the denominator is incomplete and capping at 1 would book a
  country's whole processed output as export. Those figures are for the
  step-4 denominator, `export_share_basis = "snapshot"`; on a 2014–2023
  build the default `"current"` leaves 2 shares above 1 (Abaca 2021,
  Fish, Liver Oil 2017) and applies neither.

- export_share_basis:

  One of `"current"` (default), `"processed"` or `"snapshot"`, selecting
  which world balance the second processed-products round reads its
  export share `export / (production + import)` off (whep#1143). The
  share is multiplied by a country's newly created processed production,
  so its denominator must contain that kind of production. `"current"`
  reads the balance the round runs on and adds its rows to: processed
  production from the first round, trade after imputation. It is also
  the self-consistent choice, since a share `E / S` leaves the world
  share unchanged once the round's own production and export are added.
  `"processed"` reads the same production with trade as read, before
  imputation. `"snapshot"` reads the balance before any processed
  production exists, which is what every build before whep#1143 did:
  wherever FAOSTAT reports no production of a processed product — all of
  them before 1961, and the new food balance sheets' oilseed cakes and
  molasses from 2014 — its denominator is world import alone.

  **The default moves published values**, at 2014–2023 almost entirely.
  Measured on real 2014–2023 and 1961–1965 builds of main after
  whep#1242 against `"snapshot"`: at 2014–2023, 16,526 rows change,
  world `export` falls 36.1 Mt summed over the ten years (42.8 Mt gross)
  and `domestic_supply` rises by the same, landing 29.3 Mt on `feed`;
  applied shares above 1 fall from 60 to 0 (largest 15.7, Sesameseed
  Cake 2016). World Sesameseed Cake export at 2020 goes from 466 kt to
  1.6 kt against 0.4 kt of world import, Oilseed Cakes, Other from 3.17
  Mt to 2.13 Mt against 1.85 Mt. The step-4 sheet carries no cake or
  molasses production there at all. At 1961–1965 the change is 13 kt
  gross. `"processed"` moves the same cakes (export −39.3 Mt at
  2014–2023) and differs from `"current"` only on items whose trade step
  7 alone supplies: DDGS (+2.37 Mt of export under `"current"`) and
  Sugarbeet pulp (+0.60 Mt), for which `"processed"` books no export at
  all. Production is identical under all three.

- seed_backcast:

  One of `"area_rate"` (default) or `"production_share"`, selecting what
  the pre-1962 seed back-cast reads its rate off and spends it on
  (whep#699). The fill carries a rate along the year axis: a rate is
  read from the years that report `seed`, interpolated and extrapolated
  into the years that do not, and multiplied back out. It yields tonnes
  only if the quantity it is spent on is the quantity it was divided by.

  `"area_rate"` is tonnes of seed per hectare harvested,
  `seed / area_ha` spent as `area_ha * seed_rate`, and is the default: a
  seeding rate is an agronomic quantity that holds while yields change,
  so it is the ratio worth carrying across decades. `"production_share"`
  is tonnes of seed per tonne of output, `seed / production` spent as
  `production * seed_rate`; it moves with yield, but reaches keys the
  production build gives no harvested area.

  **The default moves published values**, because neither is what
  shipped: a rate defined per hectare used to be spent on production,
  giving `t x t/ha`. Measured on a real 1850-2023 build, pre-1962 `seed`
  falls from 38.62 Gt to 7.13 Gt while the number of keys carrying one
  rises from 87,882 to 118,332, total tonnage moves -0.934% and pre-1962
  tonnage -2.716%, and no 1962-or-later value changes at all. World seed
  at 1960 goes from 680.3 Mt to 77.7 Mt against the 126.1 Mt FAOSTAT
  reports for 1961, and
  [`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md)
  on `seed` over 1950-1970 falls from 582 jumps to 176, of which the
  1960-1961 seam holds 3 rather than 295. Under `"production_share"`
  pre-1962 `seed` is 5.22 Gt, total tonnage moves -1.023%, and the seam
  holds 5 jumps.

- unmatched_processing:

  One of `"other_uses"` (default), `"processing"` or `"redistribute"`,
  selecting where a `processing` destiny goes when its item has no
  pathway in
  [cb_processing](https://eduaguilera.github.io/whep/reference/cb_processing.md),
  so no processed product exists for the mass to become (whep#781).
  Measured on a real 2010 build this is 15.92 Mt over 25 items, 10.10 Mt
  of it raw sugar.

  `"other_uses"` books it on `other_uses`, the destiny the processing
  shortfall of an item that *has* a pathway already goes to, and leaves
  `food`, `feed` and `export` as FAOSTAT reported them. `"processing"`
  keeps FAOSTAT's row as a terminal destiny; the balance still closes,
  but
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
  folds processing that no product consumes into `food`.
  `"redistribute"` is the behaviour before whep#781: the mass is split
  pro rata over `food`, `feed`, `other_uses` and `export`.

  **The default moves published values.** Against `"redistribute"` at
  2010, world `food` falls 10.82 Mt, `feed` 0.56 Mt and `export` 3.05
  Mt, while `other_uses` rises 14.43 Mt and `domestic_supply` 3.05 Mt,
  because the export share had moved domestic processing out of the
  country. Food then sits within 0.6% of FAOSTAT's for every affected
  item but coconut oil, against 6.0% for raw sugar, 14.4% for cottonseed
  oil and 40.9% for ricebran oil before. Which destiny is right is open
  — see whep#781 — and a sourced pathway per item would supersede all
  three.

- silk_basis:

  One of `"cocoon"` (default), `"raw_silk"` or `"mixed"`, selecting the
  mass basis of the Silk balance from 2014 on (whep#1251). Silk is a
  FAOSTAT chain – reelable cocoons (1185) reeled into raw silk (1186),
  plus silk waste (1187) – that the non-food Commodity Balances report
  link by link, each in its own mass, and that WHEP maps onto one item.
  Summed unconverted, with the cocoons sent to reeling dropped as a
  chain transfer, the reeled cocoons were left as stock build-up: 267 kt
  of `stock_variation` against 536 kt of production at 2020, 391 kt
  against 517 kt at 2021, measured on a real 2019-2021 build.

  `"cocoon"` counts production once, as cocoons, books the cocoons
  reeled at FAO's own cocoon mass, and converts only the raw silk that
  crossed a border or a stock, dividing by a raw-silk extraction rate of
  0.16 – the midpoint of the 12-20% of fresh cocoon weight in Lee
  (1999), *Silk reeling and testing manual*, FAO Agricultural Services
  Bulletin 136; FAO's Technical Conversion Factors carry no silk entry.
  `"raw_silk"` is the same balance multiplied by that rate, so every
  Silk quantity depends on it. `"mixed"` keeps each link's own mass and
  books the reeled cocoons as `other_uses`, the convention of FAO's
  pre-2014 aggregate item 2747: the balance closes but raw silk is
  counted twice, once as the cocoons it was reeled from. Silk waste
  keeps its own mass under every setting.

  Measured at 2020 on that build, production is 443 / 71 / 536 kt
  (cocoon / raw_silk / mixed) and `stock_variation` -170 / -32 / -164
  kt, of which -157 kt is one FAOSTAT record under every setting: China
  mainland's 2020 cocoons are booked both as `Processed` and as
  `Other uses` (FAOSTAT's own `Residuals` is -156,943 t). No non-Silk
  row moves. Years before 2014 come from the aggregated old Commodity
  Balances, which carry no link breakdown, and are unchanged, so under
  `"cocoon"` the 2013-2014 seam steps by roughly the raw silk
  production.

- tobacco_leaf_use:

  One of `"as_published"` (default) or `"one_to_one"`, selecting how the
  Tobacco balance from 2014 on treats leaf manufactured into products
  (whep#1390). The non-food Commodity Balances book the leaf (826) that
  goes into a factory as leaf `other_uses`, and the cigarettes, cigars
  and other manufactured tobacco (828, 829, 831) made from it are used
  or exported again. Their production is not booked as supply
  (whep#1276), so the summed uses exceed supply by about the
  manufactured output, and the balance closes through a stock withdrawal
  with no stock behind it – about 54 kt a year for the Netherlands and
  Ukraine over 2019-2021.

  `"as_published"` keeps FAOSTAT's leaf `other_uses`, phantom withdrawal
  included. `"one_to_one"` subtracts the products' production from the
  leaf `other_uses`, floored at zero, assuming one tonne of leaf per
  tonne of product – assumed, unverified: no sourced leaf content per
  tonne of product was found, and a cigarette also holds paper and
  filter. Where the products outweigh the leaf use (Ukraine 2019: 56 kt
  against 29.7 kt) the remainder stays as stock change. Only Tobacco
  rows from 2014 on move.

- .fixed_data:

  Optional tibble with the same structure as the output of the internal
  `.read_cbs() |> .fix_cbs()` steps. When supplied, `primary_all` is
  ignored and the pipeline skips directly to `.qc_cbs()`. Default
  `NULL`.

## Value

For `format = "long"`, a tibble with columns: `year`, legacy numeric
`area_code`, numeric `polity_area_code`, `reporting_polity_code`,
`reporting_polity_name`, `reporting_polity_has_geometry`,
`item_cbs_code`, `element` (e.g. `"production"`, `"import"`, `"food"`),
`value`, `source`, and `fao_flag`. For `format = "wide"`, the elements
become one column each, `stock_variation` is split into the non-negative
`stock_addition` and `stock_withdrawal`, and `domestic_supply` is total
use excluding `export`. A `unit` column says each row's denomination:
`"tonnes"`, or `"heads"` for the live-animal rows (see
[`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)).

`fao_flag` is FAOSTAT's own observation-status code for the value, taken
from the source that `source` names (`"A"` official, `"E"` estimated,
`"I"` imputed, `"S"` standardized, `"SD"`, `"X"`). It is `NA` wherever
the number is not one FAOSTAT published under a flag: a WHEP-derived row
(the processing pathway, the destiny gap-fills, the pre-1961 historical
extension), a row whose source carries no flag, and a row summed or
averaged from parts whose flags disagree. The flag is a claim about the
value, so it is dropped rather than guessed when the parts do not agree.
Rows sourced from `"FAOSTAT_prod"` are `NA` today because
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
does not carry the flag out of the production pin.

## Examples

``` r
build_commodity_balances(example = TRUE)
#> # A tibble: 10 × 11
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name 
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                 
#>  1  2010       120              120 LAO-1954-2025         Laos                  
#>  2  1981       222              222 TUN-1881-2025         Tunisia               
#>  3  1906       203              203 ESP-1800-2025         Spain                 
#>  4  1899       175              175 GNB-1886-1974         Guinea-Bissau (1886-1…
#>  5  2018        48               48 CRI-1800-2025         Costa Rica            
#>  6  1871        10               10 AUS-1901-2025         Australia             
#>  7  1938       226              226 UGA-1926-1962         Uganda (1926-1962)    
#>  8  1924        11               11 AUT-1919-2025         Austria               
#>  9  1928        96               96 HKG-1842-2025         Hong Kong             
#> 10  1879       236              236 VEN-1821-2025         Venezuela             
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <dbl>,
#> #   element <chr>, value <dbl>, source <chr>, fao_flag <chr>
```
