# Build detailed bilateral trade matrix

Construct the detailed bilateral trade matrix (DTM) from the FAOSTAT
Detailed Trade Matrix pin. Reports trade flows between pairs of
countries with their trade shares, aggregated to polity level and mapped
to CBS item codes.

Optionally extends the time series by joining with commodity balance
sheet years and gap-filling country shares via linear interpolation.

## Usage

``` r
build_detailed_trade(
  raw_trade = NULL,
  cbs = NULL,
  min_share = 1e-04,
  extend_time = FALSE,
  method_unbacked_quantity = c("drop", "keep", "abort"),
  method_head_units = c("convert", "drop", "abort"),
  method_time_coverage = c("cbs_cells", "cbs_years"),
  example = FALSE
)
```

## Arguments

- raw_trade:

  A data.table or tibble of raw FAOSTAT bilateral trade data. If `NULL`
  (default), the data is read from the `"faostat-trade-bilateral"` pin.

- cbs:

  A tibble of commodity balance sheets in wide format, as returned by
  [`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
  or
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).
  Required when `extend_time = TRUE`.

- min_share:

  Numeric. Partners with a country share below this threshold are
  dropped when extending time. Default `0.0001`.

- extend_time:

  Logical. If `TRUE`, extend the time series using CBS years and linear
  interpolation of country shares. Default `FALSE`.

- method_unbacked_quantity:

  How to treat a reported `tonnes` quantity for a FAOSTAT trade item
  whose country-level mass FAOSTAT itself does not publish. See the
  *Quantities FAOSTAT does not back with a mass* section. One of:

  - `"drop"` (default): discard those rows, warning with the tonnage
    removed. They are not masses, and no conversion to mass is
    derivable.

  - `"keep"`: carry them verbatim, with the same warning. The historical
    behaviour, and unsafe for anything that treats the column as mass.

  - `"abort"`: fail, so a refreshed pin cannot reintroduce them
    unnoticed.

- method_head_units:

  How to treat the FAOSTAT rows denominated in `1000 Head`. See the
  *Live animals are reported in two head units* section. One of:

  - `"convert"` (default): rescale them by 1,000 onto `heads`, the
    denomination the rest of the live-animal record already uses.

  - `"drop"`: discard them, warning with the head count removed. The
    historical behaviour, which leaves live poultry, rabbit and rodent
    trade out of the record entirely.

  - `"abort"`: fail, so a refreshed pin cannot reintroduce an unhandled
    unit unnoticed.

  Any other unit label is dropped under every method, with a
  `"whep_unhandled_trade_unit"` warning naming it, and aborts under
  `"abort"`. Monetary units are removed silently, on purpose.

- method_time_coverage:

  Which extended shares `extend_time` keeps. See the *Time extension and
  CBS coverage* section. One of:

  - `"cbs_cells"` (default): only in the year, area, item and element
    cells where `cbs` reports a non-zero flow.

  - `"cbs_years"`: in every CBS year, whether or not CBS reports the
    cell. The historical behaviour.

  Ignored when `extend_time = FALSE`.

- example:

  Logical. If `TRUE`, return a small example tibble without downloading
  remote data. Default `FALSE`.

## Value

A tibble with columns:

- `year`: Integer year.

- `area_code`: Numeric polity code of the reporter country.

- `area_code_partner`: Numeric polity code of the partner country.

- `element`: Either `"import"` or `"export"`.

- `item_cbs_code`: Numeric CBS item code.

- `unit`: Measurement unit (`"tonnes"` or `"heads"`).

- `value`: Trade quantity.

- `country_share`: Share of total trade for this partner.

- `method_unbacked_quantity`: the treatment chosen for quantities
  FAOSTAT does not back with a mass, recorded so a downstream consumer
  can tell which variant it is holding.

- `method_head_units`: the treatment chosen for the `1000 Head` rows,
  recorded for the same reason.

- `method_time_coverage`: the coverage rule of the time extension, or
  `NA` when `extend_time = FALSE`.

## Time extension and CBS coverage

With `extend_time = TRUE` every `(area, item, partner, element, unit)`
group observed in any trade year is carried across the union of trade
and CBS years, and
[`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md)
interpolates inside a group's observed span and holds the first and last
observed share constant outside it. That fills two kinds of row: years
outside the trade record, and trade years in which the reporter reported
nothing at all for that item and element (the group's share there is
0/0). `method_time_coverage` then decides which of those filled shares
are kept:

- `"cbs_cells"` (default) keeps an extended share only in a
  `(year, area_code, item_cbs_code, element)` cell where `cbs` reports a
  non-missing, non-zero flow for that element. A share partitions a CBS
  total among partners; where there is no total there is nothing to
  partition, so no share is invented there. Observed trade rows are
  always kept, whether or not CBS reports the cell.

- `"cbs_years"` keeps every extended share in every CBS year. The
  historical behaviour: the CBS *year axis* alone drives the extension,
  so shares are also emitted for cells CBS never reports.

The two methods differ only in which rows survive: a share kept by both
is identical, because it comes from the same interpolation of the
group's own anchors, and every kept cell keeps all its partners, so its
shares still sum to one. Because filled rows inside the trade record are
scoped too, a `cbs` that omits some trade years drops the filled rows of
those years under `"cbs_cells"`; pass a CBS spanning the trade record.

Measured on the `"faostat-trade-bilateral"` pin against a 1850-2023 CBS,
half of the `(year, area, item, element)` cells the uniform extension
emits are cells CBS never reports (3.42 of 6.85 million; whep#232).
Against single-year CBS builds, `"cbs_cells"` keeps 144,812 of the
286,762 rows `"cbs_years"` emits for 1975 (50.5%) and 342,617 of 384,954
for 2023 (89.0%), extending from trade years 1986-1987 and 2020-2021;
and 231,530 of 249,833 for 2000 (92.7%) and 279,210 of 293,815 for 2010
(95.0%), with trade years 1999-2001 and 2009-2011.

Neither method bounds the **year axis**. The pin covers 1986-2021, while
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
defaults to 1850-2023, so 138 of the 174 extended years (79%) lie
outside the trade record entirely and carry the 1986 (or 2021) partner
mix held constant. CBS is itself extended back to 1850, so scoping to
its cells does not shorten that back-cast; pass a year-scoped `cbs` to
limit it.

## Quantities FAOSTAT does not back with a mass

The Detailed Trade Matrix carries a `tonnes` column for every item, but
FAOSTAT does not stand behind all of it. For trade item 1293 (*Crude
organic material n.e.c.*, mapped here to CBS item 5001 `"Other"`) the
aggregate *Trade: Crops and livestock products* domain publishes a
**value** but no country-level **mass**: on the `faostat-trade-totals`
pin `20260325T120525Z-7b85f` that item has 28,213 value rows against
6,000 quantity rows, only 1,830 of them non-zero, and every quantity
above 10 Mt belongs to a FAOSTAT *regional aggregate* (area code \>=
51000) flagged `E`. Spot-checked for Colombia, Kenya, South Africa and
Japan over 2002-2010, the country-level quantity is `0` or absent in
every year.

The detailed matrix nonetheless reports masses for it that cannot be
masses (whep#1023). On the `faostat-trade-bilateral` pin
`20260407T095142Z-b3f81`, Colombia's 2004 export of item 1293 to the
United States is 2,579,549,000 tonnes against an export value of USD
584.4 million, i.e. **USD 0.227 per tonne**; South Africa's 2008-2013
flows with Uganda run at USD 0.011-0.015 per tonne. The mirrored report
of the same flow disagrees by factors of 356 to 838,000 - Kenya says it
exported 534,611,300 tonnes to the Netherlands in 2009, the Netherlands
says it imported 72,076 tonnes (7,417x) - and it is always the large
side that is not a mass. What the large side actually counts (stems,
pieces, bunches) is **not recorded anywhere in the data and is therefore
unverified**, and the implied units per tonne are not constant across
cells, so no conversion factor can be derived: the figures can be
dropped or carried, not corrected.

`method_unbacked_quantity` selects the treatment. Screened tonnage on
the full pin, restricted to items that map to a CBS item: 15.31 Gt of
83.16 Gt (**18.4%**), 99.78% of it item 1293. By year the screen takes
12.7-70.0% of 2003-2013, 5.6-16.5% of 1986-1988, 10.1-13.0% of
2000-2002, at most 3.3% of 1989-1999 and at most 0.04% of 2014-2021 -
item 1293 carries no bilateral quantity at all from 2014, which is why
the recent record looks clean.

Those tonnes do not reach this function's output today whichever method
is chosen, for an unrelated reason: item 1293's CBS name `"Other"` has
no row in
[`whep::items_full`](https://eduaguilera.github.io/whep/reference/items_full.md),
so the item-code bridge leaves it without an `item_cbs_code` and it is
dropped a step later. That was silent until now and is warned about
separately; four CBS names and 14 of the 710 trade item codes in
[`whep::cbs_trade_codes`](https://eduaguilera.github.io/whep/reference/cbs_trade_codes.md)
are affected. The screen is therefore explicit where the mapping gap was
accidental, and it keeps working if the gap is ever filled.

## Live animals are reported in two head units

FAOSTAT denominates live-animal trade in `Head` for the large species
and in `1000 Head` for the small ones, and the unit filter here used to
cover only the first label. On the `faostat-trade-bilateral` pin
`20260407T095142Z-b3f81` that silently removed **89,073 rows carrying
76,141,882 thousand head** over 1986-2021 - live chicken, turkey, duck,
goose, other-bird, rabbit and rodent trade - against the 11,708,244,416
head the `Head` rows carry. It also removed 5,011 `No` rows (whep#1092).

`1000 Head` is a decimal prefix the source states, not a coefficient:
`.aggregate_fao_trade_to_cbs()` already rescales its sibling label
`1000 An` the same way (whep#865). The reported trade *value* of the
same flows corroborates it - USD 1,408 per `1000 Head` of chickens,
2,392 for ducks, 2,483 for geese, 3,469 for turkeys and 4,945 for
rabbits, i.e. USD 1.41-4.95 a bird, against USD 654 per head of cattle,
105 per pig and 74 per sheep on the `Head` rows. Read as single head, a
live broiler chick would cost USD 1,408. **No head-to-mass factor is
involved**: head counts stay head counts, on their own `unit` key.

The `No` rows are a different case and are **not** converted. They are
FAOSTAT trade item 1181, which
[`whep::cbs_trade_codes`](https://eduaguilera.github.io/whep/reference/cbs_trade_codes.md)
names *Beehives* while the pin names it *Bees*, so what the number
counts - insects, packages or colonies - is not established by the
source. They are dropped, with the warning every unrecognised label now
raises.

## Examples

``` r
build_detailed_trade(example = TRUE)
#> # A tibble: 10 × 19
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <int>     <int>            <int> <chr>                 <chr>                
#>  1  2010         4                4 DZA-1962-2025         Algeria (1962-2025)  
#>  2  2010         4                4 DZA-1962-2025         Algeria (1962-2025)  
#>  3  2015       100              100 IND-1949-2025         India                
#>  4  2015       100              100 IND-1949-2025         India                
#>  5  2018        79               79 DEU-1990-2025         Germany              
#>  6  2018        79               79 DEU-1990-2025         Germany              
#>  7  2005         4                4 DZA-1962-2025         Algeria (1962-2025)  
#>  8  2005         4                4 DZA-1962-2025         Algeria (1962-2025)  
#>  9  2012       100              100 IND-1949-2025         India                
#> 10  2012       100              100 IND-1949-2025         India                
#> # ℹ 14 more variables: reporting_polity_has_geometry <lgl>,
#> #   area_code_partner <int>, partner_polity_code <chr>,
#> #   partner_polity_name <chr>, partner_polity_has_geometry <lgl>,
#> #   partner_polity_area_code <int>, element <chr>, item_cbs_code <int>,
#> #   unit <chr>, value <dbl>, country_share <dbl>,
#> #   method_unbacked_quantity <chr>, method_head_units <chr>,
#> #   method_time_coverage <chr>
```
