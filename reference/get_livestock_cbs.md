# Livestock commodity balance sheet entries

Build CBS rows for live animals from primary production data and
bilateral trade. Live animals are not included in the FAO commodity
balance sheet but are needed as explicit intermediates in the IO model.

Following the FABIO methodology, live-animal production is estimated
from slaughter counts as `slaughtered + exported - imported` (animals
raised in the country), and domestic supply (`processing`) equals
`production + import - export`. Only live animals with explicit
slaughter-product outputs are added; other animal products are supplied
directly by husbandry.

Units are heads (number of animals).

## Usage

``` r
get_livestock_cbs(
  primary_prod,
  method_head_units = c("convert", "drop", "abort"),
  method_cull = c("fold", "separate")
)
```

## Arguments

- primary_prod:

  Tibble from
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md).

- method_head_units:

  How the live-animal trade this balance rests on treats FAOSTAT's
  `1000 Head` rows. Passed to
  [`build_detailed_trade()`](https://eduaguilera.github.io/whep/reference/build_detailed_trade.md)'s
  helper of the same name; see its *Live animals are reported in two
  head units* section. `"convert"` (default) rescales them by 1,000 onto
  `heads`, `"drop"` discards them with a warning, `"abort"` refuses.

- method_cull:

  Where the slaughter FAOSTAT books on dairy cattle (960) and laying
  hens (1052) goes. FAOSTAT reports one slaughter count per species
  (cattle 866, chickens 1057); WHEP splits it between the dairy / layer
  and the non-dairy / broiler stock sub-items by stock share, while
  beef, poultry meat and live-animal trade are all keyed on 961 and
  1053.

  - `"fold"` (default): count that slaughter toward the live-animal
    balance of 961 (non-dairy cattle) and 1053 (broilers), next to the
    meat and the trade it belongs with. The balance then holds FAOSTAT's
    whole observed slaughter. The cost: in the live-animal balance,
    culled dairy cows and spent hens are counted as raised by the
    non-dairy / broiler sector.

  - `"separate"`: give 960 and 1052 a live-animal balance of their own,
    in which production and processing are the cull and trade is zero
    (FAOSTAT does not split live-animal trade by sub-item). Total
    slaughter is the same. No slaughtering process consumes those rows,
    because the meat of culled animals is still keyed on 961 / 1053, but
    [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
    reads CBS `production` as the 960 / 1052 output.

  Breeding swine (1051) are always folded onto 1049 (whep#1149): 1051
  has no sector of its own to keep a balance on. The choice is recorded
  in the `method_cull` column.

## Value

A tibble with the same columns as
[`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md),
plus `method_cull`.
