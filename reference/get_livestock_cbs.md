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
  method_head_units = c("convert", "drop", "abort")
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

## Value

A tibble with the same columns as
[`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).
