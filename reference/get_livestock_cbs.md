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
get_livestock_cbs(primary_prod)
```

## Arguments

- primary_prod:

  Tibble from
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md).

## Value

A tibble with the same columns as
[`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).
