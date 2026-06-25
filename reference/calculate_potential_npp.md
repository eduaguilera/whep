# Estimate potential net primary production.

Estimates potential net primary production per unit area, used
downstream to scale weed biomass on cropland. Several methods are
available; the default `lpjml` reads gross managed-grassland NPP from a
finished LPJmL run, while the climate methods compute potential NPP from
temperature, water input and actual evapotranspiration.

## Usage

``` r
calculate_potential_npp(
  x,
  method = c("lpjml", "miami", "nceas", "rosenzweig"),
  lpjml = list()
)
```

## Arguments

- x:

  A tibble. The climate methods require `temp_c` (mean annual
  temperature, degrees C), `water_input_mm` (precipitation plus
  irrigation) and `aet_mm` (actual evapotranspiration). The `lpjml`
  method instead needs a spatial key to join the gridded grass NPP (see
  `lpjml`).

- method:

  Potential-NPP method, one of `"lpjml"` (default), `"miami"`, `"nceas"`
  or `"rosenzweig"`.

- lpjml:

  A named list of options for the `lpjml` method (passed to the LPJmL
  grass reader). Ignored by the climate methods.

## Value

The input tibble with `npp_potential_dm_t_ha` (potential NPP, t DM per
hectare) and `method_npp_potential` (the method used).

## Examples

``` r
calculate_potential_npp(
  tibble::tibble(temp_c = 15, water_input_mm = 800, aet_mm = 700),
  method = "miami"
)
#> # A tibble: 1 × 5
#>   temp_c water_input_mm aet_mm npp_potential_dm_t_ha method_npp_potential
#>    <dbl>          <dbl>  <dbl>                 <dbl> <chr>               
#> 1     15            800    700                  12.4 miami               
```
