# System NUE

Calculates the NUE for Spain at the provincial level. The system NUE is
defined as the percentage of total nitrogen production (`total_prod`)
relative to the sum of all nitrogen inputs (`inputs`) into the soil
system.

## Usage

``` r
calculate_system_nue(n_soil_inputs = create_n_soil_inputs(), example = FALSE)
```

## Arguments

- n_soil_inputs:

  A tibble of nitrogen soil input (deposition, fixation, synthetic,
  manure, urban). If not provided and `example = FALSE`, it will be
  computed from
  [`create_n_soil_inputs()`](https://eduaguilera.github.io/whep/reference/create_n_soil_inputs.md).

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with the following columns:

- `year`: Year

- `province_name`: Spanish province

- `total_prod`: Total nitrogen production (Mg)

- `inputs`: Total nitrogen inputs (Mg)

- `nue_system`: System-level Nitrogen Use Efficiency (%)

## Examples

``` r
calculate_system_nue(example = TRUE)
#> # A tibble: 10 × 5
#>     year province_name total_prod inputs nue_system
#>    <dbl> <chr>              <dbl>  <dbl>      <dbl>
#>  1  1917 Tarragona           4740   9924       47.8
#>  2  1989 A_Coruna           27403  52128       52.6
#>  3  1967 Tenerife            2080  11614       17.9
#>  4  2010 Albacete           18671  70849       26.4
#>  5  1923 Albacete            8767  19187       45.7
#>  6  1968 Salamanca          11821  34294       34.5
#>  7  1932 Palencia            6206  15537       39.9
#>  8  1944 Almeria             2538  12783       19.9
#>  9  1911 Avila               4112  16873       24.4
#> 10  1893 Malaga              3296  13713       24  
```
