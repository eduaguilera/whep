# Nitrogen (N) soil inputs for Spain

Calculates total nitrogen inputs to soils in Spain at the provincial
level. This includes contributions from:

- Atmospheric deposition (`deposition`)

- Biological nitrogen fixation (`fixation`)

- Synthetic fertilizers (`synthetic`)

- Manure (excreta, solid, liquid) (`manure`)

- Urban sources (`urban`)

Special land use categories and items are aggregated:

- Semi-natural agroecosystems (e.g., Dehesa, Pasture_Shrubland)

- Firewood biomass (e.g., Conifers, Holm oak)

## Usage

``` r
create_n_soil_inputs(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble containing:

- `year`: Year

- `province_name`: Spanish province

- `item`: Crop, land use, or biomass item

- `irrig_cat`: Irrigation form (irrigated or rainfed)

- `box`: Land use or ecosystem box for aggregation

- `deposition`: N input from atmospheric deposition (Mg)

- `fixation`: N input from biological N fixation (Mg)

- `synthetic`: N input from synthetic fertilizers (Mg)

- `manure`: N input from livestock manure (Mg)

- `urban`: N input from urban sources (Mg)

## Examples

``` r
create_n_soil_inputs(example = TRUE)
#> # A tibble: 10 × 10
#>     year province_name item        irrig_cat box   deposition fixation synthetic
#>    <dbl> <chr>         <chr>       <chr>     <chr>      <dbl>    <dbl>     <dbl>
#>  1  1976 Burgos        Wheat and … Irrigated Crop…    16.5      13      184     
#>  2  1912 Alicante      Apples and… Irrigated Crop…     0.0198    0.159    0.0132
#>  3  1974 Lleida        Fallow      Rainfed   Crop…   263       151        0     
#>  4  1947 Caceres       Fodder cer… Rainfed   Crop…    27.5      80        0.0214
#>  5  1924 Huesca        Wheat and … Irrigated Crop…    34.2     157      156     
#>  6  1953 Huesca        Apples and… Rainfed   Crop…     0.441     1.04     3.35  
#>  7  1901 Avila         Cereals, O… Rainfed   Crop…     0.164     0.729    0.0448
#>  8  1909 Cordoba       Beans       Irrigated Crop…     0.207     4.86     0.0818
#>  9  1931 Leon          Pulses, Ot… Rainfed   Crop…     8.04    148        3.11  
#> 10  1950 Navarra       Apples and… Irrigated Crop…     0.806     2.4      1.34  
#> # ℹ 2 more variables: manure <dbl>, urban <dbl>
```
