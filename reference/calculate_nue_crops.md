# N soil inputs and Nitrogen Use Efficiency (NUE) for crop

N inputs (deposition, fixation, synthetic fertilizers, urban sources,
manure) and N production in Spain from 1860 to the present for the GRAFS
model at the provincial level. The crop NUE is defined as the percentage
of produced nitrogen relative to the total nitrogen inputs to the soil.
Total soil inputs are calculated as: inputs = deposition + fixation +
synthetic + manure + urban

## Usage

``` r
calculate_nue_crops(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble containing nitrogen use efficiency (NUE) for crops. It includes
the following columns:

- `year`: Year.

- `province_name`: The Spanish province.

- `item`: The item which was produced, defined in `names_biomass_cb`.

- `box`: One of the two systems of the GRAFS model: cropland or
  semi-natural agroecosystems.

- `nue`: Nitrogen Use Efficiency as a percentage (%).

## Examples

``` r
calculate_nue_crops(example = TRUE)
#> # A tibble: 10 × 5
#>     year province_name item                       box                        nue
#>    <dbl> <chr>         <chr>                      <chr>                    <dbl>
#>  1  1937 Tenerife      Oranges, Mandarines        Cropland                97.3  
#>  2  1905 Cantabria     Apples and products        Cropland                59.8  
#>  3  2005 Badajoz       Firewood                   semi_natural_agroecos…   0.345
#>  4  1968 Murcia        Millet and products        Cropland               746    
#>  5  1943 Gipuzkoa      Hard Fibres, Other         Cropland                59.4  
#>  6  1954 Malaga        Firewood                   semi_natural_agroecos…   0.491
#>  7  1973 Lugo          Tomatoes and products      Cropland                28.4  
#>  8  1953 Almeria       Lemons, Limes and products Cropland                24.4  
#>  9  1860 Lleida        Pulses, Other and products Cropland                68.3  
#> 10  2015 Valencia      Grapefruit and products    Cropland                 6.39 
```
