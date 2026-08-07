# Typologies of Josette

Typologies of provinces in Spain based on nitrogen (N) production data
of crops and livestock, considering multiple data inputs and producing
classification maps and data frames.

## Usage

``` r
create_typologies_of_josette(
  make_map = TRUE,
  shapefile_path = NULL,
  map_year = 1980,
  example = FALSE
)
```

## Arguments

- make_map:

  If TRUE a map of the typologies will be created.

- shapefile_path:

  Optional path to a Natural Earth 10m admin-1 states/provinces
  shapefile. When `NULL` (default) the layer is downloaded from
  <https://www.naturalearthdata.com> on first use and cached locally;
  set `options(whep.provinces_shapefile = )` to point at an existing
  copy instead.

- map_year:

  The year for which the typology map is created.

- example:

  If `TRUE`, return a small example output without reading the remote
  inputs or the Natural Earth layer. Default is `FALSE`. The example
  output carries the three data elements only, not the `df_inputs_plots`
  plot, so that it needs no plotting package.

## Value

A named list with `typologies_df` (the typology classification per year
and province), `n_input_df` (the N soil inputs the classification read),
`imported_feed_share_df` (the imported-feed share per year and province)
and `df_inputs_plots` (a `ggplot` of N inputs by typology).

## Examples

``` r
create_typologies_of_josette(example = TRUE)$typologies_df
#> # A tibble: 10 × 3
#>     Year Province_name Typology                             
#>    <dbl> <chr>         <chr>                                
#>  1  2020 A_Coruna      Forage-based crop & livestock system 
#>  2  2020 Albacete      Specialized stockless cropping system
#>  3  2020 Alicante      Urban system                         
#>  4  2020 Almeria       Forage-based crop & livestock system 
#>  5  2020 Araba         Specialized stockless cropping system
#>  6  2020 Asturias      Grass-based crop & livestock system  
#>  7  2020 Avila         Forage-based crop & livestock system 
#>  8  2020 Badajoz       Forage-based crop & livestock system 
#>  9  2020 Barcelona     Urban system                         
#> 10  2020 Bizkaia       Urban system                         
```
