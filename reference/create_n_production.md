# N production for Spain

Calculates N production at the provincial level in Spain. Production is
derived from consumption, export, import, and other uses.

## Usage

``` r
create_n_production(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble containing:

- `year`: Year

- `province_name`: Spanish province

- `item`: Product item

- `box`: Ecosystem box

- `prod`: Produced N (Mg)

## Examples

``` r
create_n_production(example = TRUE)
#> # A tibble: 10 × 5
#>     year province_name item                  box                 prod
#>    <dbl> <chr>         <chr>                 <chr>              <dbl>
#>  1  1931 Valencia      Meat, Other           Livestock       36.5    
#>  2  1990 Granada       Nuts and products     Cropland       532      
#>  3  1957 Teruel        Tomatoes and products Cropland         9.19   
#>  4  1898 Barcelona     Brans                 Cropland         0.518  
#>  5  1943 Barcelona     Urea                  Agro-industry    1.66   
#>  6  1953 Lleida        Demersal Fish         Fish             0      
#>  7  1904 Salamanca     Wheat and products    Cropland      1840      
#>  8  2015 Burgos        Sweet potatoes        Cropland         0.00461
#>  9  1988 Zaragoza      DDGS Barley           Cropland       619      
#> 10  2014 Soria         Crustaceans           Fish             0      
```
