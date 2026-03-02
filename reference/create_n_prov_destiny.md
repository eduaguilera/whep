# GRAFS Nitrogen (N) flows

Provides N flows of the spanish agro-food system on a provincial level
between 1860 and 2020. This dataset is the the base of the GRAFS model
and contains data in megagrams of N (MgN) for each year, province, item,
origin and destiny. Thereby, the origin column represents where N comes
from, which includes N soil inputs, imports and production. The destiny
column shows where N goes to, which includes export, population food,
population other uses and feed or cropland (in case of N soil inputs).
Processed items, residues, woody crops, grazed weeds are taken into
account.

## Usage

``` r
create_n_prov_destiny(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A final tibble containing N flow data by origin and destiny. It includes
the following columns:

- `year`: The year in which the recorded event occurred.

- `province_name`: The Spanish province where the data is from.

- `item`: The item which was produced, defined in `names_biomass_cb`.

- `irrig_cat`: Irrigation form (irrigated or rainfed)

- `box`: One of the GRAFS model systems: cropland, Semi-natural
  agroecosystems, Livestock, Fish, or Agro-industry.

- `origin`: The origin category of N: Cropland, Semi-natural
  agroecosystems, Livestock, Fish, Agro-industry, Deposition, Fixation,
  Synthetic, People (waste water), Livestock (manure).

- `destiny`: The destiny category of N: population_food,
  population_other_uses, livestock_mono, livestock_rum (feed), export,
  Cropland (for N soil inputs).

- `mg_n`: Nitrogen amount in megagrams (Mg).

## Examples

``` r
create_n_prov_destiny(example = TRUE)
#> # A tibble: 10 × 8
#>     year province_name item               irrig_cat box   origin destiny    mg_n
#>    <dbl> <chr>         <chr>              <chr>     <chr> <chr>  <chr>     <dbl>
#>  1  1865 Huesca        Sugarbeet pulp     NA        Crop… Outsi… livest… 4.52e-3
#>  2  1929 Tarragona     Olives (including… Irrigated Crop… Cropl… export  6.92e+1
#>  3  1955 Albacete      Wheat and products Rainfed   NA    Synth… Cropla… 6.16e+2
#>  4  1957 Gipuzkoa      Wheat and products NA        Crop… Outsi… popula… 8.17e-1
#>  5  1862 Huesca        Grapes and produc… Irrigated Crop… Cropl… popula… 9.39e-2
#>  6  1980 Lleida        Wheat and products Irrigated NA    Fixat… Cropla… 1.71e+2
#>  7  1863 A_Coruna      Millet and produc… Rainfed   NA    Fixat… Cropla… 9.95e-3
#>  8  1987 Lugo          Tomatoes and prod… Irrigated Crop… Cropl… livest… 1.29e-3
#>  9  1950 Castello      Apples and produc… Irrigated Crop… Cropl… livest… 1.96e-2
#> 10  1988 Zaragoza      Grassland          NA        semi… semi_… livest… 1.32e+3
```
