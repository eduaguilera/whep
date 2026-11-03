# GRAFS Nitrogen (N) flows

Provides N flows of the Spanish agro-food system on a provincial level
between 1860 and 2020. This dataset is the base of the GRAFS model and
contains data in megagrams of N (MgN) for each year, province, item,
origin and destiny. Thereby, the origin column represents where N comes
from, which includes N soil inputs, imports and production. The destiny
column shows where N goes to, which includes export, population food,
population other uses and feed or cropland (in case of N soil inputs).
Processed items, residues, woody crops, grazed weeds are taken into
account.

## Usage

``` r
create_n_prov_destiny(example = FALSE, wood_n_method = c("tissue", "single"))
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

- wood_n_method:

  How the nitrogen concentration of the two wood items is chosen. Both
  harvested `Wood` and the forest and shrubland residue booked as
  `Firewood` map to the single `Average wood` row of
  [biomass_coefs](https://eduaguilera.github.io/whep/reference/biomass_coefs.md),
  although one is stemwood and the other branches and bark. One of:

  - `"tissue"` (default): price each by its own tissue, using the pooled
    medians of Thurner et al. (2025), 0.0010 kg N per kg dry matter for
    stem sapwood (`Wood`) and 0.0035 for branches (`Firewood`).

  - `"single"`: use the shared `Average wood` cell for both, 0.0030,
    which is a branch concentration and carries `Wood` about 3 times too
    high.

  Thurner, M., Yu, K., Manzoni, S., Prokushkin, A., Thurner, M. A.,
  Wang, Z., and Hickler, T. (2025). Nitrogen concentrations in boreal
  and temperate tree tissues vary with tree age/size, growth rate, and
  climate. *Biogeosciences* 22(5), 1475-1493.
  [doi:10.5194/bg-22-1475-2025](https://doi.org/10.5194/bg-22-1475-2025)
  .

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
  agroecosystems, Livestock (manure), People (waste water), Deposition,
  Fixation, Synthetic, or Outside. Fish and Agro-industry are `box`
  values, not origins; their nitrogen enters as Outside.

- `destiny`: The destiny category of N: population_food (edible-basis;
  see `.split_food_inedible_loss()`), population_food_inedible (the
  inedible fraction of the same food, split out of population_food so
  nothing is silently discarded from the total), population_other_uses,
  livestock_mono, livestock_rum (feed), aquaculture (feed too, but into
  the Fish box rather than Livestock – aquaculture has no land-based
  production of its own, see issue \#379), export, processing_losses (N
  not credited to a processed output), Cropland and
  semi_natural_agroecosystems (for N soil inputs).

- `mg_n`: Nitrogen amount in megagrams (Mg).

## Examples

``` r
create_n_prov_destiny(example = TRUE)
#> # A tibble: 11 × 8
#>     year province_name item               irrig_cat box   origin destiny    mg_n
#>    <dbl> <chr>         <chr>              <chr>     <chr> <chr>  <chr>     <dbl>
#>  1  1865 Huesca        Sugarbeet pulp     NA        Crop… Outsi… livest… 4.52e-3
#>  2  1929 Tarragona     Olives (including… Irrigated Crop… Cropl… export  6.92e+1
#>  3  1955 Albacete      Wheat and products Rainfed   NA    Synth… Cropla… 6.16e+2
#>  4  1957 Gipuzkoa      Wheat and products NA        Crop… Outsi… popula… 8.17e-1
#>  5  1862 Huesca        Grapes and produc… Irrigated Crop… Cropl… popula… 8.45e-2
#>  6  1862 Huesca        Grapes and produc… Irrigated Crop… Cropl… popula… 9.39e-3
#>  7  1980 Lleida        Wheat and products Irrigated NA    Fixat… Cropla… 1.71e+2
#>  8  1863 A_Coruna      Millet and produc… Rainfed   NA    Fixat… Cropla… 9.95e-3
#>  9  1987 Lugo          Tomatoes and prod… Irrigated Crop… Cropl… livest… 1.29e-3
#> 10  1950 Castello      Apples and produc… Irrigated Crop… Cropl… livest… 1.96e-2
#> 11  1988 Zaragoza      Grassland          NA        semi… semi_… livest… 1.32e+3
```
