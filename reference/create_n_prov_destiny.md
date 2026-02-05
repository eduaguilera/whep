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
create_n_prov_destiny()
```

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

- `MgN`: Nitrogen amount in megagrams (Mg).
