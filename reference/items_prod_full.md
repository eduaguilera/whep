# Full production item table

Comprehensive reference table for all production items, combining CBS
linkages, biomass names, multiple classification schemes, and crop
ecological traits.

## Usage

``` r
items_prod_full
```

## Format

A tibble where each row corresponds to one production item. It contains
the following columns:

- `item_prod`: Name of the production item.

- `item_prod_code`: FAOSTAT production item code (character).

- `item_cbs`: Name of the corresponding CBS item.

- `item_cbs_code`: Numeric CBS item code.

- `group`: Item group. One of `"Primary crops"`, `"Crop products"`,
  `"Livestock products"`, `"Crop residues"`, `"Grassland"`,
  `"Scavenging"`.

- `live_anim`: Name of the parent live animal for livestock-derived
  items (`NA` for crop items).

- `live_anim_code`: Numeric CBS code of the parent live animal (`NA` for
  crop items).

- `Cat_Krausmann`: Item category used in Krausmann et al. biomass flow
  accounting.

- `Name_biomass`: Corresponding item name in `biomass_coefs`, enabling
  joins for biomass coefficients.

- `Name_Eurostat`: Corresponding item name in Eurostat agricultural
  statistics.

- `Name`: Alternative or display name for the item.

- `Cat_Labour`: Labour category used in agricultural labour analyses.

- `Cat_FAO1`: Top-level FAO commodity category (e.g., `"Cereals"`,
  `"Oilcrops"`).

- `Cat_Origin`: Origin-based commodity category. One of `"Cereals"`,
  `"Vegetables and Fruits"`, `"Sugar and Stimulants"`, `"Oil Crops"`,
  `"Fodder crops"`, `"Fibres and Crude Materials"`, or `NA`.

- `Cat_Use`: Use-based commodity category (e.g., `"Grains"`,
  `"Oils and Fats"`, `"Fodder crops"`,
  `"Beverages, sugar and stimulants"`).

- `Order`: Numeric ordering field used for sorting items consistently
  across outputs.

- `Categ`: General category label used in some analyses.

- `Farm_class`: Farm system classification (see `items_prim` for
  values).

- `c3_c4`: Photosynthetic pathway. One of `"c3"`, `"c4"`, or `NA`.

- `ann_per_nfx`: Annual/perennial and nitrogen fixation trait. One of
  `"ann"` (annual), `"per"` (perennial), `"nfx"` (nitrogen-fixing), or
  `NA`.

- `Cat_1`: Primary category label for material flow accounting.

- `Cat_2`: Secondary category label.

- `Cat_3`: Tertiary category label.

- `Cat_4`: Quaternary category label.

- `Herb_Woody`: Plant growth form. One of `"Herbaceous"`, `"Woody"`, or
  `NA`.

- `Crop_irrig`: Irrigation category used in water-use analyses.

- `Cat_Org`: Organic farming category classification.

- `Cat_Ymax`: Maximum attainable yield category.

- `Cat_Ymax_leg`: Legend label for the `Cat_Ymax` category.

## Source

Derived from [FAOSTAT Production
data](https://www.fao.org/faostat/en/#data/QCL) and multiple
classification schemes from the literature.
