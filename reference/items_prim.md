# Primary production items linked to CBS

Maps FAOSTAT primary production items and crop products to their CBS
counterparts, along with farm and labour classifications.

## Usage

``` r
items_prim
```

## Format

A tibble where each row corresponds to one production item. It contains
the following columns:

- `item_prod`: Name of the production item (e.g., `"Wheat"`, `"Rice"`).

- `item_prod_code`: FAOSTAT production item code (character).

- `item_cbs`: Name of the corresponding CBS item.

- `item_cbs_code`: Numeric CBS item code.

- `Farm_class`: Farm system classification. Crop items use codes such as
  `"COP"` (cereals, oilseeds, protein crops), `"Vegetables"`,
  `"Fruits"`, `"Olive"`, `"Grapevine"`, `"Other_crops"`. Livestock items
  use `"Dairy_cows"`, `"Cattle"`, `"Monogastric"`, `"Sheep_goats"`,
  `"Bees"`, `"Game"`. `NA` for non-farm items.

- `Cat_Labour`: Labour category used in agricultural labour analyses.

- `Cat_FAO1`: Top-level FAO commodity category.

- `group`: Item group classification. One of `"Primary crops"`,
  `"Crop products"`, `"Livestock products"`, `"Grass"`,
  `"Crop residues"`, `"Scavenging"`, `"Livestock"`.

## Source

Derived from [FAOSTAT Production
data](https://www.fao.org/faostat/en/#data/QCL).
