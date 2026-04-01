# Animal codes and classifications

Maps live animal CBS items to their livestock classifications, process
codes, and associated product items used in livestock modeling.

## Usage

``` r
animals_codes
```

## Format

A tibble where each row corresponds to one live animal CBS item. It
contains the following columns:

- `Item_Code`: Numeric FAOSTAT item code for the live animal.

- `item_cbs`: Name of the CBS item (e.g., `"Cattle"`, `"Asses"`).

- `proc_code`: Short process code used internally (e.g., `"p092"`).

- `proc`: Descriptive process name (e.g., `"Asses"`).

- `item_cbs_code`: Numeric CBS item code (often equal to `Item_Code`).

- `Farm_class`: Broad farm classification grouping the animal. One of
  `"Cattle"`, `"Dairy_cows"`, `"Monogastric"`, `"Sheep_goats"`,
  `"Bees"`, `"Game"`.

- `Item_product`: Name of the primary product derived from this animal,
  if applicable (e.g., milk for dairy cows).

- `Item_Code_product`: Numeric FAOSTAT code for the associated product
  item.

- `Liv_prod_cat`: Livestock product category the animal belongs to.

- `Graniv_grazers`: Broad feeding behaviour classification. One of
  `"Grazers"`, `"Granivores"`, `"Bees"`, `"Game"`.

- `Livestock_name`: Internal livestock identifier used across datasets
  (e.g., `"Cattle"`, `"Dairy_cows"`, `"Asses"`).

- `Animal_class`: Fine-grained animal class, including production type
  distinctions (e.g., `"Broilers"`, `"Hens"`, `"Hogs"`, `"Dairy_cows"`).

- `Item_FAOmanure`: Name of the corresponding FAOSTAT manure management
  item.

- `Item_Code_FAOmanure`: Numeric code of the FAOSTAT manure management
  item.

- `Cat_Labour`: Labour category used in labour-related analyses. One of
  `"Cattle"`, `"Equines"`, `"Dairy_cows"`, `"Birds"`,
  `"Small_ruminants"`, `"Pigs"`, `"Bees"`.

- `Cat_FAO1`: Top-level FAO category. Currently always `"Animal"`.

- `item_bouwman`: Item name used in Bouwman et al. livestock datasets.

## Source

Derived from [FAOSTAT data](https://www.fao.org/faostat/en/#data/QA) and
internal livestock classification work.
