# Assign each crop to its soil-carbon crop group.

Map `item_prod_code` and an irrigation flag to the crop group the soil
carbon balance marches: `cropland_rainfed_herbaceous`,
`cropland_irrigated_herbaceous`, or one group per woody species such as
`cropland_rainfed_olive` (the species slug comes from `Name_biomass`,
the column Spain_Hist's own category is built from, so it is "Olive"
rather than the `names_cats` key "Olives"). Every label keeps the
`cropland_` prefix, which is what lets the balance recognise a group as
cropland for its soil-cover curve, its C:N lookup and its water term
without enumerating groups.

## Usage

``` r
soc_crop_group(
  item_prod_code,
  irrigated = FALSE,
  vocabulary = whep::items_prod_full
)
```

## Arguments

- item_prod_code:

  Integer vector of FAOSTAT production item codes.

- irrigated:

  Logical vector, recycled: is the area irrigated?

- vocabulary:

  The crop vocabulary, a tibble with `item_prod_code`, `Herb_Woody` and
  `Name_biomass`. Defaults to
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md).

## Value

A character vector of group labels, one per input element. An item with
no `Herb_Woody` classification aborts, naming it: an unclassified crop
silently pooled into the wrong group would move carbon between groups
with nothing recording it.

## Examples

``` r
soc_crop_group(c(15L, 260L, 260L), irrigated = c(FALSE, FALSE, TRUE))
#> [1] "cropland_rainfed_herbaceous" "cropland_rainfed_olive"     
#> [3] "cropland_irrigated_olive"   
```
