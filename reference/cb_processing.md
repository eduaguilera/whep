# Commodity balance sheet processing fractions

Specifies the product fractions obtained when CBS items are processed,
linking processed items to their output CBS categories.

## Usage

``` r
cb_processing
```

## Format

A tibble where each row corresponds to one processed-item /
output-category combination. It contains the following columns:

- `ProcessedItem`: Name of the CBS item being processed (e.g.,
  `"Apples and products"`, `"Barley and products"`).

- `item_cbs`: Name of the output CBS category produced by processing
  (e.g., `"Alcohol, Non-Food"`).

- `Product_fraction`: Conversion factor from processed input quantity to
  output product quantity. This can exceed 1 when the output includes
  added mass, such as water in beverages.

- `Value_fraction`: Economic value fraction associated with the output
  product (numeric; largely `NA` in current data).

- `Required`: Marks required co-product links in selected processing
  chains.

A fraction is a prior conversion rate per candidate output, not a
mass-conserving recipe: `.correct_processed()` rescales it per area-year
to reproduce the observed production of the output item, and several
inputs declare a single lossy output (olives to olive oil at 0.20).

The one dairy row, `"Milk - Excluding Butter"` to `"Butter, Ghee"`,
carries the milk churned into butter and ghee, which FAOSTAT's new FBS
reports as milk's `processing` destiny from 2010 (the old FBS does not
report it at all). Without it that mass was split onto food and feed,
inflating 2010 world milk food by 30.5% (#757).

## Source

Derived from FAOSTAT commodity balance sheet processing assumptions. The
milk-to-butter fraction of 0.045 is the median `"Butter of Cow Milk"`
extraction rate over the 69 countries reporting one in FAO (1997),
*Technical Conversion Factors for Agricultural Commodities*, Rome: FAO,
MILK section (range 3.3-7.3%). It is consistent with the ratio implied
by the FBS itself: global butter production over milk processing is
0.047 in 2010 and 0.044-0.047 across 2010-2019.

## Examples

``` r
head(cb_processing)
#> # A tibble: 6 × 5
#>   ProcessedItem                item_cbs Product_fraction Value_fraction Required
#>   <chr>                        <chr>               <dbl>          <dbl>    <dbl>
#> 1 Apples and products          Alcohol…            0.199           0.05       NA
#> 2 Barley and products          Alcohol…            0.204           0.05       NA
#> 3 Cassava and products         Alcohol…            0.09           NA          NA
#> 4 Cereals, Other               Alcohol…            0.275           0.9        NA
#> 5 Fruits, Other                Alcohol…            0.199          NA          NA
#> 6 Grapes and products (excl w… Alcohol…            0.199          NA          NA
```
