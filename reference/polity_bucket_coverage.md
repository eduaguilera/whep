# Report reporting buckets whose polity covers less than the bucket

`polity_area_code` is an aggregation bucket, not an identity: FABIO
folds several FAOSTAT reporting areas into one numeric code, and WHEP's
builds sum them under it. `reporting_polity_code` is then resolved from
that bucket code, so a bucket folding more than one live territory can
end up labelled with a polity covering only part of what the value
covers.

This lists every `(polity_area_code, year)` that folds more than one
polity and classifies whether the bucket's own label covers the fold:

- `"aggregate"`: the bucket resolves to an aggregate polity (Rest of
  World, Belgium-Luxembourg, the FAOSTAT combined-reporting entities),
  whose name and polygon already mean the union of its members. Honest.

- `"partial"`: the bucket sums several territories but is labelled with
  a single-territory polity, so the value and its polity describe
  different extents. This is the defect.

- `"unlabelled"`: the bucket code resolves to no polity, so rows carry
  `NA` and the gap is at least visible rather than wrong.

The known `"partial"` case is bucket 206, which folds FAOSTAT areas 276
Sudan and 277 South Sudan after the 2011 secession while no live polity
means "Sudan and South Sudan" (whep#414). `.aggregate_to_polities()`
warns when it builds such a bucket; set
`options(whep.warn_polity_folds = FALSE)` to silence that warning.

## Usage

``` r
polity_bucket_coverage(years = NULL)
```

## Arguments

- years:

  Integer vector of years to classify. Defaults to the FAOSTAT reporting
  era, 1961 to 2025. Years before the back-cast anchor resolve to the
  anchor-year territory, so they classify identically to 1961.

## Value

A tibble with one row per folded `(polity_area_code, year)`, with the
folded member polities, the polity the bucket itself resolves to, and
the `coverage` classification. Zero rows means no bucket folds more than
one polity in the requested years.

## Examples

``` r
polity_bucket_coverage(years = 2015L)
#> # A tibble: 2 × 10
#>   polity_area_code  year n_member_polities member_polity_codes member_area_codes
#>              <int> <int>             <int> <chr>               <chr>            
#> 1              206  2015                 3 SDN-2011-2025, SSD… 206, 276, 277    
#> 2              999  2015                 8 GNQ-1968-2025, GUF… 5, 6, 17, 22, 24…
#> # ℹ 5 more variables: bucket_polity_code <chr>, bucket_polity_name <chr>,
#> #   bucket_mapping_status <chr>, bucket_polity_type <chr>, coverage <chr>
```
