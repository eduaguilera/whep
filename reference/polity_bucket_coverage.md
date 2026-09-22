# Report reporting buckets that sum more than one territory

`polity_area_code` is an aggregation bucket, not an identity: FABIO
folds several FAOSTAT reporting areas into one numeric code, and WHEP's
builds sum them under it. `reporting_polity_code` is then resolved from
that bucket code, so a bucket folding more than one live territory can
end up labelled with a polity that covers only part of what the value
covers, or that covers the whole of it but in a period that has ended.

This lists every `(polity_area_code, year)` that folds more than one
polity and classifies whether the bucket's own label covers the fold:

- `"aggregate"`: the bucket resolves to an aggregate polity (Rest of
  World, Belgium-Luxembourg, the FAOSTAT combined-reporting entities),
  whose name and polygon already mean the union of its members. Honest.

- `"predecessor"`: the bucket is labelled with a polity that has
  **ended**, and whose published `successor` set is exactly the set of
  polities the bucket folds. The extent is right — that predecessor's
  territory is the union of its successors — but the period is not, so a
  consumer filtering polities by span drops the rows.

- `"partial"`: the bucket sums several territories but is labelled with
  a polity covering only part of them, so the value and its polity
  describe different extents. This is the worst case, and no bucket is
  in it today.

- `"unlabelled"`: the bucket code resolves to no polity, so rows carry
  `NA` and the gap is at least visible rather than wrong.

An area counts as a member only in the years it **reports**: its polity
must be in span, and the upstream FAOSTAT map must report the area that
year. A year-aware lookup answers every `(area_code, year)` pair
regardless, standing in with the nearest period, so asking it about an
area that does not report in that year invents a member. FAOSTAT reports
area 206 for 1961-2011 and areas 276/277 for 2012-2024, never in the
same year, so counting the stand-ins reported bucket 206 as a three-way
fold in all 65 years rather than a two-way fold in the 14 it is one
(whep#414).

Bucket 206 is the one fold reported today, and it is `"aggregate"` for
2012-2025: it sums FAOSTAT areas 276 Sudan and 277 South Sudan and
resolves to `F206-2011-2025` "Sudan and South Sudan (combined
reporting)", the aggregate whep#860 wired in. It read `"predecessor"`
before that, labelled with `SUD-1956-2011`, a polity that had ended.

So the labelling is settled and only the regionalisation is still open:
whether to keep folding the two successors at all is whep#680, and
`options(whep.unfold_predecessor_bucket = "all")` is the switch that
promotes them. It is off by default because it is not mass-neutral — see
[`folded_reporting_areas()`](https://eduaguilera.github.io/whep/reference/folded_reporting_areas.md)
for what it withdraws and why. `.aggregate_to_polities()` warns when it
builds such a bucket; set `options(whep.warn_polity_folds = FALSE)` to
silence that warning.

The polity reported here as the bucket's own is also the `area` label
the builds attach to the summed row, and the one the reporting columns
resolve. A bucket carries one label whatever its members resolve to,
because `area` is a join key and a bucket under two labels stops summing
(whep#563).

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
#> # A tibble: 1 × 10
#>   polity_area_code  year n_member_polities member_polity_codes member_area_codes
#>              <int> <int>             <int> <chr>               <chr>            
#> 1              206  2015                 2 SDN-2011-2025, SSD… 276, 277         
#> # ℹ 5 more variables: bucket_polity_code <chr>, bucket_polity_name <chr>,
#> #   bucket_mapping_status <chr>, bucket_polity_type <chr>, coverage <chr>
```
