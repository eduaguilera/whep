# Attach a provenance record to a result.

Store a provenance record (from
[`record_provenance()`](https://eduaguilera.github.io/whep/reference/record_provenance.md))
on an object as an attribute, so the result travels together with its
lineage. Retrieve it later with
[`get_provenance()`](https://eduaguilera.github.io/whep/reference/get_provenance.md).

## Usage

``` r
attach_provenance(x, provenance)
```

## Arguments

- x:

  Any R object, typically a result tibble.

- provenance:

  Provenance tibble from
  [`record_provenance()`](https://eduaguilera.github.io/whep/reference/record_provenance.md).

## Value

`x`, unchanged except for an added `whep_provenance` attribute.

## Examples

``` r
prov <- record_provenance(
  aliases = "bilateral_trade",
  recorded_at = as.POSIXct("2026-01-01", tz = "UTC")
)
result <- attach_provenance(tibble::tibble(value = 1), prov)
get_provenance(result)
#> # A tibble: 1 × 5
#>   recorded_at         whep_version r_version input_alias     input_version      
#>   <dttm>              <chr>        <chr>     <chr>           <chr>              
#> 1 2026-01-01 00:00:00 0.3.0.9000   4.6.1     bilateral_trade 20250714T123347Z-2…
```
