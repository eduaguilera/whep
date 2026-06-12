# Get WHEP polity geometries

Returns the periodized polity database, including geometry. Pass
`polity_codes` to retrieve a subset that can be joined to outputs from
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md).

## Usage

``` r
get_polity_geometries(polity_codes = NULL)
```

## Arguments

- polity_codes:

  Optional character vector of WHEP polity codes.

## Value

An sf data frame.
