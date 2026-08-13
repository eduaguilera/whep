# Read the polycell support table from its registered pin

Resolves the versioned `polycell_support` input (DA-17), preferring a
local parquet named by `Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH")` so a
development build can be used before it is published.

## Usage

``` r
read_polycell_support(path = NULL, version = NULL)
```

## Arguments

- path:

  Optional path to a local parquet, overriding the environment variable
  and the pin.

- version:

  Pin version, passed to
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md).
  `NULL` takes the version frozen in
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md).

## Value

A `tibble` in the
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
grain.

## Examples

``` r
# Requires WHEP_POLYCELL_SUPPORT_PATH or a published pin; not run without it.
if (nzchar(Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH"))) {
  read_polycell_support()
}
```
