# Assert that a Parquet file is structurally sound.

Build-time gate over a Parquet artifact. Aborts when
[`check_parquet_integrity()`](https://eduaguilera.github.io/whep/reference/check_parquet_integrity.md)
reports any anomaly, so a corrupt multi-gigabyte cube fails loudly at
the moment it is written instead of being read back truncated for months
(whep#531).

## Usage

``` r
assert_parquet_integrity(path, deep = FALSE)
```

## Arguments

- path:

  Path to a Parquet file.

- deep:

  Whether to also read every row group and report the ones that fail to
  decode. Exact, but reads the entire file.

## Value

Invisibly, `path`. Called for its side effect of aborting on violation.

## Examples

``` r
path <- tempfile(fileext = ".parquet")
nanoparquet::write_parquet(data.frame(x = 1:10), path)
assert_parquet_integrity(path)
unlink(path)
```
