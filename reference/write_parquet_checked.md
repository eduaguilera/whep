# Write a Parquet file and verify it before returning.

Wrapper around
[`nanoparquet::write_parquet()`](https://nanoparquet.r-lib.org/reference/write_parquet.html)
that reopens the file it just wrote and runs
[`assert_parquet_integrity()`](https://eduaguilera.github.io/whep/reference/assert_parquet_integrity.md)
on it. Use it for pipeline artifacts large enough that a silent write
failure would go unnoticed; the verification is metadata-only, so it
costs milliseconds regardless of file size.

## Usage

``` r
write_parquet_checked(data, path, deep = FALSE, ...)
```

## Arguments

- data:

  Data frame to write.

- path:

  Destination path.

- deep:

  Whether to verify by decoding every row group as well as by checking
  the layout. Reads the whole file back.

- ...:

  Passed to
  [`nanoparquet::write_parquet()`](https://nanoparquet.r-lib.org/reference/write_parquet.html).

## Value

Invisibly, `path`.

## Examples

``` r
path <- tempfile(fileext = ".parquet")
write_parquet_checked(data.frame(x = 1:10), path)
unlink(path)
```
