# Check the structural integrity of a Parquet file.

Detect Parquet files whose footer describes a layout the file itself
does not have. The motivating failure is `nanoparquet` before 0.5.0,
which stored column-chunk file offsets and sizes as 32-bit integers:
past 4 GiB (2^32 bytes) they wrapped around, so a multi-gigabyte cube
received a footer that still declared every row group and row but
pointed at the wrong bytes for most of them. A reader then returns the
first ~4 GiB and throws thrift `"Deserializing page header failed"` on
everything after it, which a consumer that does not read row group by
row group never sees: it silently gets truncated data (whep#531).

The default check is metadata-only and therefore costs milliseconds even
on a 15 GB file. Column chunks are written contiguously in
`(row_group, column)` order by every mainstream writer, so their byte
ranges must not overlap and must stay inside the file. An offset that
jumps backwards is the signature of the 32-bit wraparound. The upper
bound is the start of the footer rather than the end of the file, so a
chunk that runs into the metadata is caught too.

`deep = TRUE` additionally decodes every row group with `arrow`, which
is exact rather than structural but reads the whole file.

## Usage

``` r
check_parquet_integrity(path, deep = FALSE)
```

## Arguments

- path:

  Path to a Parquet file.

- deep:

  Whether to also read every row group and report the ones that fail to
  decode. Exact, but reads the entire file.

## Value

A tibble with one row per anomaly, empty when the file is sound:

- `row_group`: Zero-based row group index.

- `column`: Zero-based column index, `NA` for whole-row-group problems
  found by `deep = TRUE`.

- `issue`: One of `"offset_overlap"` (a chunk starts before the previous
  chunk ends, i.e. offsets are not monotonic), `"offset_past_data"` (a
  chunk ends beyond the last byte the data section can hold) or
  `"row_group_unreadable"` (`deep = TRUE` only).

- `chunk_start`, `chunk_end`: Byte range the footer claims for the
  chunk.

- `detail`: Human-readable context, e.g. the reader's error.

## Examples

``` r
path <- tempfile(fileext = ".parquet")
nanoparquet::write_parquet(data.frame(x = 1:10), path)
check_parquet_integrity(path)
#> # A tibble: 0 × 6
#> # ℹ 6 variables: row_group <int>, column <int>, issue <chr>, chunk_start <dbl>,
#> #   chunk_end <dbl>, detail <chr>
unlink(path)
```
