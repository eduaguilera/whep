# Produce row-level evidence for a table.

Record, as data, what each row of a result rests on: which producer
claimed it, at which version of that producer's source, and the
documented evidence fields the producer carries per row (a FAOSTAT flag,
a `method_*` label, a gap-filling status). This is the row-level
counterpart of
[`record_provenance()`](https://eduaguilera.github.io/whep/reference/record_provenance.md),
which records the code and input versions behind a whole dataset
(whep#372).

The record is a **separate table**, not an attribute. Attributes are
silently dropped by `dplyr` joins, filters and `summarise()`, so
attribute-borne evidence cannot survive the composition it is meant to
document. A keyed sidecar can:
[`evidence_for()`](https://eduaguilera.github.io/whep/reference/evidence_for.md)
re-aligns it onto a table after a join,
[`combine_row_evidence()`](https://eduaguilera.github.io/whep/reference/combine_row_evidence.md)
merges the output of several producers without letting one overwrite
another, and
[`evidence_conflicts()`](https://eduaguilera.github.io/whep/reference/evidence_conflicts.md)
reports where two producers disagree.

## Usage

``` r
row_evidence(
  data,
  source_id,
  key,
  fields,
  source_version = NA_character_,
  recorded_at = Sys.time()
)
```

## Arguments

- data:

  Table the evidence describes. Not modified. May have zero rows, which
  yields a zero-row evidence table.

- source_id:

  Immutable identifier of the producer, one non-empty string.

- key:

  Character vector of column names of `data` that jointly identify a
  row. Must be unique-valued and free of `NA`, otherwise a piece of
  evidence would address more than one row or none.

- fields:

  The row-level evidence to record, either a character vector of column
  names of `data`, or a named list whose elements are vectors of length
  1 or `nrow(data)`. A list column cannot be recorded.

- source_version:

  Version or vintage of the source, one string or `NA`.

- recorded_at:

  Timestamp of the record, a length-one `POSIXct`. Defaults to the
  current time; pass a fixed value for reproducible output.

## Value

A tibble of row evidence, as described above.

## Evidence table

Format `"whep-row-evidence/1"`. One row per (table row × evidence
field), every column `character` so the table round-trips through
Parquet, CSV and YAML unchanged:

- `row_key`: The row's identity, i.e. its key values joined by `U+001F`
  (unit separator). Opaque: build and read it with this family's
  functions rather than by hand.

- `key_columns`: The key column names, comma-separated, so a consumer
  can re-derive `row_key` from the data alone.

- `source_id`: The producer's immutable identifier. Callers should use
  the package's existing dataset labels (`"FAOSTAT_prod"`,
  `"FAOSTAT_FBS_New"`, `"LUH2"`), because nothing downstream can recover
  an identity that was renamed between builds.

- `source_version`: Version or vintage of that source, `NA` when the
  producer has none.

- `recorded_at`: When the record was made, as ISO 8601 UTC.

- `field`, `value`: The evidence field's name and its formatted value
  for that row. `value` may be `NA`; the field name may not.

Rows are ordered by `row_key`, `field`, `source_id`, `recorded_at` and
`value`, in the C locale, so two runs over the same input give
byte-identical output apart from `recorded_at`.

The schema is available as data from
[`row_evidence_schema()`](https://eduaguilera.github.io/whep/reference/row_evidence_schema.md)
and is asserted with
[`assert_table_schema()`](https://eduaguilera.github.io/whep/reference/assert_table_schema.md).
It declares no key: combining two runs of the same producer legitimately
repeats a claim, so duplication is not an error. Disagreement is, and
that is what
[`evidence_conflicts()`](https://eduaguilera.github.io/whep/reference/evidence_conflicts.md)
finds.

## See also

[`combine_row_evidence()`](https://eduaguilera.github.io/whep/reference/combine_row_evidence.md)
to merge producers,
[`evidence_for()`](https://eduaguilera.github.io/whep/reference/evidence_for.md)
to carry evidence through a join,
[`evidence_conflicts()`](https://eduaguilera.github.io/whep/reference/evidence_conflicts.md)
to find disagreement, and
[`record_provenance()`](https://eduaguilera.github.io/whep/reference/record_provenance.md)
for the dataset-level record.

## Examples

``` r
cbs <- tibble::tibble(
  area_code = c(724L, 724L, 76L),
  item_cbs_code = c(2511L, 2513L, 2511L),
  year = 2020L,
  value = c(1.5, 2.5, 3.5),
  fao_flag = c("A", "E", "A")
)

# Evidence the table already carries per row, plus one derived field.
evidence <- row_evidence(
  cbs,
  source_id = "FAOSTAT_FBS_New",
  key = c("area_code", "item_cbs_code", "year"),
  fields = list(
    fao_flag = cbs$fao_flag,
    imputed = cbs$fao_flag == "E"
  ),
  source_version = "2024-03-14",
  recorded_at = as.POSIXct("2026-01-01", tz = "UTC")
)
evidence
#> # A tibble: 6 × 7
#>   row_key           key_columns source_id source_version recorded_at field value
#>   <chr>             <chr>       <chr>     <chr>          <chr>       <chr> <chr>
#> 1 "724\u001f2511\u… area_code,… FAOSTAT_… 2024-03-14     2026-01-01… fao_… A    
#> 2 "724\u001f2511\u… area_code,… FAOSTAT_… 2024-03-14     2026-01-01… impu… FALSE
#> 3 "724\u001f2513\u… area_code,… FAOSTAT_… 2024-03-14     2026-01-01… fao_… E    
#> 4 "724\u001f2513\u… area_code,… FAOSTAT_… 2024-03-14     2026-01-01… impu… TRUE 
#> 5 "76\u001f2511\u0… area_code,… FAOSTAT_… 2024-03-14     2026-01-01… fao_… A    
#> 6 "76\u001f2511\u0… area_code,… FAOSTAT_… 2024-03-14     2026-01-01… impu… FALSE

# It is a documented schema, so it can be proved rather than trusted.
assert_table_schema(evidence, row_evidence_schema())

# And it round-trips: every column is character.
path <- tempfile(fileext = ".parquet")
write_table_checked(evidence, path)
identical(tibble::as_tibble(nanoparquet::read_parquet(path)), evidence)
#> [1] TRUE
unlink(path)
```
