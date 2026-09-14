# Report the expected keys that no row supplies

Enumerate the keys an expected lattice requires and the data does not
supply, returning one row per missing key. A zero-row result means every
expected key is present in every group.

This is the detector
[`check_keys_complete()`](https://eduaguilera.github.io/whep/reference/check_keys_complete.md)
signals on, exposed separately because a caller that wants to *act* on
the gaps – report them, drop the groups that carry them – needs the keys
themselves rather than a condition.

## Usage

``` r
key_lattice_gaps(data, expected, .by = NULL, max_gaps = 1e+06)
```

## Arguments

- data:

  A tibble to check.

- expected:

  The expected key set: a named list of value vectors, a data frame of
  expected key rows, or a function of `data` returning either.

- .by:

  Character vector of grouping columns. Every group must carry the whole
  expected key set. `NULL` (default) treats the table as one group.

- max_gaps:

  Refuse rather than enumerate when more keys than this are missing. A
  result that large is not a partially incomplete lattice, it is an
  almost entirely absent one, and enumerating it helps nobody.

## Value

A tibble with the `.by` columns and the expected key columns, one row
per missing key, sorted by group. Zero rows when the lattice is
complete.

## Why a total, an identity or a row count cannot do this

An absent month drops its row, so a year sums **eleven** months, and the
eleven-month total is a plausible number that no `na.rm` choice can
question – there is no `NA` anywhere. A conservation check over the rows
that arrived still balances, because both sides are built from the same
eleven. A row count still looks reasonable, because it is reasonable: it
is the count of the rows that came. Only an assertion against the key
set that *should* have arrived distinguishes the two.

## What the expected lattice may be

`expected` is one of three forms, and the form decides the cost.

- A **named list** of value vectors, e.g. `list(month = 1:12)`. The
  expected key set is the cross product of the elements, required in
  every group.

- A **data frame** of expected key rows, e.g. every `(year, area_code)`
  pair a polity vocabulary admits. Use this for a lattice that varies
  along one of its own keys: put the varying key in the frame and do not
  group on it, and the whole per-year vocabulary is one key set.

- A **function** of `data` returning either of the above, for a rule
  that has to read the data first – the years it actually spans, the
  model's cell list.

The contract is containment, not equality: every expected key must
occur, and a key the data carries beyond the expected set is not a
failure.

## Cost, which is the reason the design is shaped this way

A full cross-join assertion is free on twelve months and ruinous on tens
of millions of cell-months, so completeness is established in stages and
each stage runs only when the cheaper one could not settle it.

1.  **Scalar counts.** When the observed vocabulary of each key column
    lies inside the expected one, the lattice is complete if and only if
    the number of distinct key tuples equals the number of groups times
    the size of the expected key set. That is two
    [`dplyr::n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
    passes over column vectors: no frame is copied, no lattice is built
    and no grouped result is materialised.

2.  **A distinct-key anti-join**, reached only when the counts disagree
    (or when the observed vocabulary is not inside the expected one,
    which makes the count identity unsound). This works from the
    *distinct* key tuples, not from every row.

3.  **Enumeration of the incomplete groups only.** The lattice is
    expanded for the groups that already failed their count, never for
    the whole table. The enumeration is the diagnostic, not the
    detector.

One rule follows from stage 1 and is worth knowing before wiring a check
on a large table: **narrow the data to the keys you are asserting
about**. An expected set deliberately smaller than a column's observed
vocabulary – "months 1 and 12 must be here", on a frame that also holds
2 to 11 – defeats the count identity and sends every call down stage 2,
even when nothing is missing. Filtering to the two months first keeps
the assertion identical and the cost scalar; `.wb_check_swc_boundary()`
does exactly this.

Measured on the 86,781,420-row `lpjml-soc-hydrology` cell-month lattice
(7,231,785 cell-year groups, `lon`/`lat`/`year`/`month`): stage 1
settles it in **13.9 s with no measurable allocation above the frame
itself**. For comparison, a grouped distinct count of the same lattice
takes 66.7 s and +166 MB, and materialising the full lattice and
anti-joining it takes 36.6 s and +2.1 GB. Memory is the binding
constraint on this chain (whep#624), which is why the cheapest stage is
also the one that allocates nothing; stage 2 costs a further 6.4 s and
+2.0 GB there, and is paid only by a lattice that is already broken.

## What it cannot see

Three absences pass this check, and a caller must not read a pass as
more than it is.

- A **group that is absent entirely**. No rows at all means no group,
  and `.by` can only name groups the data already carries. Assert the
  expected *group* set the same way: put the grouping keys in the
  data-frame form of `expected` and pass no `.by`.

- A key that is present but **wrong**. This is a completeness check, not
  a validity one; month 13 is not a failure here, only an absent month
  12 is.

- A lattice that is complete while its **values** are absent as zeros.
  That is the other half of the family: assert the input was supplied,
  not that its keys are all there (whep#1034).

## Examples

``` r
eleven <- tibble::tibble(year = 2000L, month = 1:11)
key_lattice_gaps(eleven, list(month = 1:12), .by = "year")
#> # A tibble: 1 × 2
#>    year month
#>   <int> <int>
#> 1  2000    12

# Complete: zero rows.
key_lattice_gaps(
  tibble::tibble(year = 2000L, month = 1:12),
  list(month = 1:12),
  .by = "year"
)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: year <int>, month <int>
```
