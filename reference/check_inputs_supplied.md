# Refuse an input that was never supplied

Assert that each named input **arrived and is not vacuous**, rather than
that the totals it feeds reconcile. An input is *supplied* when its
column exists and holds at least one value that is neither missing nor
zero; it is *absent* when the column is missing, or present and
identically zero or missing in every row.

## Usage

``` r
check_inputs_supplied(
  data,
  required,
  stamp = NULL,
  action = c("abort", "warn"),
  details = NULL
)
```

## Arguments

- data:

  A tibble carrying the inputs to check.

- required:

  Column names to check, optionally named. A name is the key used in
  messages and matched against `stamp`; an unnamed entry uses the column
  name as its own key.

- stamp:

  Optional name of a provenance column written by
  [`stamp_inputs_supplied()`](https://eduaguilera.github.io/whep/reference/stamp_inputs_supplied.md).
  Used when `data` carries it, ignored otherwise.

- action:

  `"abort"` (default) to raise, `"warn"` to carry on. Abort where a
  wrong answer is worse than no answer – a density whose denominator is
  inflated, a stressor that would ship as zero. Warn where a caller
  reading only the columns that did arrive is unaffected.

- details:

  Extra `cli` bullets appended to the message, for the caller to name
  its source and its remedy.

## Value

`data`, invisibly. Raises or warns with class `whep_absent_input` when
an input is absent; the condition carries the absent keys in its
`absent` field.

## Why reconciliation cannot do this

Zero satisfies a sum. A pin published without its inland water and ice
layers booked 1,643 Mha of lakes and 618 Mha of glaciers as land, and
`polity_area_ha == land_area_ha + inland_water_ha + ice_area_ha` still
held to `max |residual| = 0 ha`, because both absent layers were zero
(whep#885, whep#1010). Three FAOSTAT emission labels lost to a
pin-vocabulary change summed to a literal zero through
`sum(na.rm = TRUE)`, and a test named *"enteric_ch4_kt conservation is
exact"* went on passing, because zero distributes to zero (whep#1016). A
consistency identity, a conservation check and a row count are all
satisfied by an absence. Only an assertion that the input was supplied
is not.

## Supplied is a label first, a value test second

When `stamp` names a column the producer wrote, that column decides: it
is a comma-separated list of the keys that were actually consumed,
written by
[`stamp_inputs_supplied()`](https://eduaguilera.github.io/whep/reference/stamp_inputs_supplied.md),
and no arithmetic downstream can forge it. The value test is the
fallback for a table published before its producer carried a stamp. The
fallback is deliberately **scale-free** – identically zero, not a
magnitude floor – so that it holds on a single-country development build
as well as on a global one. A floor belongs at publication time, where
the expected scale is known.

## What it cannot see

Three absences pass this check, and a caller must not read a pass as
more than it is.

- A **partial** absence. One non-zero value anywhere in the column is
  enough; a layer supplied for three countries out of two hundred is
  supplied as far as this is concerned.

- A **zero-row** input. Zero rows is a filter that matched nothing, not
  a zero-filled column, and the caller that wrote the filter is the one
  to answer for it. Columns are judged only when `data` has rows.

- A value the **code failed to reach**. If the lookup was never called,
  the quantity is absent from the code path and not from the data, and
  that is a defect rather than a fill (whep#1034). Run the lookup and
  look at what it returns.

## Examples

``` r
supplied <- tibble::tibble(land_ha = c(1, 2), water_ha = c(0.5, 0))
check_inputs_supplied(supplied, c("land_ha", "water_ha"))

absent <- tibble::tibble(land_ha = c(1, 2), water_ha = c(0, 0))
try(check_inputs_supplied(absent, c(land = "land_ha", water = "water_ha")))
#> Error in eval(expr, envir) : Input water was not supplied.
#> ✖ An absent input and a measured zero are the same number downstream, so every
#>   total that reconciles will go on reconciling while this is missing.
```
