# Refuse an aggregate built over an incomplete key lattice

Assert that every key an expected lattice requires is supplied by some
row, and abort naming the keys that are **missing** rather than the rows
that are present. This is the absent-row sibling of the absent-column
and absent-label guards: an aggregate over eleven of twelve months is a
number with nothing wrong with it except that it is wrong, and no `NA`
policy, identity or row count can tell it from a twelve-month one.

The gaps themselves come from
[`key_lattice_gaps()`](https://eduaguilera.github.io/whep/reference/key_lattice_gaps.md);
see its documentation for the forms `expected` may take and for the
staged cost that makes the check affordable on a lattice of tens of
millions of keys.

## Usage

``` r
check_keys_complete(
  data,
  expected,
  .by = NULL,
  action = c("abort", "warn"),
  details = NULL
)
```

## Arguments

- data:

  A tibble to check.

- expected:

  The expected key set; see
  [`key_lattice_gaps()`](https://eduaguilera.github.io/whep/reference/key_lattice_gaps.md).

- .by:

  Character vector of grouping columns, or `NULL` for one group.

- action:

  `"abort"` (default) to raise, `"warn"` to carry on.

- details:

  Extra `cli` bullets appended to the message, for the caller to name
  its source and its remedy. Interpolated in the caller's own
  environment, so a bullet may name the caller's variables.

## Value

`data`, invisibly. Raises or warns with class `whep_incomplete_lattice`
(and `whep_absent_input`) when a key is missing; the condition carries
the gap tibble in its `missing` field.

## Abort versus warn is the caller's decision, not this function's

Both are wrong as a blanket policy, so neither is imposed.

- **Abort** is right where the aggregate is the deliverable and a
  short-summed one would ship: an eleven-month annual water balance
  drives nitrogen leaching downstream, and no consumer of the annual
  total can recover the twelfth month. It is wrong where a legitimately
  partial period exists – a model run still writing its current year –
  because it blocks a build over the complete years for the sake of an
  incomplete one the caller never asked for.

- **Warn** keeps that build alive, and is what a caller who has decided
  the gap is acceptable should select explicitly. It must not be the
  default: two warnings were already being printed when whep#1010
  shipped 533 Mha of lakes and glaciers as land, so a warning inside a
  build that runs for hours gates nothing.

The default is therefore `"abort"`, the most rigorous option, and a
caller who wants the build to continue says so and is recorded as having
said so. A third policy – drop the incomplete groups, so the aggregate
is absent rather than wrong – is deliberately not an `action` here,
because this function returns `data` unchanged and a silent drop is the
very thing being fixed. Do it with
[`key_lattice_gaps()`](https://eduaguilera.github.io/whep/reference/key_lattice_gaps.md)
and an anti-join at the site, where the reader can see which groups went
and why.

## Examples

``` r
complete <- tibble::tibble(year = 2000L, month = 1:12, value = 1)
check_keys_complete(complete, list(month = 1:12), .by = "year")

eleven <- tibble::tibble(year = 2000L, month = c(1:6, 8:12), value = 1)
try(check_keys_complete(eleven, list(month = 1:12), .by = "year"))
#> Error in eval(expr, envir) : 
#>   1 expected key is missing from the lattice.
#> ✖ An aggregate over the keys that did arrive is a plausible number that no
#>   total, identity, `na.rm` choice or row count can tell from a complete one --
#>   nothing is missing as an "NA", the rows are simply not there.
#> ℹ Missing: "year=2000, month=7"
```
