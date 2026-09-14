# Refuse a value vocabulary that no longer contains the labels a filter needs

Assert that each label a downstream
[`filter()`](https://rdrr.io/r/stats/filter.html) selects on actually
occurs in the column it selects from. A label that has been renamed
upstream does not raise: the filter matches no rows, the sum of no rows
is zero, and the zero is indistinguishable from a measurement.

This is the sibling of
[`check_inputs_supplied()`](https://eduaguilera.github.io/whep/reference/check_inputs_supplied.md)
for the case where the column is present and populated but its
**vocabulary** has moved. It is the shape of whep#1016, where three
FAOSTAT emission `Element` labels were lost to a pin revision and
roughly 108 Tg CH4/yr shipped as literal zero, and it is invisible to
any check written around `coalesce()`, `replace_na()` or `na.rm` – none
of those appear anywhere near it.

The message names the labels that are **present**, up to six of them, as
well as the ones that are missing. A rename is only obvious once the new
spelling is in front of the reader.

A table with **no rows** carries no vocabulary and is passed, for the
same reason
[`check_inputs_supplied()`](https://eduaguilera.github.io/whep/reference/check_inputs_supplied.md)
does not judge one: an empty table is an absent table rather than a
moved label, and the caller that produced it is the one to answer for
it. The case this is for – whep#1016 – is a pin of 2.5 million rows
whose labels moved, not an empty one.

## Usage

``` r
check_labels_supplied(
  data,
  column,
  labels,
  action = c("abort", "warn"),
  details = NULL
)
```

## Arguments

- data:

  A tibble carrying the vocabulary to check.

- column:

  Name of the column the labels are drawn from. A column that is not
  there at all is reported as the whole vocabulary being absent, even on
  a table with no rows.

- labels:

  Labels that must each occur at least once. Compared as character, so a
  numeric code may be given as a number.

- action:

  `"abort"` (default) to raise, `"warn"` to carry on.

- details:

  Extra `cli` bullets appended to the message.

## Value

`data`, invisibly. Raises or warns with class `whep_absent_label` (and
`whep_absent_input`) when a label is missing; the condition carries the
missing labels in its `absent` field and what was seen instead in its
`observed` field.

## Examples

``` r
landuse <- tibble::tibble(Element = c("Area", "Area"), Value = c(1, 2))
check_labels_supplied(landuse, "Element", "Area")

renamed <- tibble::tibble(Element = c("Area under cultivation"), Value = 1)
try(check_labels_supplied(renamed, "Element", "Area"))
#> Error in eval(expr, envir) : 
#>   Label "Area" is not in the Element vocabulary.
#> ✖ A filter on a label nothing carries matches no rows, and the sum of no rows
#>   is zero -- which no total, conservation check or row count downstream can
#>   tell from a measurement.
#> ℹ Element holds "Area under cultivation".
```
