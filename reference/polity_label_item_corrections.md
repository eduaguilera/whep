# Source label corrections scoped to one item

Rows a source files under ANOTHER territory's label for one item,
published by `whep-polities`. An alias maps a label to a polity with no
item dimension, so it cannot say that Mitchell's pre-1910
`"south africa"` sugar cane is Natal's while the other items under that
label are the Cape's. Each rule replaces the label before resolution;
[`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md)
applies them when it is given `item`.

## Usage

``` r
polity_label_item_corrections
```

## Format

A tibble with one row per rule. It has zero rows when the shipped
snapshot was taken from a `whep-polities` revision that published no
rules. Columns:

- `source`: Source slug the rule applies to.

- `source_label`: The label the source files the rows under.

- `item`: The item, exactly as the source writes it.

- `year_start`, `year_end`: Inclusive year range.

- `unit`, `indicator`: Scope of the rule. `NA` (blank upstream) means
  any; when set, the row's own unit / indicator must equal it. All `NA`
  in a snapshot taken before whep-polities introduced the columns.

- `correct_label`: The label to resolve instead.

- `polity_code`: Where `correct_label` resolves in the same upstream
  revision, or `"UNROUTED"` when the rows belong to no polity and
  [`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md)
  leaves them unassigned.

- `observed_rows`: Source rows the rule relabels upstream.

- `issue`: The `whep-polities` issue that decided the rule.

- `evidence`: Why the rows belong to the other territory.

## Source

`~/whep-polities/data/final/source_label_item_corrections.csv`.

## Details

A row matches a rule when `source`, `source_label` (normalised as the
alias map's labels are) and `item` all match and the row's year lies in
`[year_start, year_end]`. Rows without a year are never corrected, and
rules do not chain. A rule with `unit` or `indicator` set also needs the
row's own unit / indicator to equal it.
