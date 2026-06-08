# Interactive footprint Sankey viewer

Create a browser-based Sankey viewer from a footprint table, such as the
output of
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md).
The viewer includes stage filters, a per-stage node limit controls, a
minimum-flow threshold, hover tooltips, click-to-highlight interactions,
and SVG download.

Each row is treated as one path through the selected `stages`, with
`value_col` giving the path size. Rows are summed before plotting, so
repeated paths are shown as one flow.

The default stages are `origin_area`, `origin_item`, `target_item`, and
`target_area`; `target_fd` is appended automatically when present. If
columns such as `origin_area_name` or `target_item_name` are present,
they are used as display labels.

## Usage

``` r
plot_footprint_sankey(
  footprints,
  stages = NULL,
  value_col = "value",
  label_cols = NULL,
  max_nodes = 10,
  other_label = "Other",
  stage_max_nodes = NULL,
  stage_other_labels = NULL,
  embed_max_nodes = Inf,
  stage_embed_max_nodes = NULL,
  min_share = 0,
  title = "Footprint Sankey Viewer",
  subtitle = NULL,
  value_label = "footprint",
  width = "100%",
  height = 680,
  file = NULL,
  open = FALSE
)
```

## Arguments

- footprints:

  A data frame with footprint paths and a numeric value column.

- stages:

  Character vector of columns to use as Sankey stages. Defaults to the
  footprint origin and target columns.

- value_col:

  Name of the numeric column containing flow values.

- label_cols:

  Optional named character vector mapping stage columns to display-label
  columns, for example `c(origin_area = "origin_area_name")`. When
  omitted, `{stage}_name` columns are used where available.

- max_nodes:

  Maximum number of individual nodes to show per stage in the current
  filtered browser view. Less important nodes are grouped into
  `other_label`. Use `Inf` to show all nodes.

- other_label:

  Label used for grouped nodes when `max_nodes` is finite.

- stage_max_nodes:

  Optional named numeric vector overriding `max_nodes` for selected
  stages, for example `c(product = 75, target_area = 12)`.

- stage_other_labels:

  Optional named character vector overriding `other_label` for selected
  stages, for example `c(product = "Other products")`.

- embed_max_nodes:

  Maximum number of individual nodes to embed per stage before writing
  the viewer. Less important nodes are permanently grouped before
  serialization, reducing file size. Use `Inf` to keep all nodes
  available to the browser.

- stage_embed_max_nodes:

  Optional named numeric vector overriding `embed_max_nodes` for
  selected stages.

- min_share:

  Minimum visible path size as a percentage of the current filtered
  total. Set to 0 to keep diffuse trade links visible. Users can change
  this in the browser.

- title:

  Optional viewer title.

- subtitle:

  Optional viewer subtitle.

- value_label:

  Label used in tooltips and the summary line.

- width:

  Viewer width as a CSS value.

- height:

  Sankey SVG height in pixels.

- file:

  Optional path to write a standalone HTML viewer.

- open:

  If `TRUE` and `file` is supplied, open the written viewer in the
  default browser. If `TRUE` and `file` is `NULL`, a temporary HTML file
  is written and opened.

## Value

A browsable HTML object when `file` is `NULL` and `open` is `FALSE`;
otherwise invisibly returns the HTML file path.

## Examples

``` r
footprints <- tibble::tribble(
  ~origin_area, ~origin_item, ~target_item, ~target_area, ~value,
  "Brazil", "Soybeans", "Pigmeat", "China", 10,
  "Brazil", "Soybeans", "Milk", "China", 4,
  "Brazil", "Soybeans", "Soybean Oil", "France", 3
)

if (
  interactive() &&
    requireNamespace("htmltools", quietly = TRUE) &&
    requireNamespace("jsonlite", quietly = TRUE)
) {
  plot_footprint_sankey(footprints)
}
```
