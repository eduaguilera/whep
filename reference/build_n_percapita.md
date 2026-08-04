# Build country per-capita anthropogenic reactive nitrogen.

Aggregates a
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
long-format nitrogen-input tibble to the total anthropogenic reactive
nitrogen entering each country's agricultural land and divides by
population, giving the per-capita reactive nitrogen (kg N/cap/yr) that
[`build_n_boundary_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_percapita.md)
consumes as its `n_percapita` input. The default `"synthetic_bnf"`
framing (the Campbell / Global framing) sums the synthetic-fertiliser
and biological-nitrogen- fixation input terms using
`synthetic * syn_tot_agri_ratio + BNF`, the locked Campbell / Global
framing; recycled or internal terms (manure, deposition, urban,
soil-organic-matter mineralization) are excluded. Any finer grid key
(`lon`, `lat`, `item_cbs_code`) is aggregated away to the country total,
and country-years without a matching population row are dropped. The
chosen framing is stamped on every row.

## Usage

``` r
build_n_percapita(
  n_inputs,
  population,
  framing = c("synthetic_bnf"),
  params = NULL,
  example = FALSE
)
```

## Arguments

- n_inputs:

  A
  [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
  long-format output with `fert_type`, `n_input_t` and the `year`,
  `area_code` keys (finer grid keys such as `lon`/`lat`/`item_cbs_code`
  are summed away).

- population:

  A tibble keyed by `year`, `area_code` with `population` (absolute
  persons).

- framing:

  How the total anthropogenic reactive nitrogen is defined.
  `"synthetic_bnf"` (default) scales the `"synthetic"` term by
  `syn_tot_agri_ratio` and adds the `"bnf"` term; other framings can be
  added.

- params:

  Boundary parameters, defaulting to
  [n_boundary_params](https://eduaguilera.github.io/whep/reference/n_boundary_params.md),
  used here for `syn_tot_agri_ratio`.

- example:

  If `TRUE`, return a small fixture instead of computing from
  `n_inputs`/`population`. Defaults to `FALSE`.

## Value

A tibble keyed by `year`, `area_code` with `n_percapita_kg`, the country
total anthropogenic reactive nitrogen per capita (kg N/cap/yr), and
`framing`, the anthropogenic definition it was computed under.

## Examples

``` r
build_n_percapita(example = TRUE)
#> # A tibble: 2 × 4
#>    year area_code n_percapita_kg framing      
#>   <int>     <int>          <dbl> <chr>        
#> 1  2000        10            8.5 synthetic_bnf
#> 2  2000        20           22   synthetic_bnf
```
