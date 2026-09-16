# Soil carbon-to-nitrogen ratios for organic-matter balances.

Soil carbon-to-nitrogen ratios used to convert a soil organic carbon
stock change into net nitrogen mineralization (when carbon is lost) or
net nitrogen sequestration (when carbon accumulates), by cropland class
and management system.

## Usage

``` r
soil_cn_ratios
```

## Format

A tibble with columns:

- cropland_class:

  Land class: `"Cropland"` or `"NonCropland"`.

- management:

  Management system: `"Conventional"` or `"Organic"`. **Only the
  `"Conventional"` rows are selectable by this package.**
  [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  has no management dimension – it is a global gridded balance – so an
  argument for this column could only be set world-wide, and "run the
  whole world as organic" is not a run anyone should perform. The
  `"Organic"` rows therefore ship as *reference values for downstream
  consumers*, not as inputs this package can be asked to use (issue
  809).

- cn_ratio:

  Soil organic-matter carbon-to-nitrogen ratio.

- cn_mineralization:

  Carbon-to-nitrogen ratio applied when soil organic carbon is
  mineralized (net carbon loss).

- cn_sequestration:

  Carbon-to-nitrogen ratio applied when soil organic carbon is
  sequestered (net carbon gain).

## Source

**Expert parameterisation with a documented rationale and no citation.**
Traced through the Spain historical pipeline's coefficient workbook
(`input/SOC_coefs.xlsx`, sheet `Soil_CN_ratios`), which has no source
column and no notes column, as does the packaged
`inst/extdata/balances/soil_cn_ratios.csv`:

- The bulk ratios (`cn_ratio` 10 cropland, 15 non-cropland) enter the
  workbook on 2025-03-14, in a sheet holding only `Cropland_class` and
  `CN_ratio`. The workbook's earliest tracked version (2021-02-19) has
  no such sheet at all.

- The asymmetry – management stratification plus `cn_mineralization` and
  `cn_sequestration` – is added on 2026-03-26, in a commit titled "Use
  asymmetric C:N ratios for SOC and add N-limitation on SOC
  sequestration".

The rationale is written up in that project's supplementary methods
("Asymmetric C:N ratios for SOC-nitrogen coupling"): a lower ratio on
mineralization for the microbial-biomass and labile pools that are
preferentially decomposed, a higher one on sequestration for stable
humus formation, and organic cropland sequestering at 13 against 11
conventional for the larger stable-humus fraction of manure- and
compost-derived carbon. No citation is attached to any of the values.

One published anchor exists nearby and is worth knowing: the same
supplement cites Cleveland & Liptzin (2007),
[doi:10.1007/s10533-007-9132-0](https://doi.org/10.1007/s10533-007-9132-0)
, for a soil microbial-biomass C:N of roughly 8-13, and the cropland
`cn_mineralization` values (8 and 9) fall inside it. It is cited there
for a different parameter, and it is not offered here as the source of
these numbers – only as the nearest published range they are consistent
with.

This entry previously cited Coleman & Jenkinson's RothC (1996) as the
framework the values are "consistent with". That attribution has been
removed: RothC is a carbon-only model and sets no carbon-to-nitrogen
ratio at all –
[`calculate_soc_rothc`](https://eduaguilera.github.io/whep/reference/calculate_soc_rothc.md)
returns `dpm`, `rpm`, `bio`, `hum`, `iom` and `soc_total`, and no
nitrogen anywhere – so no RothC reference can be a source for these
numbers. Naming one made an undocumented coefficient set read as a
sourced one (whep#346).

The values move published nitrogen: every soil-carbon stock change is
divided by one of them to reach mineralized or sequestered nitrogen.
Whether to cite Cleveland & Liptzin for the mineralization ratios, and
what to cite for the rest, is whep#346 and still open.

## What the asymmetry represents

The two directional ratios are not two process stoichiometries. They
express the FLEXIBILITY of soil C:N: a soil gaining carbon is expected
to move to a wider ratio, a soil losing carbon to a narrower one, so the
pair brackets the bulk value. Cropland is 8 – 10 – 11 and non-cropland
11 – 15 – 15.

This is why both are documented as applying to the *net* change: the
ratio describes where a given soil's C:N is heading, which is a property
of the soil, not of an individual crop or field on part of it. Applying
the choice at a finer grain than the soil makes the two directions
non-cancelling and manufactures nitrogen at a soil whose carbon did not
change.

The bounds themselves remain unsourced; see the source section below.
What is wanted is a published *marginal* C:N – the ratio of the CHANGE
in soil carbon to the change in soil nitrogen – not a bulk soil C:N,
which is a different quantity.

## Examples

``` r
soil_cn_ratios
#> # A tibble: 4 × 5
#>   cropland_class management   cn_ratio cn_mineralization cn_sequestration
#>   <chr>          <chr>           <dbl>             <dbl>            <dbl>
#> 1 Cropland       Conventional       10                 8               11
#> 2 Cropland       Organic            10                 9               13
#> 3 NonCropland    Conventional       15                11               15
#> 4 NonCropland    Organic            15                11               15
```
