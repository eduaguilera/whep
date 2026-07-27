# Humification fraction by carbon input type.

Per-input-type fraction of soil carbon input that is stabilised directly
into humus (rather than entering the labile, fresh pool). The HSOC
turnover model takes a scalar humification fraction; this table supplies
the type-specific values so a carbon-input builder can compute a
carbon-weighted effective humification fraction per cell-year before
running the SOC model. Other models (RothC, ICBM, AMG, Century) use
their native carbon partition and ignore this table; AMG instead uses
`amg_h_by_input_type`. Values are transcribed from the Spain historical
agroecosystem `Biomass_coefs` `Residue_humified_kgC_kgC` column (manure
and roots from the corresponding manure and `Root_humified_kgC_kgC`
entries).

## Usage

``` r
residue_humification
```

## Format

A tibble with columns:

- input_type:

  Canonical carbon input type: `"crop_residue"`, `"root"`, `"weed"`,
  `"woody_residue"`, `"manure"`, `"excreta"`, `"urban"`,
  `"urban_compost"`, `"compost"`, `"green_manure"`.

- humified_fraction:

  Fraction of input carbon stabilised directly into humus (kg humified
  carbon per kg carbon input).

- description:

  Human-readable description of the input type and its Spain historical
  provenance.

## Source

Spain historical agroecosystem coefficient set (`Biomass_coefs.xlsx`,
`Residue_humified_kgC_kgC` and `Root_humified_kgC_kgC` columns), itself
compiled from soil organic carbon turnover and litter humification
studies including Andren, O. & Katterer, T. (1997).
[doi:10.1890/1051-0761(1997)007\[1226:ITICBM\]2.0.CO;2](https://doi.org/10.1890/1051-0761%281997%29007%5B1226%3AITICBM%5D2.0.CO%3B2)
; Katterer, T., Bolinder, M. A., Andren, O., Kirchmann, H. & Menichetti,
L. (2011). Roots contribute more to refractory soil organic matter than
above- ground crop residues, as revealed by a long-term field
experiment. *Agriculture, Ecosystems & Environment*, 141(1-2), 184-192.
[doi:10.1016/j.agee.2011.02.029](https://doi.org/10.1016/j.agee.2011.02.029)
.

## Examples

``` r
residue_humification
#> # A tibble: 10 × 3
#>    input_type    humified_fraction description                                  
#>    <chr>                     <dbl> <chr>                                        
#>  1 crop_residue              0.115 Aboveground herbaceous crop residue (cereals…
#>  2 root                      0.178 Belowground herbaceous root and rhizodeposit…
#>  3 weed                      0.115 Aboveground weed and spontaneous-grass bioma…
#>  4 woody_residue             0.325 Woody residue and prunings from fruit citrus…
#>  5 manure                    0.254 Livestock solid and liquid manure averaged o…
#>  6 excreta                   0.254 Livestock total excreta averaged over specie…
#>  7 urban                     0.320 Urban organic waste applied to soil (aggrega…
#>  8 urban_compost             0.386 Composted municipal solid waste; Spain_Hist …
#>  9 compost                   0.71  Stabilised compost amendment; Spain_Hist Bio…
#> 10 green_manure              0.115 Incorporated green manure (fresh herbaceous)…
```
