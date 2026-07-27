# Fertiliser-type modifying factors for direct soil N2O.

Multiplicative modifying factors applied to the disaggregated direct N2O
emission factor by fertiliser type and climate zone. They scale the
climate-by-irrigation emission factor up or down according to the
nitrogen source. Mediterranean factors follow the Cayuela et al. (2017)
meta-analysis for synthetic, solid and liquid sources and the IPCC
(2019) dry-area values for excreta; Atlantic factors follow the IPCC
(2019) wet-area values. Missing factors mark sources whose N2O is not
modelled through this pathway in the Mediterranean (recycled organic
fertilisers, soil organic matter, urban N).

## Usage

``` r
fertiliser_n2o_modifiers
```

## Format

A tibble with columns:

- fert_type:

  Nitrogen source: one of `"Synthetic"`, `"Solid"`, `"Liquid"`,
  `"Recycling"`, `"Excreta_cattle_monog"`, `"Excreta_other"`, `"SOM"`,
  `"Urban"`.

- climate:

  Climate zone: `"MED"` or `"ATL"`.

- mf:

  Multiplicative modifying factor on the N2O emission factor; `NA` where
  the source is not modelled through this pathway.

- source:

  Provenance note transcribed from the coefficient workbook (Cayuela et
  al. 2017 or IPCC 2019).

## Source

Cayuela, M. L. et al. (2017). Direct nitrous oxide emissions in
Mediterranean climate cropping systems. *Agriculture, Ecosystems &
Environment*, 238, 25-35.
[doi:10.1016/j.agee.2016.10.006](https://doi.org/10.1016/j.agee.2016.10.006)
. IPCC (2019), 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Vol. 4, Chapter 11.

## Examples

``` r
fertiliser_n2o_modifiers
#> # A tibble: 16 × 4
#>    fert_type            climate    mf source                                    
#>    <chr>                <chr>   <dbl> <chr>                                     
#>  1 Synthetic            MED      1    Cayuela et al., 2017                      
#>  2 Solid                MED      0.38 Cayuela et al., 2017                      
#>  3 Liquid               MED      1.7  Cayuela et al., 2017                      
#>  4 Recycling            MED      0    Cayuela et al., 2017, solid organic ferti…
#>  5 Excreta_cattle_monog MED      0.2  IPCC, 2019, dry areas                     
#>  6 Excreta_other        MED      0.3  IPCC, 2019, dry areas                     
#>  7 SOM                  MED      0    Cayuela et al., 2017, solid organic ferti…
#>  8 Urban                MED      0    Cayuela et al., 2017, solid organic ferti…
#>  9 Synthetic            ATL      1.6  IPCC, 2019, wet areas, synthetic          
#> 10 Solid                ATL      0.6  IPCC, 2019, wet areas, other N inputs     
#> 11 Liquid               ATL      0.6  IPCC, 2019, wet areas, other N inputs     
#> 12 Recycling            ATL      0.6  IPCC, 2019, wet areas, other N inputs     
#> 13 Excreta_cattle_monog ATL      0.6  IPCC, 2019, wet areas                     
#> 14 Excreta_other        ATL      0.3  IPCC, 2019, wet areas                     
#> 15 SOM                  ATL      0.6  IPCC, 2019, wet areas, other N inputs     
#> 16 Urban                ATL      0.6  IPCC, 2019, wet areas, other N inputs     
```
