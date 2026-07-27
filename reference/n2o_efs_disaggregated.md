# Disaggregated direct soil N2O emission factors by climate and irrigation.

Direct nitrous-oxide emission factors (fraction of applied nitrogen
emitted as N2O-N) disaggregated by climate zone and irrigation type.
Atlantic strata use the IPCC Tier 1 default of 0.01 (row
`irrig_type == "Tier_1"`); Mediterranean strata use the Cayuela et al.
(2017) meta-analytic factors that resolve the strong effect of water
management on N2O. Atlantic non-flooded irrigation rows carry a missing
factor because Atlantic strata are routed to the `"Tier_1"` factor; the
Atlantic flooded row keeps its own value.

## Usage

``` r
n2o_efs_disaggregated
```

## Format

A tibble with columns:

- irrig_type:

  Irrigation / management stratum: one of `"Tier_1"`, `"Rainfed"`,
  `"Traditional"`, `"Drip"`, `"Sprinkler"`, `"Flooded"`,
  `"Med_average"`.

- climate:

  Climate zone: `"MED"` (Mediterranean) or `"ATL"` (Atlantic).

- ef:

  Direct N2O emission factor (kg N2O-N per kg applied N); `NA` for
  Atlantic non-flooded irrigation strata.

## Source

Cayuela, M. L., Aguilera, E., Sanz-Cobena, A., Adams, D. C., Abalos, D.,
Barton, L., Ryals, R., Silver, W. L., Alfaro, M. A., Pappa, V. A.,
Smith, P., Garnier, J., Billen, G., Bouwman, L., Bondeau, A. &
Lassaletta, L. (2017). Direct nitrous oxide emissions in Mediterranean
climate cropping systems: emission factors based on a meta-analysis of
available measurement data. *Agriculture, Ecosystems & Environment*,
238, 25-35.
[doi:10.1016/j.agee.2016.10.006](https://doi.org/10.1016/j.agee.2016.10.006)
. Precursor Mediterranean N2O review: Aguilera, E., Lassaletta, L.,
Sanz-Cobena, A., Garnier, J. & Vallejo, A. (2013). The potential of
organic fertilizers and water management to reduce N2O emissions in
Mediterranean climate cropping systems. A review. *Agriculture,
Ecosystems & Environment*, 164, 32-52.
[doi:10.1016/j.agee.2012.09.006](https://doi.org/10.1016/j.agee.2012.09.006)
. Atlantic Tier 1 default: IPCC (2019), 2019 Refinement to the 2006 IPCC
Guidelines for National Greenhouse Gas Inventories, Vol. 4, Chapter 11.

## Examples

``` r
n2o_efs_disaggregated
#> # A tibble: 12 × 3
#>    irrig_type  climate      ef
#>    <chr>       <chr>     <dbl>
#>  1 Tier_1      ATL      0.01  
#>  2 Rainfed     MED      0.0027
#>  3 Traditional MED      0.0047
#>  4 Drip        MED      0.0051
#>  5 Sprinkler   MED      0.0091
#>  6 Flooded     MED      0.0019
#>  7 Med_average MED      0.005 
#>  8 Rainfed     ATL     NA     
#>  9 Traditional ATL     NA     
#> 10 Drip        ATL     NA     
#> 11 Sprinkler   ATL     NA     
#> 12 Flooded     ATL      0.005 
```
