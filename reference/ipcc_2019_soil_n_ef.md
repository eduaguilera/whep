# IPCC 2019 Tier 1 soil nitrogen emission and leaching factors.

IPCC 2019 Refinement Tier 1 default factors for managed soils: direct
N2O emission factors for N inputs (EF1), flooded rice (EF1FR) and urine
and dung deposited by grazing animals (EF3PRP), the indirect N2O factors
for volatilisation and redeposition (EF4) and for leaching and runoff
(EF5), and the leaching fraction FracLEACH-(H). Many are given as an
aggregated value and as wet and dry climate values. A climate is wet if
annual precipitation / potential evapotranspiration (PET) is greater
than 1 in temperate and boreal climates, or annual precipitation is
greater than 1000 mm in tropical climates (Table 11.1, footnote 4);
otherwise it is dry.

## Usage

``` r
ipcc_2019_soil_n_ef
```

## Format

A tibble with columns:

- parameter:

  IPCC parameter name: `"EF1"`, `"EF1FR"`, `"EF3PRP_CPP"`,
  `"EF3PRP_SO"`, `"EF4"`, `"EF5"` or `"FracLEACH_H"`.

- disaggregation:

  Climate or water-management stratum the value applies to, e.g.
  `"aggregated"`, `"wet"`, `"dry"`.

- value:

  The factor.

- unit:

  Unit of the factor.

- source:

  Locator of the value in the IPCC 2019 Refinement.

## Source

IPCC (2019), 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Vol. 4, Chapter 11: Table 11.1 (Updated) p.
11.13; Table 11.3 (Updated) p. 11.26; Section 11.2.2.2 p. 11.25.

## Examples

``` r
ipcc_2019_soil_n_ef
#> # A tibble: 18 × 5
#>    parameter   disaggregation           value unit              source          
#>    <chr>       <chr>                    <dbl> <chr>             <chr>           
#>  1 EF1         aggregated               0.01  kg N2O-N per kg N IPCC 2019 Refin…
#>  2 EF1         synthetic_wet            0.016 kg N2O-N per kg N IPCC 2019 Refin…
#>  3 EF1         other_wet                0.006 kg N2O-N per kg N IPCC 2019 Refin…
#>  4 EF1         all_dry                  0.005 kg N2O-N per kg N IPCC 2019 Refin…
#>  5 EF1FR       aggregated               0.004 kg N2O-N per kg N IPCC 2019 Refin…
#>  6 EF1FR       continuous_flooding      0.003 kg N2O-N per kg N IPCC 2019 Refin…
#>  7 EF1FR       single_multiple_drainage 0.005 kg N2O-N per kg N IPCC 2019 Refin…
#>  8 EF3PRP_CPP  aggregated               0.004 kg N2O-N per kg N IPCC 2019 Refin…
#>  9 EF3PRP_CPP  wet                      0.006 kg N2O-N per kg N IPCC 2019 Refin…
#> 10 EF3PRP_CPP  dry                      0.002 kg N2O-N per kg N IPCC 2019 Refin…
#> 11 EF3PRP_SO   aggregated               0.003 kg N2O-N per kg N IPCC 2019 Refin…
#> 12 EF4         aggregated               0.01  kg N2O-N per kg N IPCC 2019 Refin…
#> 13 EF4         wet                      0.014 kg N2O-N per kg N IPCC 2019 Refin…
#> 14 EF4         dry                      0.005 kg N2O-N per kg N IPCC 2019 Refin…
#> 15 EF5         aggregated               0.011 kg N2O-N per kg N IPCC 2019 Refin…
#> 16 FracLEACH_H wet                      0.24  kg N per kg N     IPCC 2019 Refin…
#> 17 FracLEACH_H dry                      0     kg N per kg N     IPCC 2019 Refin…
#> 18 FracLEACH_H dry_irrigated_non_drip   0.24  kg N per kg N     IPCC 2019 Refin…
```
