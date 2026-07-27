# Soil organic carbon turnover parameters by model.

Per-model structural parameters for the five soil organic carbon
turnover models ported in Module B: HSOC (Spain two-pool plus inert
organic matter), RothC (five pools), ICBM (two pools), AMG (active plus
stable), and Century (five pools). Stored in long form so a builder can
assemble each model's pool rate constants, initial pool fractions,
inter-pool transfer fractions, and texture/lignin defaults. Climate rate
modifiers are model-native and live in the turnover functions, not in
this table.

## Usage

``` r
soc_turnover_params
```

## Format

A tibble in long form with columns:

- model:

  Model namespace: one of `"hsoc"`, `"rothc"`, `"icbm"`, `"amg"`,
  `"century"`.

- component:

  Pool or structural component the parameter belongs to (e.g. `"fresh"`,
  `"dpm"`, `"young"`, `"active"`, `"str"`, `"defaults"`).

- parameter:

  Parameter name (e.g. `"decomposition_rate"`, `"init_active_fraction"`,
  `"transfer_fraction"`, `"base_rate_weekly"`).

- value:

  Numeric parameter value.

- unit:

  Unit of the value (e.g. `"per_year"`, `"fraction"`, `"per_week"`).

- description:

  Human-readable description of the parameter.

## Source

RothC: Coleman, K. & Jenkinson, D. S. (1996). RothC-26.3: A model for
the turnover of carbon in soil. In D. S. Powlson et al. (Eds.),
*Evaluation of Soil Organic Matter Models* (NATO ASI Series I, Vol. 38,
pp. 237-246). Springer.
[doi:10.1007/978-3-642-61094-3_17](https://doi.org/10.1007/978-3-642-61094-3_17)
. ICBM: Andren, O. & Katterer, T. (1997). ICBM: The introductory carbon
balance model for exploration of soil carbon balances. *Ecological
Applications*, 7(4), 1226-1236.
[doi:10.1890/1051-0761(1997)007\[1226:ITICBM\]2.0.CO;2](https://doi.org/10.1890/1051-0761%281997%29007%5B1226%3AITICBM%5D2.0.CO%3B2)
. AMG: Saffih-Hdadi, K. & Mary, B. (2008). Modeling consequences of
straw residues export on soil organic carbon. *Soil Biology and
Biochemistry*, 40(3), 594-607.
[doi:10.1016/j.soilbio.2007.08.022](https://doi.org/10.1016/j.soilbio.2007.08.022)
. Century: Parton, W. J., Schimel, D. S., Cole, C. V. & Ojima, D. S.
(1987). Analysis of factors controlling soil organic matter levels in
Great Plains grasslands. *Soil Science Society of America Journal*,
51(5), 1173-1179.
[doi:10.2136/sssaj1987.03615995005100050015x](https://doi.org/10.2136/sssaj1987.03615995005100050015x)
; SoilR Century implementation: Sierra, C. A., Mueller, M. & Trumbore,
S. E. (2012). Models of soil organic matter decomposition: the SoilR
package, version 1.0. *Geoscientific Model Development*, 5, 1045-1060.
[doi:10.5194/gmd-5-1045-2012](https://doi.org/10.5194/gmd-5-1045-2012) .
Inert organic matter initialisation (RothC, HSOC): Falloon, P., Smith,
P., Coleman, K. & Marshall, S. (1998). Estimating the size of the inert
organic matter pool from total soil organic carbon content for use in
the Rothamsted carbon model. *Soil Biology and Biochemistry*, 30(8-9),
1207-1211.
[doi:10.1016/S0038-0717(97)00256-3](https://doi.org/10.1016/S0038-0717%2897%2900256-3)
.

## Examples

``` r
soc_turnover_params
#> # A tibble: 43 × 6
#>    model component parameter            value unit     description              
#>    <chr> <chr>     <chr>                <dbl> <chr>    <chr>                    
#>  1 hsoc  fresh     decomposition_rate    0.48 per_year Fresh pool first-order d…
#>  2 hsoc  humus     decomposition_rate    0.02 per_year Humus pool first-order d…
#>  3 hsoc  iom       decomposition_rate    0    per_year Inert organic matter poo…
#>  4 rothc dpm       decomposition_rate   10    per_year Decomposable plant mater…
#>  5 rothc rpm       decomposition_rate    0.3  per_year Resistant plant material…
#>  6 rothc bio       decomposition_rate    0.66 per_year Microbial biomass pool d…
#>  7 rothc hum       decomposition_rate    0.02 per_year Humified organic matter …
#>  8 rothc iom       decomposition_rate    0    per_year Inert organic matter poo…
#>  9 rothc input     dpm_rpm_ratio         1.44 ratio    Default ratio of decompo…
#> 10 rothc dpm       init_active_fraction  0.01 fraction Initial fraction of acti…
#> # ℹ 33 more rows
```
