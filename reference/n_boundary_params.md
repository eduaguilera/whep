# Planetary reactive-nitrogen boundary parameters.

Parameters defining the planetary boundary for anthropogenic reactive
nitrogen and its agri-food-system apportionment, used by the SJOS-N
per-capita boundary axis. The low, high and top values bracket the
published planetary reactive-nitrogen limit; the per-capita cap, the
synthetic-to-total agricultural ratio and the food share of agricultural
nitrogen scale that global limit to a comparable per-capita agricultural
basis.

## Usage

``` r
n_boundary_params
```

## Format

A tibble in long form with columns:

- parameter:

  Parameter name (e.g. `"boundary_low"`, `"per_capita_cap"`,
  `"syn_tot_agri_ratio"`).

- value:

  Numeric parameter value.

- unit:

  Unit of the value (e.g. `"Tg N/yr"`, `"kg N/cap/yr"`, `"ratio"`,
  `"fraction"`).

- description:

  Human-readable description of the parameter.

## Source

Planetary reactive-nitrogen boundary literature: de Vries, W., Kros, J.,
Kroeze, C. & Seitzinger, S. P. (2013). Assessing planetary and regional
nitrogen boundaries related to food security and adverse environmental
impacts. *Current Opinion in Environmental Sustainability*, 5(3-4),
392-402.
[doi:10.1016/j.cosust.2013.07.004](https://doi.org/10.1016/j.cosust.2013.07.004)
; Campbell, B. M., Beare, D. J., Bennett, E. M., Hall-Spencer, J. M.,
Ingram, J. S. I., Jaramillo, F., Ortiz, R., Ramankutty, N., Sayer, J. A.
& Shindell, D. (2017). Agriculture production as a major driver of the
Earth system exceeding planetary boundaries. *Ecology and Society*,
22(4):8.
[doi:10.5751/ES-09595-220408](https://doi.org/10.5751/ES-09595-220408) ;
Springmann, M. et al. (2018). Options for keeping the food system within
environmental limits. *Nature*, 562, 519-525.
[doi:10.1038/s41586-018-0594-0](https://doi.org/10.1038/s41586-018-0594-0)
; regional agricultural-nitrogen boundaries: Schulte-Uebbing, L. F.,
Beusen, A. H. W., Bouwman, A. F. & de Vries, W. (2022). From planetary
to regional boundaries for agricultural nitrogen pollution. *Nature*,
610, 507-512.
[doi:10.1038/s41586-022-05158-2](https://doi.org/10.1038/s41586-022-05158-2)
. The specific low/high/top boundary values, the per-capita cap and the
agricultural-apportionment ratios are transcribed from the Global SJOS-N
analysis synthesising these sources; verify against Zotero before any
manuscript use.

## Examples

``` r
n_boundary_params
#> # A tibble: 6 × 4
#>   parameter           value unit        description                             
#>   <chr>               <dbl> <chr>       <chr>                                   
#> 1 boundary_low        60    Tg N/yr     Lower estimate of the planetary boundar…
#> 2 boundary_high      125    Tg N/yr     Upper estimate of the planetary boundar…
#> 3 boundary_top       205    Tg N/yr     Top reactive-N boundary estimate boundi…
#> 4 per_capita_cap      40    kg N/cap/yr Per-capita anthropogenic reactive-nitro…
#> 5 syn_tot_agri_ratio   1.53 ratio       Scaling from synthetic to total agricul…
#> 6 food_agri_share      0.95 fraction    Food share of agricultural reactive nit…
```
