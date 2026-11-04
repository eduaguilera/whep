# Attribute grazing water with an explicit accounting basis.

Calculate water attributed to grazing from supplied evapotranspiration
and biomass or valuation inputs. This calculator preserves the input's
spatial and temporal context; it does not infer a reporting territory or
annualize a study period. It does not download data or choose missing
parameters.

`biomass_allocation` multiplies occupied-domain ET by consumed dry
matter divided by reference available dry matter. Heinke et al. (2020),
doi:10.1029/2019WR026995, Methods p. 3, use this attribution principle
with ET and maximum biomass yield from dedicated grazing-intensity
simulations. This function does not turn whole-plant NPP or realized
herd intake into that reference yield. The caller must identify the
reference basis and distinguish sourced estimates from explicit
unverified assumptions. Reference ET and biomass must describe the same
management scenario: ET from an actual stocking-rate run cannot silently
be paired with a biomass maximum from a different experiment.

`occupation` attributes all supplied grassland ET to occupation of that
domain. `value_allocation` applies a supplied economic allocation
fraction to the same domain. Schyns et al. (2019),
doi:10.1073/pnas.1817380116, Supplement S3, describe economic
attribution; this function applies the supplied fraction and does not
derive or validate its ecosystem valuation. These methods answer
different accounting questions. None is a silent fallback for
unavailable inputs to another method.

## Usage

``` r
calculate_grazing_water(
  data,
  method = c("biomass_allocation", "occupation", "value_allocation"),
  excess = c("abort", "extrapolate")
)
```

## Arguments

- data:

  A tibble with `component` (`"green"` or `"blue"`), `grass_water_m3`
  (ET over the reference grassland domain), and `source_water`. Biomass
  attribution also requires `grass_intake_dm_t`, `reference_dm_t`,
  `source_intake`, `source_reference`, `reference_basis` and
  `reference_status` (`"sourced"` or `"assumed_unverified"`). Matching
  `water_scenario` and `reference_scenario` labels identify the common
  reference experiment. An assumed reference requires a nonempty
  `reference_assumption` description. Economic attribution requires
  `value_fraction` between zero and one and `source_fraction`.
  Quantities must share the same domain and period. Source labels record
  provenance assertions; they are not verification. The input must not
  already contain this function's output columns.

- method:

  Attribution method. The default `"biomass_allocation"` requires the
  supplied biomass reference. Alternatives are `"occupation"` and
  `"value_allocation"`.

- excess:

  Policy when intake exceeds reference biomass. `"abort"` (default)
  requires reconciliation of the inputs. `"extrapolate"` warns,
  preserves all intake, and reports an intensity-based water requirement
  which can exceed supplied ET. Such rows are not a physical ET
  partition.

## Value

The input tibble with `allocation_fraction`, `water_attributed_m3`,
`water_unallocated_m3`, `excess_intake_dm_t`, `quantity_basis` and
`method_grazing_water`. Extrapolated excess rows have unknown
unallocated water, recorded as `NA`, rather than a negative physical
remainder. Non-biomass methods leave excess intake unknown. A declared
zero intake and zero reference allocates zero water; positive intake
against zero reference aborts under either policy. Green and blue rows
can repeat the same intake/reference context. Do not sum those repeated
biomass quantities across water components as separate feed. Compare
methods by reusing the original inputs, not these results.

## Examples

``` r
# Illustrative input, not a scientific coefficient or reference value.
data <- tibble::tribble(
  ~component, ~grass_water_m3, ~source_water,
  "green",    1000,            "Illustrative ET fixture"
)
calculate_grazing_water(data, method = "occupation")
#> # A tibble: 1 × 9
#>   component grass_water_m3 source_water   allocation_fraction excess_intake_dm_t
#>   <chr>              <dbl> <chr>                        <dbl>              <dbl>
#> 1 green               1000 Illustrative …                   1                 NA
#> # ℹ 4 more variables: water_attributed_m3 <dbl>, water_unallocated_m3 <dbl>,
#> #   quantity_basis <chr>, method_grazing_water <chr>
```
