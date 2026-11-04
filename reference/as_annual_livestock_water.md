# Validate annual livestock consumption against cohort exposure.

Adapt extensive direct-water quantities to an annual consumption ledger,
matching an independently supplied cohort inventory before any
aggregation. Drinking requirements and free drinking intake remain
distinct from net consumptive blue water. FAO (2019), *Water use of
livestock production systems and supply chains*, sections 4.5.4–4.5.5,
distinguishes supplied, returned and evaporated water (ISBN
978-92-5-131713-6). No return fraction, animal-day exposure, year or
territory is inferred by this adapter.

Source and inventory scope declarations are attestations, not proof of
applicability or completeness. The inventory must enumerate all cohorts
for the stated target scope independently of rate availability. Annual
snapshot heads are not automatically annual animal-days. This function
preserves canonical identities and does not aggregate compatibility
buckets.

## Usage

``` r
as_annual_livestock_water(ledger, inventory, coverage = c("abort", "partial"))
```

## Arguments

- ledger:

  Tibble of extensive water quantities, for example from
  [`calculate_livestock_water()`](https://eduaguilera.github.io/whep/reference/calculate_livestock_water.md).
  It must carry the inventory matching columns below, `quantity_basis`,
  `consumption_m3`, `coverage_direct_water`, `consumption_source`,
  `consumption_basis`, `water_origin`, `consumption_boundary` and
  `boundary_source`. Eligible quantities have complete drinking/service
  coverage, known nonnegative consumption,
  `water_origin = "surface_or_groundwater"`, and
  `consumption_boundary = "net_freshwater_consumption"`, describing
  withdrawals less returns to the same relevant freshwater system and
  assessment interval. A directly supplied
  `"net_consumptive_blue_water"` quantity requires its own consumption
  provenance. A `"withdrawal"` ledger additionally requires
  drinking/service volumes and explicit fractions, whose calculation
  must match the supplied consumption. Requirement and intake rows can
  be retained under partial coverage but never become consumptive water.
  Unknown metadata may be supplied as NA.

- inventory:

  Tibble unique by `exposure_id`, with `year`, canonical
  `reporting_polity_code`, `item_cbs_code`, `cohort`,
  `production_system`, `animal_days`, `period_start`, `period_end`,
  `inventory_source`, and `inventory_scope`. Dates must be Date vectors
  within the single declared calendar year. Each ledger ID must match
  these identity/exposure columns exactly; missing ledger IDs remain
  visible as missing exposure. Inventory metadata are retained in the
  `exposure_inventory` attribute.

- coverage:

  `"abort"` requires eligible quantities for every inventory row.
  `"partial"` warns and retains all inventory rows, with unknown
  `impact_u` for uncovered rows. It does not declare a complete
  footprint.

## Value

A tibble retaining ledger fields and inventory identities, with
`coverage_status`, `impact_u` (eligible consumptive cubic metres only),
`impact_quantity_basis`, `method_livestock_water` and
`polity_year_basis`. The source ledger and independent inventory are
retained as attributes, excluding the unrelated derived CBS extract
cache (`.cb_extracts`). Source provenance columns and other attributes
are preserved. Structural mismatches and invalid annual scope always
abort, even under partial coverage.

## Examples

``` r
# Entirely illustrative quantities, not observed farm data.
identity <- tibble::tibble(year = 2000L, area_code = 203L) |>
  add_polity_code()
inventory <- tibble::tibble(
  exposure_id = "illustrative", year = 2000L,
  reporting_polity_code = identity$polity_code, item_cbs_code = 960L,
  cohort = "lactating", production_system = "illustrative",
  animal_days = 100, period_start = as.Date("2000-01-01"),
  period_end = as.Date("2000-12-31"), inventory_source = "Toy inventory",
  inventory_scope = "Toy herd with one cohort"
)
ledger <- dplyr::mutate(
  inventory, quantity_basis = "net_consumptive_blue_water",
  consumption_m3 = 3, coverage_direct_water = "complete",
  consumption_source = "Toy water balance", consumption_basis = "assumed",
  water_origin = "surface_or_groundwater",
  consumption_boundary = "net_freshwater_consumption",
  boundary_source = "Toy matched supply and returns"
)
as_annual_livestock_water(ledger, inventory)
#> # A tibble: 1 × 24
#>   exposure_id  year reporting_polity_code item_cbs_code cohort production_system
#>   <chr>       <int> <chr>                         <int> <chr>  <chr>            
#> 1 illustrati…  2000 ESP-1800-2025                   960 lacta… illustrative     
#> # ℹ 18 more variables: animal_days <dbl>, period_start <date>,
#> #   period_end <date>, polity_year_basis <chr>, inventory_source <chr>,
#> #   inventory_scope <chr>, quantity_basis <chr>, consumption_m3 <dbl>,
#> #   coverage_direct_water <chr>, consumption_source <chr>,
#> #   consumption_basis <chr>, water_origin <chr>, consumption_boundary <chr>,
#> #   boundary_source <chr>, coverage_status <chr>, impact_u <dbl>,
#> #   impact_quantity_basis <chr>, method_livestock_water <chr>
```
