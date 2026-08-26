# Package index

## Build datasets

Build full datasets from raw FAOSTAT inputs. These functions orchestrate
reading, gap-filling, historical extension, and balance validation.

- [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
  : Build primary production dataset
- [`build_historical_land_areas()`](https://eduaguilera.github.io/whep/reference/build_historical_land_areas.md)
  : Build a pre-1962 land series measured on each year's own borders
- [`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
  : Build commodity balance sheets
- [`build_processing_coefs()`](https://eduaguilera.github.io/whep/reference/build_processing_coefs.md)
  : Build processing coefficients
- [`build_detailed_trade()`](https://eduaguilera.github.io/whep/reference/build_detailed_trade.md)
  : Build detailed bilateral trade matrix
- [`build_trade_prices()`](https://eduaguilera.github.io/whep/reference/build_trade_prices.md)
  : Build global trade prices
- [`build_primary_prices()`](https://eduaguilera.github.io/whep/reference/build_primary_prices.md)
  : Build primary item prices
- [`build_cbs_prices()`](https://eduaguilera.github.io/whep/reference/build_cbs_prices.md)
  : Build CBS item prices

## Spatialization

Disaggregate country-level agricultural data to a 0.5-degree grid.
[`run_spatialize()`](https://eduaguilera.github.io/whep/reference/run_spatialize.md)
wraps
[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
with named presets (LPJmL-faithful vs full WHEP) for direct cross-model
comparison.

- [`run_spatialize()`](https://eduaguilera.github.io/whep/reference/run_spatialize.md)
  : Run the gridded land-use spatialization pipeline
- [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  : Build gridded landuse dataset
- [`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)
  : Build gridded livestock dataset

## Polycell spatial support

Build the canonical spatial support unit, the polycell: a 0.5-degree
grid cell intersected with a polity over its validity interval, carrying
the territory decomposed into land, inland water and ice so that
polycells re-aggregate to a polity without value change or cross-border
leakage.

- [`build_polycell_land_uses()`](https://eduaguilera.github.io/whep/reference/build_polycell_land_uses.md)
  : Partition each polycell into mutually exclusive land uses.
- [`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
  : Build the polycell spatial support table
- [`expand_polycell_years()`](https://eduaguilera.github.io/whep/reference/expand_polycell_years.md)
  : Expand the interval-keyed polycell support to one row per year
- [`polycell_example_geometries()`](https://eduaguilera.github.io/whep/reference/polycell_example_geometries.md)
  : A minimal polity geometry table for examples and smoke tests
- [`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md)
  : Read the polycell support table from its registered pin
- [`read_glwd_water()`](https://eduaguilera.github.io/whep/reference/read_glwd_water.md)
  : Read the GLWD inland-water fraction on the 0.5-degree grid
- [`glwd_water_fraction()`](https://eduaguilera.github.io/whep/reference/glwd_water_fraction.md)
  : Derive the lake-and-river fraction of each 0.5-degree cell from GLWD
- [`read_glaciated_areas()`](https://eduaguilera.github.io/whep/reference/read_glaciated_areas.md)
  : Read the Natural Earth glaciated-areas ice layer
- [`read_luh2_terrestrial()`](https://eduaguilera.github.io/whep/reference/read_luh2_terrestrial.md)
  : Read the LUH2 terrestrial-area validation layer

## Constant-territory back-casting

Rebuild historical series onto a fixed reference-year’s boundaries by
dasymetric spatial reallocation, optionally weighted by LPJmL/WHEP
gridded covariates.

- [`build_constant_territory_series()`](https://eduaguilera.github.io/whep/reference/build_constant_territory_series.md)
  : Build a constant-territory time series for a reference year's
  boundaries
- [`make_lpjml_covariate()`](https://eduaguilera.github.io/whep/reference/make_lpjml_covariate.md)
  : Build an LPJmL/WHEP spatial covariate function

## LPJmL grass availability

Read managed-grassland net primary production from a finished LPJmL run
into grazable above-ground dry-matter availability, the forage supply
ceiling for feed allocation.

- [`build_grass_availability()`](https://eduaguilera.github.io/whep/reference/build_grass_availability.md)
  : Build grazable grass availability.
- [`build_grass_availability_lpjml()`](https://eduaguilera.github.io/whep/reference/build_grass_availability_lpjml.md)
  : Build grazable grass availability from an LPJmL run.
- [`grass_access_shares()`](https://eduaguilera.github.io/whep/reference/grass_access_shares.md)
  : Accessibility and conversion parameters for grazable grass
  availability.
- [`aggregate_grass_to_polity()`](https://eduaguilera.github.io/whep/reference/aggregate_grass_to_polity.md)
  : Aggregate gridded grass availability to polity totals.
- [`read_lpjml_grass_productivity()`](https://eduaguilera.github.io/whep/reference/read_lpjml_grass_productivity.md)
  : Read natural-grass productivity from an LPJmL run.

## Soil balances - water

Read LPJmL hydrology outputs (drainage, evapotranspiration components,
precipitation, irrigation, runoff, discharge, soil water content and the
per-crop net irrigation requirement) into tidy long form, close the
gridded soil water balance exposing the footprint-relevant terms
(precipitation, applied irrigation, blue and green consumptive water,
net irrigation requirement), and assemble the monthly climate drivers
for the soil-organic-carbon dynamics.

- [`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
  : Read an LPJmL hydrology variable into a tidy tibble.
- [`read_lpjml_npp()`](https://eduaguilera.github.io/whep/reference/read_lpjml_npp.md)
  : Read a per-PFT annual LPJmL carbon variable into a tidy tibble.
- [`read_cru_climate()`](https://eduaguilera.github.io/whep/reference/read_cru_climate.md)
  : Read a CRU TS 4.09 monthly climate variable into a tidy tibble.
- [`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md)
  : Build a gridded soil water balance from LPJmL hydrology.
- [`get_soc_climate_drivers()`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md)
  : Assemble monthly SOC climate drivers from CRU climate and LPJmL
  hydrology.

## Soil-balance coefficient tables

Structural parameter tables for the soil-organic-carbon turnover models
(HSOC, RothC, ICBM, AMG, Century), the AMG humification coefficient by
carbon input type, the soil carbon-to-nitrogen ratios used to convert
soil-carbon change into net nitrogen mineralization or sequestration,
the per-input-type humification fractions for the HSOC model, and the
USDA texture-class soil hydraulic properties (with the HWSD texture-code
crosswalk) feeding the ICBM moisture modifier.

- [`soc_turnover_params`](https://eduaguilera.github.io/whep/reference/soc_turnover_params.md)
  : Soil organic carbon turnover parameters by model.
- [`amg_h_by_input_type`](https://eduaguilera.github.io/whep/reference/amg_h_by_input_type.md)
  : AMG humification coefficient by carbon input type.
- [`soil_cn_ratios`](https://eduaguilera.github.io/whep/reference/soil_cn_ratios.md)
  : Soil carbon-to-nitrogen ratios for organic-matter balances.
- [`residue_humification`](https://eduaguilera.github.io/whep/reference/residue_humification.md)
  : Humification fraction by carbon input type.
- [`soc_soil_cover_curve`](https://eduaguilera.github.io/whep/reference/soc_soil_cover_curve.md)
  : Generic land-use soil-cover curve for the RothC/HSOC cover factor.
- [`soil_hydraulic_by_texture`](https://eduaguilera.github.io/whep/reference/soil_hydraulic_by_texture.md)
  : Soil hydraulic properties by USDA texture class.
- [`hwsd_texture_usda`](https://eduaguilera.github.io/whep/reference/hwsd_texture_usda.md)
  : HWSD topsoil USDA texture code to texture-class crosswalk.

## Nitrogen-loss coefficient tables

Coefficient datasets for the nitrogen-loss cascade (Module C): the
disaggregated direct N2O emission factors by climate and irrigation, the
fertiliser-type N2O modifying factors, the Meisinger and Randall topsoil
denitrification matrix with its drainage-class bins, the subsoil nitrate
reduction shares, the MANNER ammonia-volatilisation factor tables (plus
the synthetic-fertiliser rate/rainfall factors, the organic-manure
incorporation-delay factor, the inorganic nitrogen fraction by species
and the gross-default technique/incorporation-delay blend), the IPCC
2006 Tier 1 direct soil N2O factors, the soil organic matter content
bins, and the C:N leaching-attenuation and indirect-N2O constants.

- [`n2o_efs_disaggregated`](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
  : Disaggregated direct soil N2O emission factors by climate and
  irrigation.
- [`fertiliser_n2o_modifiers`](https://eduaguilera.github.io/whep/reference/fertiliser_n2o_modifiers.md)
  : Fertiliser-type modifying factors for direct soil N2O.
- [`meisinger_denitrification`](https://eduaguilera.github.io/whep/reference/meisinger_denitrification.md)
  : Meisinger and Randall topsoil denitrification share matrix.
- [`drainage_ranges`](https://eduaguilera.github.io/whep/reference/drainage_ranges.md)
  : Drainage-class bins keyed on annual soil drainage.
- [`subsoil_no3_reduction`](https://eduaguilera.github.io/whep/reference/subsoil_no3_reduction.md)
  : Subsoil nitrate reduction shares by source, climate and irrigation.
- [`manner_params`](https://eduaguilera.github.io/whep/reference/manner_params.md)
  : MANNER process-based ammonia-volatilisation factors.
- [`manner_rate_factor`](https://eduaguilera.github.io/whep/reference/manner_rate_factor.md)
  : MANNER synthetic-fertiliser application-rate factor.
- [`manner_rain_factor`](https://eduaguilera.github.io/whep/reference/manner_rain_factor.md)
  : MANNER synthetic-fertiliser rainfall factor.
- [`manner_incorporation_factor`](https://eduaguilera.github.io/whep/reference/manner_incorporation_factor.md)
  : MANNER organic-manure incorporation-delay factor.
- [`manure_inorganic_n`](https://eduaguilera.github.io/whep/reference/manure_inorganic_n.md)
  : Inorganic (mineral) nitrogen fraction of excreted manure by species.
- [`manner_default_technique_mix`](https://eduaguilera.github.io/whep/reference/manner_default_technique_mix.md)
  : Gross-default technique/incorporation-delay blend for MANNER.
- [`n2o_efs_ipcc2006`](https://eduaguilera.github.io/whep/reference/n2o_efs_ipcc2006.md)
  : IPCC 2006 Tier 1 direct soil N2O emission factors by climate and
  irrigation.
- [`som_ranges`](https://eduaguilera.github.io/whep/reference/som_ranges.md)
  : Soil organic matter content bins.
- [`n_attenuation_constants`](https://eduaguilera.github.io/whep/reference/n_attenuation_constants.md)
  : Nitrogen leaching-attenuation and indirect-N2O constants.

## Urban nitrogen coefficient tables

Spain historical benchmark series (Module C, Task C3) behind
[`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md)’s
global default per-capita urban-N-to-agriculture rate: the raw
national-total urban nitrogen series and its derived per-capita rate.

- [`urban_n_reference`](https://eduaguilera.github.io/whep/reference/urban_n_reference.md)
  : Spain historical urban nitrogen applied to agriculture.
- [`urban_kgn_cap_reference`](https://eduaguilera.github.io/whep/reference/urban_kgn_cap_reference.md)
  : Spain historical per-capita urban nitrogen rate.

## Soil balances - carbon (SOC climate)

Per-model native climate rate-modifying functions for the
soil-organic-carbon turnover models. Each model carries its own
published temperature and moisture response (RothC/HSOC a*b*c, ICBM
re_clim, AMGv2 f(T)\*f(H), Century DEFAC), not a shared scalar.

- [`soc_rate_modifier_rothc()`](https://eduaguilera.github.io/whep/reference/soc_rate_modifier_rothc.md)
  : Compute the RothC and HSOC annual climate rate modifier.
- [`soc_rate_modifier_icbm()`](https://eduaguilera.github.io/whep/reference/soc_rate_modifier_icbm.md)
  : Compute the ICBM annual climate rate modifier.
- [`soc_rate_modifier_amg()`](https://eduaguilera.github.io/whep/reference/soc_rate_modifier_amg.md)
  : Compute the AMG (AMGv2) annual climate rate modifier.
- [`soc_rate_modifier_century()`](https://eduaguilera.github.io/whep/reference/soc_rate_modifier_century.md)
  : Compute the Century DEFAC annual climate rate modifier.

## Soil balances - carbon (SOC models)

Soil-organic-carbon turnover models sharing a common annual call
contract. HSOC (two pools plus inert organic matter) and RothC (five
pools) step forward in time; ICBM (two pools) and AMG (active plus
stable) are analytical; Century (five pools) integrates an ODE. Each
pulls its rate constants from `soc_turnover_params` and takes an annual
climate modifier. `calculate_soc_dynamics` is the selector that
dispatches to any of them and builds the per-model native climate
modifier from supplied climate drivers. Each model function returns its
own wide pool columns; the selector normalises all five to one long
schema (`year`, `pool`, `stock_mgc_ha`, `soc_total`, `method_soc`) so
callers never branch on the model.

- [`calculate_soc_dynamics()`](https://eduaguilera.github.io/whep/reference/calculate_soc_dynamics.md)
  : Simulate soil organic carbon dynamics with a selectable model.
- [`calculate_soc_hsoc()`](https://eduaguilera.github.io/whep/reference/calculate_soc_hsoc.md)
  : Simulate soil organic carbon with the HSOC two-pool model.
- [`calculate_soc_rothc()`](https://eduaguilera.github.io/whep/reference/calculate_soc_rothc.md)
  : Simulate soil organic carbon with the RothC five-pool model.
- [`calculate_soc_icbm()`](https://eduaguilera.github.io/whep/reference/calculate_soc_icbm.md)
  : Simulate soil organic carbon with the ICBM two-pool model.
- [`calculate_soc_amg()`](https://eduaguilera.github.io/whep/reference/calculate_soc_amg.md)
  : Simulate soil organic carbon with the AMG model.
- [`calculate_soc_century()`](https://eduaguilera.github.io/whep/reference/calculate_soc_century.md)
  : Simulate soil organic carbon with the Century five-pool model.

## Soil balances - carbon (inputs)

Assemble the soil carbon-input layer the turnover models consume: crop
residue, root and applied-manure carbon per cell, crop and year in
megagrams of carbon per hectare, plus the carbon-weighted humification
fraction from `residue_humification`. The grassland and natural-land
carbon inputs come from the LPJmL net primary production minus harvested
carbon (grassland additionally carrying grazing-excreta carbon).

- [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  : Assemble soil carbon inputs per cell, crop and year.
- [`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md)
  : Build grassland and natural-land soil carbon inputs from LPJmL.
- [`build_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_carbon_inputs.md)
  : Assemble the per-land-use-class soil carbon inputs.

## Soil balances - nitrogen

Gridded nitrogen-balance inputs (Module C). Atmospheric nitrogen
deposition from HaNi NHx and NOy, aggregated to WHEP’s 0.5-degree grid
and converted to a per-hectare rate using the true latitude-dependent
cell area. Urban/human-excreta nitrogen from gridded HYDE population and
a per-capita rate, with neighbour-cell transport buffering for cells
with no local cropland room. Ammonia-N volatilisation from the MANNER
process-based model, for synthetic fertiliser and organic manure, plus a
gross-default organic-manure variant that fills in the application
technique and incorporation delay from a documented placeholder blend
instead of requiring per-cell/per-era survey data. Gridded windspeed and
soil pH drivers, the cell-polity assembly, and the
polity-to-crop-to-grid spatialization of a country-level nitrogen total
(Task C6 infrastructure). The gridded nitrogen INPUT assembly combines
every source (BNF, residue/root recycling, manure, deposition, urban,
soil organic-matter mineralization, synthetic fertiliser) into one
long-format table (Task C6 final phase).

- [`read_n_deposition()`](https://eduaguilera.github.io/whep/reference/read_n_deposition.md)
  : Read a HaNi atmospheric nitrogen deposition species onto WHEP's
  grid.
- [`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)
  : Build gridded atmospheric nitrogen deposition inputs.
- [`read_hyde_population()`](https://eduaguilera.github.io/whep/reference/read_hyde_population.md)
  : Read gridded HYDE urban population onto WHEP's grid.
- [`read_population()`](https://eduaguilera.github.io/whep/reference/read_population.md)
  : Read national population on WHEP area codes.
- [`read_wpp_population()`](https://eduaguilera.github.io/whep/reference/read_wpp_population.md)
  : Read UN World Population Prospects population.
- [`population_source_reach()`](https://eduaguilera.github.io/whep/reference/population_source_reach.md)
  : Report which areas a present-day-ISO3 population source can reach.
- [`read_habitual_cv()`](https://eduaguilera.github.io/whep/reference/read_habitual_cv.md)
  : Read FAOSTAT's coefficient of variation of habitual caloric
  consumption.
- [`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md)
  : Build gridded urban/human-excreta nitrogen inputs to agriculture.
- [`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)
  : Estimate ammonia-N volatilisation with the MANNER process-based
  model.
- [`calculate_manner_nh3_default()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3_default.md)
  : Estimate ammonia-N volatilisation with MANNER's gross-default
  technique and incorporation-delay blend.
- [`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)
  : Estimate ammonia-N volatilisation from applied nitrogen.
- [`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md)
  : Estimate direct soil N2O emissions from applied nitrogen.
- [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)
  : Estimate nitrate leaching, topsoil denitrification and indirect N2O.
- [`calculate_indirect_n2o_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_indirect_n2o_nh3.md)
  : Estimate indirect N2O from volatilised ammonia.
- [`read_lpjml_wind()`](https://eduaguilera.github.io/whep/reference/read_lpjml_wind.md)
  : Read gridded LPJmL-forcing windspeed onto WHEP's grid.
- [`read_soil_ph()`](https://eduaguilera.github.io/whep/reference/read_soil_ph.md)
  : Read gridded soil pH onto WHEP's grid.
- [`read_soil_hydraulic()`](https://eduaguilera.github.io/whep/reference/read_soil_hydraulic.md)
  : Read gridded soil hydraulic properties from HWSD onto WHEP's grid.
- [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  : Assemble WHEP's cell-polity crosswalk with true grid-cell area.
- [`spatialize_country_n_to_crops()`](https://eduaguilera.github.io/whep/reference/spatialize_country_n_to_crops.md)
  : Spatialize a polity-level nitrogen total to crops and grid cells.
- [`build_ag_land_support()`](https://eduaguilera.github.io/whep/reference/build_ag_land_support.md)
  : Build the gridded agricultural land support.
- [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
  : Assemble gridded nitrogen inputs from every WHEP N-input source.
- [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  : Build the full nitrogen balance: inputs, outputs, losses and NUE.

## SJOS - nitrogen

Safe and Just Operating Space for nitrogen. Reader for the
Schulte-Uebbing et al. (2022) gridded critical-nitrogen layers (critical
surplus, critical input, exceedance, the medium-specific critical losses
and the binding-threshold map) that the boundary-exceedance modules
compare the WHEP nitrogen surplus and process-based losses against. The
gridded soil-surface nitrogen surplus (net inputs minus
harvested-nitrogen exports) and its surplus-mode critical-boundary
exceedance, decomposed per crop into the parts within and above the
boundary. The pathway-mode boundary instead routes each process-based
nitrogen loss to its medium-specific critical load (ammonia to air,
nitrate to the tighter of groundwater and surface water), decomposing
each medium and naming the binding boundary per crop. The nourishment
(“just”) axis supplies per-capita protein and dietary energy, from the
commodity-balance food element times the biomass nutrition coefficients
divided by population, or from FAOSTAT Food Balance Sheet per-capita
supply, and normalizes that supply onto a piecewise adequacy score
classified Under, Adequate or Over. The boundary and nourishment sides
are then crossed per crop into the 2-way safe-and-just classification,
and each crop’s exceedance, within-boundary or total surplus nitrogen
becomes the intensity of an embodied-nitrogen trade footprint extension.

- [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
  : Read a Schulte-Uebbing gridded critical-nitrogen layer.
- [`calculate_n_surplus()`](https://eduaguilera.github.io/whep/reference/calculate_n_surplus.md)
  : Calculate the gridded soil-surface nitrogen surplus.
- [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md)
  : Build source-exact gridded critical-nitrogen exceedance.
- [`build_n_pathway_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_pathway_exceedance.md)
  : Build the pathway-mode critical-nitrogen boundary exceedance.
- [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
  : Build per-capita food supply for the nourishment axis.
- [`build_protein_requirement()`](https://eduaguilera.github.io/whep/reference/build_protein_requirement.md)
  : Build the population protein requirement.
- [`build_intake_dispersion()`](https://eduaguilera.github.io/whep/reference/build_intake_dispersion.md)
  : Build the within-country intake dispersion.
- [`build_loss_wedge()`](https://eduaguilera.github.io/whep/reference/build_loss_wedge.md)
  : Build the supply-to-intake loss wedge.
- [`build_protein_quality()`](https://eduaguilera.github.io/whep/reference/build_protein_quality.md)
  : Build the protein-quality correction for the nourishment band.
- [`build_protein_score()`](https://eduaguilera.github.io/whep/reference/build_protein_score.md)
  : Score a diet's protein against the age-weighted requirement pattern.
- [`build_nourishment_band()`](https://eduaguilera.github.io/whep/reference/build_nourishment_band.md)
  : Build the SJOS-N nourishment band.
- [`normalize_nourishment()`](https://eduaguilera.github.io/whep/reference/normalize_nourishment.md)
  : Normalize and classify per-capita nourishment.
- [`calculate_food_gini()`](https://eduaguilera.github.io/whep/reference/calculate_food_gini.md)
  : Between-country population-weighted Gini of per-capita food supply.
- [`disaggregate_ussr()`](https://eduaguilera.github.io/whep/reference/disaggregate_ussr.md)
  : Split the pre-1992 aggregate USSR supply into successor states.
- [`build_n_boundary_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_percapita.md)
  : Build the per-capita nitrogen-boundary versus nourishment scatter.
- [`build_n_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_percapita.md)
  : Build country per-capita anthropogenic reactive nitrogen.
- [`classify_sjos_n()`](https://eduaguilera.github.io/whep/reference/classify_sjos_n.md)
  : Classify crops into the 2-way SJOS-N safe-and-just space.
- [`build_n_exceedance_extension()`](https://eduaguilera.github.io/whep/reference/build_n_exceedance_extension.md)
  : Build the embodied-nitrogen footprint extension.
- [`build_sjos_n_footprint()`](https://eduaguilera.github.io/whep/reference/build_sjos_n_footprint.md)
  : Build the embodied-nitrogen trade footprint.
- [`build_sjos_nitrogen()`](https://eduaguilera.github.io/whep/reference/build_sjos_nitrogen.md)
  : Assemble the end-to-end SJOS-N output tables.

## SJOS - nitrogen coefficient tables

Planetary reactive-nitrogen boundary parameters, the nourishment
protein/energy thresholds (with the waste-inequality factor and class
cutoffs), and the boundary/nourishment classification levels with their
plotting colours, feeding the SJOS-N per-capita boundary axis and the
2-way safe-and-just classification.

- [`n_boundary_params`](https://eduaguilera.github.io/whep/reference/n_boundary_params.md)
  : Planetary reactive-nitrogen boundary parameters.
- [`nourishment_thresholds`](https://eduaguilera.github.io/whep/reference/nourishment_thresholds.md)
  : Nourishment protein and energy thresholds.
- [`sjos_levels`](https://eduaguilera.github.io/whep/reference/sjos_levels.md)
  : Safe-and-just nitrogen classification levels and colours.
- [`nourish_levels`](https://eduaguilera.github.io/whep/reference/nourish_levels.md)
  : Nourishment classification levels and colours.

## Soil balances - carbon (historical balance)

Reconstruct per-cell soil-organic-carbon stock trajectories: equilibrium
initialisation under the earliest land-use carbon inputs, a forward
march on yearly per-cell per-class land-use areas with a
carbon-conserving land-use-change transfer, and the derived
soil-organic-nitrogen change via the asymmetric soil carbon-to-nitrogen
ratios.

- [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  : Build the historical gridded soil-organic-carbon balance.
- [`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)
  : Read gridded yearly LUH2 land-use-class fractions and areas.

## Crop NPP, BNF and coefficient tables

Crop net primary production (products, residues, roots, weeds),
biological nitrogen fixation, residue destinies, and the coefficient
tables that drive them. Ported from afsetools, item-keyed and
multi-method.

- [`whep_coef_table()`](https://eduaguilera.github.io/whep/reference/whep_coef_table.md)
  : Read a WHEP coefficient table.
- [`calculate_potential_npp()`](https://eduaguilera.github.io/whep/reference/calculate_potential_npp.md)
  : Estimate potential net primary production.
- [`calculate_crop_residues()`](https://eduaguilera.github.io/whep/reference/calculate_crop_residues.md)
  : Estimate crop above-ground residue biomass.
- [`calculate_crop_roots()`](https://eduaguilera.github.io/whep/reference/calculate_crop_roots.md)
  : Estimate crop below-ground (root) biomass.
- [`calculate_crop_npp()`](https://eduaguilera.github.io/whep/reference/calculate_crop_npp.md)
  : Estimate total crop net primary production.
- [`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md)
  : Partition crop and weed NPP into dry matter, carbon and nitrogen.
- [`calculate_crop_npp_components()`](https://eduaguilera.github.io/whep/reference/calculate_crop_npp_components.md)
  : Estimate cropland NPP components including weeds.
- [`calculate_crop_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_crop_bnf.md)
  : Estimate symbiotic biological nitrogen fixation by crop legumes.
- [`calculate_weed_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_weed_bnf.md)
  : Estimate symbiotic biological nitrogen fixation by weeds and cover
  crops.
- [`calculate_nonsymbiotic_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_nonsymbiotic_bnf.md)
  : Estimate non-symbiotic biological nitrogen fixation.
- [`calculate_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_bnf.md)
  : Estimate total biological nitrogen fixation.
- [`summarize_bnf()`](https://eduaguilera.github.io/whep/reference/summarize_bnf.md)
  : Summarise biological nitrogen fixation results.
- [`calculate_residue_destinies()`](https://eduaguilera.github.io/whep/reference/calculate_residue_destinies.md)
  : Estimate the destinies of crop residues.
- [`build_residue_feed_avail()`](https://eduaguilera.github.io/whep/reference/build_residue_feed_avail.md)
  : Build residue feed availability for feed allocation.

## Tidy datasets

Get cleaned dataframes with project related data.

### Production

Production, supply/use, and processing datasets.

- [`build_supply_use()`](https://eduaguilera.github.io/whep/reference/build_supply_use.md)
  : Supply and use tables
- [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md)
  : Livestock feed intake
- [`build_feed_demand()`](https://eduaguilera.github.io/whep/reference/build_feed_demand.md)
  : Build livestock feed demand.
- [`build_feed_intake_local()`](https://eduaguilera.github.io/whep/reference/build_feed_intake_local.md)
  : Build local (per-cell) feed intake, chunked by year.
- [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  : Primary items production
- [`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md)
  : Crop residue items
- [`get_processing_coefs()`](https://eduaguilera.github.io/whep/reference/get_processing_coefs.md)
  : Processed products share factors
- [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  : Commodity balance sheet data.
- [`get_livestock_cbs()`](https://eduaguilera.github.io/whep/reference/get_livestock_cbs.md)
  : Livestock commodity balance sheet entries

### Trade

Bilateral trade datasets.

- [`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md)
  : Bilateral trade data

### Nitrogen cycles

Nitrogen inputs, outputs, production, and destinies for Spain.

- [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md)
  : GRAFS Nitrogen (N) flows
- [`create_n_soil_inputs()`](https://eduaguilera.github.io/whep/reference/create_n_soil_inputs.md)
  : Nitrogen (N) soil inputs for Spain
- [`create_n_production()`](https://eduaguilera.github.io/whep/reference/create_n_production.md)
  : N production for Spain
- [`calculate_nue_crops()`](https://eduaguilera.github.io/whep/reference/calculate_nue_crops.md)
  : N soil inputs and Nitrogen Use Efficiency (NUE) for crop
- [`calculate_nue_livestock()`](https://eduaguilera.github.io/whep/reference/calculate_nue_livestock.md)
  : NUE for Livestock
- [`calculate_system_nue()`](https://eduaguilera.github.io/whep/reference/calculate_system_nue.md)
  : System NUE
- [`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md)
  : GRAFS Nitrogen (N) flows – National Spain
- [`create_grafs_plot_df()`](https://eduaguilera.github.io/whep/reference/create_grafs_plot_df.md)
  : Create GRAFS plot dataset.
- [`plot_input_output()`](https://eduaguilera.github.io/whep/reference/plot_input_output.md)
  : Plot national nitrogen inputs, production, and surplus for a land
  system.
- [`plot_input_output_livestock()`](https://eduaguilera.github.io/whep/reference/plot_input_output_livestock.md)
  : Plot national nitrogen inputs, production, and surplus for
  livestock.
- [`plot_input_output_system()`](https://eduaguilera.github.io/whep/reference/plot_input_output_system.md)
  : Plot national nitrogen inputs and uses for the full agro-food
  system.

## Download large input datasets

Fetch large files from external sources and cache them.

- [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
  : Download, cache and read files
- [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  : External inputs
- [`whep_list_file_versions()`](https://eduaguilera.github.io/whep/reference/whep_list_file_versions.md)
  : Input file versions
- [`whep_clear_cache()`](https://eduaguilera.github.io/whep/reference/whep_clear_cache.md)
  : Clear the build pipeline cache

## Code namings

Add name columns in dataframes from their codes or viceversa.

### Polities

- [`polities`](https://eduaguilera.github.io/whep/reference/polities.md)
  : Polities
- [`polity_area_crosswalk`](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
  : FAOSTAT/FABIO area-to-polity crosswalk
- [`polity_label_aliases`](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
  : Source label to polity aliases
- [`polities_cats`](https://eduaguilera.github.io/whep/reference/polities_cats.md)
  : Polity categories and regional classifications
- [`regions_full`](https://eduaguilera.github.io/whep/reference/regions_full.md)
  : Full polity and region reference table
- [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  : Add WHEP polity codes to a table
- [`polity_bucket_coverage()`](https://eduaguilera.github.io/whep/reference/polity_bucket_coverage.md)
  : Report reporting buckets that sum more than one territory
- [`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
  : Find rows attributed to a polity not live in the row's year
- [`polity_mapping_provenance()`](https://eduaguilera.github.io/whep/reference/polity_mapping_provenance.md)
  : Report which authority a row's territorial identity rests on
- [`add_area_code()`](https://eduaguilera.github.io/whep/reference/add_area_code.md)
  : Get area codes from area names
- [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md)
  : Get area names from area codes
- [`get_polity_geometries()`](https://eduaguilera.github.io/whep/reference/get_polity_geometries.md)
  : Get WHEP polity geometries
- [`folded_reporting_areas()`](https://eduaguilera.github.io/whep/reference/folded_reporting_areas.md)
  : List the reporting areas whose data is folded into another area code
- [`row_promotion_status()`](https://eduaguilera.github.io/whep/reference/row_promotion_status.md)
  : Report which Rest-of-World members report under their own territory
- [`whep_polity_columns`](https://eduaguilera.github.io/whep/reference/whep_polity_columns.md)
  : Polity columns on WHEP outputs
- [`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md)
  : Resolve a source's country label to a polity
- [`polity_identity_conventions()`](https://eduaguilera.github.io/whep/reference/polity_identity_conventions.md)
  : Which territorial identity WHEP's year-less objects carry

### Commodity balance sheet items

- [`items_cbs`](https://eduaguilera.github.io/whep/reference/items_cbs.md)
  : Commodity balance sheet items
- [`items_full`](https://eduaguilera.github.io/whep/reference/items_full.md)
  : Full CBS item table
- [`cbs_trade_codes`](https://eduaguilera.github.io/whep/reference/cbs_trade_codes.md)
  : CBS to trade item code mapping
- [`add_item_cbs_code()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_code.md)
  : Get commodity balance sheet item codes from item names
- [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md)
  : Get commodity balance sheet item names from item codes

### Primary production items

- [`items_prod`](https://eduaguilera.github.io/whep/reference/items_prod.md)
  : Primary production items
- [`items_prim`](https://eduaguilera.github.io/whep/reference/items_prim.md)
  : Primary production items linked to CBS
- [`items_prod_full`](https://eduaguilera.github.io/whep/reference/items_prod_full.md)
  : Full production item table
- [`add_item_prod_code()`](https://eduaguilera.github.io/whep/reference/add_item_prod_code.md)
  : Get production item codes from item names
- [`add_item_prod_name()`](https://eduaguilera.github.io/whep/reference/add_item_prod_name.md)
  : Get production item names from item codes

## Reference tables

Lookup and coefficient tables used internally across the pipeline.

- [`animals_codes`](https://eduaguilera.github.io/whep/reference/animals_codes.md)
  : Animal codes and classifications
- [`biomass_coefs`](https://eduaguilera.github.io/whep/reference/biomass_coefs.md)
  : Biomass coefficients for crops and livestock products
- [`cb_processing`](https://eduaguilera.github.io/whep/reference/cb_processing.md)
  : Commodity balance sheet processing fractions
- [`cft_mapping`](https://eduaguilera.github.io/whep/reference/cft_mapping.md)
  : FAOSTAT crop to LPJmL crop functional type (CFT) mapping
- [`coello_synthetic_n`](https://eduaguilera.github.io/whep/reference/coello_synthetic_n.md)
  : Coello (2025) crop-specific synthetic nitrogen application rates
- [`crops_eurostat`](https://eduaguilera.github.io/whep/reference/crops_eurostat.md)
  : Eurostat crop classification codes
- [`crops_manure_n`](https://eduaguilera.github.io/whep/reference/crops_manure_n.md)
  : Manure nitrogen application by crop and country
- [`lassaletta_grassland_share`](https://eduaguilera.github.io/whep/reference/lassaletta_grassland_share.md)
  : Grassland share of synthetic nitrogen by country and year
- [`liv_lu_coefs`](https://eduaguilera.github.io/whep/reference/liv_lu_coefs.md)
  : Livestock unit coefficients
- [`mueller_synthetic_n`](https://eduaguilera.github.io/whep/reference/mueller_synthetic_n.md)
  : Synthetic nitrogen application rates by crop and country
- [`primary_double`](https://eduaguilera.github.io/whep/reference/primary_double.md)
  : Items with double-counting in production statistics
- [`smil_2001_synthetic_n_global`](https://eduaguilera.github.io/whep/reference/smil_2001_synthetic_n_global.md)
  : Smil (2001) global synthetic nitrogen production, 1913-2000

## FAOSTAT raw data

Download FAOSTAT data as is.

- [`get_faostat_data()`](https://eduaguilera.github.io/whep/reference/get_faostat_data.md)
  : Scrape activity data from FAOSTAT and post-process it

## Data sources

Get a tidy dataframe with the found sources for different data.

- [`expand_trade_sources()`](https://eduaguilera.github.io/whep/reference/expand_trade_sources.md)
  : Trade data sources

## Nitrogen typologies Spain

Functions to generate data for N inputs, outputs, production, destinies,
typologies, etc.

- [`create_typologies_of_josette()`](https://eduaguilera.github.io/whep/reference/create_typologies_of_josette.md)
  : Typologies of Josette
- [`create_typologies_grafs_spain()`](https://eduaguilera.github.io/whep/reference/create_typologies_grafs_spain.md)
  : Typologies of Julia
- [`create_alfredos_typologies()`](https://eduaguilera.github.io/whep/reference/create_alfredos_typologies.md)
  : Alfredo's typology classification
- [`create_typologies_whep()`](https://eduaguilera.github.io/whep/reference/create_typologies_whep.md)
  : Create WHEP typologies for Spain
- [`create_grafs_plot_df()`](https://eduaguilera.github.io/whep/reference/create_grafs_plot_df.md)
  : Create GRAFS plot dataset.
- [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md)
  : GRAFS Nitrogen (N) flows
- [`create_n_soil_inputs()`](https://eduaguilera.github.io/whep/reference/create_n_soil_inputs.md)
  : Nitrogen (N) soil inputs for Spain
- [`create_n_production()`](https://eduaguilera.github.io/whep/reference/create_n_production.md)
  : N production for Spain
- [`calculate_nue_crops()`](https://eduaguilera.github.io/whep/reference/calculate_nue_crops.md)
  : N soil inputs and Nitrogen Use Efficiency (NUE) for crop
- [`calculate_nue_livestock()`](https://eduaguilera.github.io/whep/reference/calculate_nue_livestock.md)
  : NUE for Livestock
- [`calculate_system_nue()`](https://eduaguilera.github.io/whep/reference/calculate_system_nue.md)
  : System NUE
- [`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md)
  : GRAFS Nitrogen (N) flows – National Spain
- [`run_typology_sensitivity()`](https://eduaguilera.github.io/whep/reference/run_typology_sensitivity.md)
  : Run one-at-a-time sensitivity analysis on typology thresholds.
- [`plot_typology_indicators_panel()`](https://eduaguilera.github.io/whep/reference/plot_typology_indicators_panel.md)
  : Plot four N indicators as time series per typology
- [`plot_typology_periods_panel()`](https://eduaguilera.github.io/whep/reference/plot_typology_periods_panel.md)
  : Plot four N indicators as period comparisons per typology

## Data-frame utilities

Declare a table’s schema once, as serializable data, then either reach
it or prove it.
[`ensure_columns()`](https://eduaguilera.github.io/whep/reference/ensure_columns.md)
coerces a table to a typed prototype;
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
reports every violation of a declarative schema without touching the
table, and
[`assert_table_schema()`](https://eduaguilera.github.io/whep/reference/assert_table_schema.md)
is the build-time gate over the same schema.

- [`ensure_columns()`](https://eduaguilera.github.io/whep/reference/ensure_columns.md)
  : Complete columns from a typed prototype.
- [`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
  : Check a table against a declarative schema.
- [`assert_table_schema()`](https://eduaguilera.github.io/whep/reference/assert_table_schema.md)
  : Assert that a table conforms to a declarative schema.

## Gap filling functions

Functions to fill gaps (NA values) in time-dependent variables using
different methods.
[`interp_vec()`](https://eduaguilera.github.io/whep/reference/interp_vec.md)
is the vector-level interpolation primitive behind
`fill_linear(log_space = TRUE)`, for callers working with plain vectors.

- [`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md)
  : Fill gaps by linear interpolation, or carrying forward or backward.
- [`fill_sum()`](https://eduaguilera.github.io/whep/reference/fill_sum.md)
  : Fill gaps summing the previous value of a variable to the value of
  another variable.
- [`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)
  : Fill gaps using growth rates from proxy variables
- [`interp_vec()`](https://eduaguilera.github.io/whep/reference/interp_vec.md)
  : Interpolate anchor points at arbitrary output positions.

## Multi-source consolidation

Reduce a multi-source panel to one winning row per cell by source
priority, with measure-aware demotion, coverage and quality tie-breaks,
and an isolated-flip continuity override.

- [`consolidate_sources()`](https://eduaguilera.github.io/whep/reference/consolidate_sources.md)
  : Consolidate a multi-source panel to one winning row per cell.

## Decomposition analysis

Functions for index decomposition analysis, and LMDI decomposition of
territorial nitrogen surplus and losses (cropland, semi-natural,
livestock manure, urban), specialization and crop-livestock connectivity
covariance, and destiny-mix shifts.

- [`calculate_lmdi()`](https://eduaguilera.github.io/whep/reference/calculate_lmdi.md)
  : Calculate LMDI decomposition.
- [`decompose_weighted_ratio()`](https://eduaguilera.github.io/whep/reference/decompose_weighted_ratio.md)
  : Decompose a weighted aggregate ratio.
- [`decompose_cropland_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_cropland_surplus.md)
  : Decompose cropland N surplus into size, intensity, and inefficiency
  drivers
- [`decompose_semi_natural_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_semi_natural_surplus.md)
  : Decompose semi-natural agroecosystem N surplus into size, intensity,
  and inefficiency drivers
- [`decompose_manure_losses()`](https://eduaguilera.github.io/whep/reference/decompose_manure_losses.md)
  : Decompose livestock manure management losses into herd, feed,
  excretion, and management-loss drivers
- [`decompose_urban_losses()`](https://eduaguilera.github.io/whep/reference/decompose_urban_losses.md)
  : Decompose urban nitrogen losses into population, per-capita, and
  recycling drivers
- [`decompose_terr_losses()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses.md)
  : Decompose total territorial N losses into compartments and
  mechanisms
- [`decompose_terr_losses_periods()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses_periods.md)
  : Decompose territorial N losses by reference period (chained)
- [`decompose_specialization_cov()`](https://eduaguilera.github.io/whep/reference/decompose_specialization_cov.md)
  : Decompose specialization from diversification via the Olley-Pakes
  allocation covariance
- [`decompose_crop_livestock_conn()`](https://eduaguilera.github.io/whep/reference/decompose_crop_livestock_conn.md)
  : Compute the crop-livestock connectivity index per province
- [`decompose_destiny_mix()`](https://eduaguilera.github.io/whep/reference/decompose_destiny_mix.md)
  : Compute the national cropland destiny mix over time
- [`plot_loss_decomp()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp.md)
  : Plot cumulative drivers of the change in territorial N losses
- [`plot_loss_decomp_periods()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_periods.md)
  : Plot period-based drivers of the change in territorial N losses
- [`plot_loss_decomp_periods_panel()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_periods_panel.md)
  : Plot period-based drivers of territorial N losses, as one combined
  panel plot
- [`plot_loss_decomp_rolling()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_rolling.md)
  : Plot rolling-mean year-on-year drivers of the change in territorial
  N losses
- [`plot_loss_decomp_rolling_panel()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_rolling_panel.md)
  : Plot rolling-mean year-on-year drivers of territorial N losses, as
  one combined panel plot
- [`plot_loss_decomp_yearly()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_yearly.md)
  : Plot year-on-year (non-cumulative) drivers of the change in
  territorial N losses
- [`plot_compart_factor()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor.md)
  : Plot each compartment's own factor breakdown
- [`plot_compart_factor_periods()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor_periods.md)
  : Plot each compartment's own factor breakdown by period, as one
  combined panel plot
- [`plot_compart_factor_roll()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor_roll.md)
  : Plot each compartment's own factor breakdown, rolling mean
- [`plot_compart_factor_roll_panel()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor_roll_panel.md)
  : Plot each compartment's own factor breakdown, rolling mean, as one
  combined panel plot
- [`plot_compart_factor_yearly()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor_yearly.md)
  : Plot each compartment's own factor breakdown, year-on-year
  (non-cumulative)
- [`plot_specialization_cov()`](https://eduaguilera.github.io/whep/reference/plot_specialization_cov.md)
  : Plot the specialization-vs-diversification allocation covariance
- [`plot_crop_livestock_conn()`](https://eduaguilera.github.io/whep/reference/plot_crop_livestock_conn.md)
  : Plot the crop-livestock connectivity index
- [`plot_destiny_mix()`](https://eduaguilera.github.io/whep/reference/plot_destiny_mix.md)
  : Plot the cropland destiny mix over time

## Circularity index

Finn’s cycling index for the nitrogen flow network.

- [`create_finn_indicator()`](https://eduaguilera.github.io/whep/reference/create_finn_indicator.md)
  : Calculate Finn Cycling Index for each province and year
- [`plot_finn_circularity()`](https://eduaguilera.github.io/whep/reference/plot_finn_circularity.md)
  : Plot Finn Cycling Index evolution and period comparison

## National trade validation

Compare provincial-model trade flows against raw FAOSTAT bilateral trade
totals.

- [`compute_trade_flows_raw()`](https://eduaguilera.github.io/whep/reference/compute_trade_flows_raw.md)
  : Compute national trade flows: model vs. raw historical FAO series
- [`validate_national_trade()`](https://eduaguilera.github.io/whep/reference/validate_national_trade.md)
  : Validate national net trade balance
- [`validate_national_trade_raw()`](https://eduaguilera.github.io/whep/reference/validate_national_trade_raw.md)
  : Validate national net trade against raw historical FAO series
- [`plot_national_trade_flows_raw()`](https://eduaguilera.github.io/whep/reference/plot_national_trade_flows_raw.md)
  : Plot national trade flows: model vs. raw historical FAO series
- [`plot_national_trade_validation()`](https://eduaguilera.github.io/whep/reference/plot_national_trade_validation.md)
  : Plot national net trade validation

## Multi-regional input-output model

Build and analyze multi-regional input-output (MRIO) models following
the FABIO framework.

- [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
  : Build multi-regional input-output model.
- [`compute_leontief_inverse()`](https://eduaguilera.github.io/whep/reference/compute_leontief_inverse.md)
  : Compute Leontief inverse.
- [`balance_ras()`](https://eduaguilera.github.io/whep/reference/balance_ras.md)
  : Balance a matrix to target margins by RAS.
- [`balance_io_flows()`](https://eduaguilera.github.io/whep/reference/balance_io_flows.md)
  : Balance input-output flows so the footprint conserves.
- [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
  : Compute environmental footprints.
- [`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md)
  : Compute a footprint end-to-end from an extension table.
- [`align_extension()`](https://eduaguilera.github.io/whep/reference/align_extension.md)
  : Align an extension table to input-output sector labels.
- [`compute_footprint_paths()`](https://eduaguilera.github.io/whep/reference/compute_footprint_paths.md)
  : Compute first-use footprint paths.
- [`compute_fp_product_paths()`](https://eduaguilera.github.io/whep/reference/compute_fp_product_paths.md)
  : Compute final-product footprint paths.
- [`add_footprint_product_stage()`](https://eduaguilera.github.io/whep/reference/add_footprint_product_stage.md)
  : Add a final-demand product-area stage to footprints.
- [`build_crop_land_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_land_extension.md)
  : Build per-crop physical cropland extension.
- [`get_crop_land_extension()`](https://eduaguilera.github.io/whep/reference/get_crop_land_extension.md)
  : Get the per-crop physical cropland extension from spatialization
  inputs.
- [`get_arable_permanent_land()`](https://eduaguilera.github.io/whep/reference/get_arable_permanent_land.md)
  : Physical arable and permanent-crop land base (fallow-inclusive).
- [`build_fao_arable_fallow_extension()`](https://eduaguilera.github.io/whep/reference/build_fao_arable_fallow_extension.md)
  : Build a per-crop physical land extension with FAO fallow-inclusive
  arable land.
- [`build_cropgrids_land_extension()`](https://eduaguilera.github.io/whep/reference/build_cropgrids_land_extension.md)
  : Build a per-crop physical land extension from CROPGRIDS.
- [`build_hayr_land_extension()`](https://eduaguilera.github.io/whep/reference/build_hayr_land_extension.md)
  : Build a hectare-year (land-occupation) crop land extension.
- [`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md)
  : Build the native grassland land extension.
- [`build_livestock_ghg_extension()`](https://eduaguilera.github.io/whep/reference/build_livestock_ghg_extension.md)
  : Build the livestock greenhouse-gas emissions extension.
- [`build_energy_co2_extension()`](https://eduaguilera.github.io/whep/reference/build_energy_co2_extension.md)
  : Build the livestock energy-use CO2 footprint extension (meat only).
- [`build_crop_soil_n2o_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_soil_n2o_extension.md)
  : Build the crop/soil N2O extension.
- [`gridded_fallow_weights()`](https://eduaguilera.github.io/whep/reference/gridded_fallow_weights.md)
  : Build agro-climatic, rainfed-gated fallow allocation weights.
- [`attribute_fallow_to_crops()`](https://eduaguilera.github.io/whep/reference/attribute_fallow_to_crops.md)
  : Attribute reported fallow land to crops.
- [`plot_footprint_sankey()`](https://eduaguilera.github.io/whep/reference/plot_footprint_sankey.md)
  : Interactive footprint Sankey viewer
- [`compute_footprint_balance()`](https://eduaguilera.github.io/whep/reference/compute_footprint_balance.md)
  : Compute land footprints by physical trade balance.
- [`build_land_balance_footprint()`](https://eduaguilera.github.io/whep/reference/build_land_balance_footprint.md)
  : Build a consumption land footprint by physical trade balance.
- [`melt_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/melt_bilateral_trade.md)
  : Melt a bilateral trade matrix to long format.
- [`compare_footprint_methods()`](https://eduaguilera.github.io/whep/reference/compare_footprint_methods.md)
  : Compare two footprint estimates.
- [`allocate_grazing_to_products()`](https://eduaguilera.github.io/whep/reference/allocate_grazing_to_products.md)
  : Allocate grazing land forward to livestock products.
- [`build_grazing_feed_footprint()`](https://eduaguilera.github.io/whep/reference/build_grazing_feed_footprint.md)
  : Build a grazing-land footprint by forward feed-allocation.

## Trustworthiness and quality control

Checks that make footprint, balance and time-series results trustworthy:
conservation invariants, accounting identities, within-series jump
detection (the level-2 detector of the AFE data-validation framework),
and other data-quality diagnostics.

- [`check_footprint_conservation()`](https://eduaguilera.github.io/whep/reference/check_footprint_conservation.md)
  : Check footprint conservation against direct extensions.
- [`summarise_conservation()`](https://eduaguilera.github.io/whep/reference/summarise_conservation.md)
  : Summarise a footprint conservation report.
- [`assert_footprint_invariants()`](https://eduaguilera.github.io/whep/reference/assert_footprint_invariants.md)
  : Assert that footprint conservation invariants hold.
- [`check_supply_use_balance()`](https://eduaguilera.github.io/whep/reference/check_supply_use_balance.md)
  : Check the commodity balance sheet supply-use identity.
- [`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md)
  : Flag implausible year-on-year jumps in a time series.
- [`check_parquet_integrity()`](https://eduaguilera.github.io/whep/reference/check_parquet_integrity.md)
  : Check the structural integrity of a Parquet file.
- [`assert_parquet_integrity()`](https://eduaguilera.github.io/whep/reference/assert_parquet_integrity.md)
  : Assert that a Parquet file is structurally sound.
- [`write_parquet_checked()`](https://eduaguilera.github.io/whep/reference/write_parquet_checked.md)
  : Write a Parquet file and verify it before returning.
- [`write_table_checked()`](https://eduaguilera.github.io/whep/reference/write_table_checked.md)
  : Write a table to disk safely and verifiably.

### Uncertainty and sensitivity

Propagate input uncertainty through a footprint, combine data-quality
coefficients of variation, and rank sectors by local sensitivity.

- [`propagate_fp_uncertainty()`](https://eduaguilera.github.io/whep/reference/propagate_fp_uncertainty.md)
  : Propagate input uncertainty through a footprint.
- [`combine_cov()`](https://eduaguilera.github.io/whep/reference/combine_cov.md)
  : Combine independent coefficient-of-variation components.
- [`footprint_sensitivity()`](https://eduaguilera.github.io/whep/reference/footprint_sensitivity.md)
  : Local sensitivity of a footprint to each extension.

### Provenance and reproducibility

Record and carry the code and input versions behind a result so any
number can be traced back to what produced it.

- [`record_provenance()`](https://eduaguilera.github.io/whep/reference/record_provenance.md)
  : Record provenance for a reproducible result.
- [`attach_provenance()`](https://eduaguilera.github.io/whep/reference/attach_provenance.md)
  : Attach a provenance record to a result.
- [`get_provenance()`](https://eduaguilera.github.io/whep/reference/get_provenance.md)
  : Retrieve a result's provenance record.

### Scope and transparency

Attach a machine-readable goal-and-scope record (method, boundary,
allocation, vintage, limitations) to a footprint result.

- [`footprint_scope()`](https://eduaguilera.github.io/whep/reference/footprint_scope.md)
  : Describe the scope of a footprint result.
- [`attach_scope()`](https://eduaguilera.github.io/whep/reference/attach_scope.md)
  : Attach a scope record to a result.
- [`get_scope()`](https://eduaguilera.github.io/whep/reference/get_scope.md)
  : Retrieve a result's scope record.

## Livestock emissions

Calculate livestock GHG emissions using IPCC 2019 Tier 1 and Tier 2
methods for enteric methane, manure methane, and manure nitrous oxide.

- [`prepare_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/prepare_livestock_emissions.md)
  : Prepare production data for livestock emission calculations.
- [`calculate_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/calculate_livestock_emissions.md)
  : Calculate all livestock emissions.
- [`calculate_enteric_ch4()`](https://eduaguilera.github.io/whep/reference/calculate_enteric_ch4.md)
  : Calculate enteric methane emissions.
- [`calculate_manure_emissions()`](https://eduaguilera.github.io/whep/reference/calculate_manure_emissions.md)
  : Calculate manure emissions (CH4 + N2O).
- [`estimate_n_excretion()`](https://eduaguilera.github.io/whep/reference/estimate_n_excretion.md)
  : Estimate livestock nitrogen, carbon and volatile-solids excretion.
- [`split_manure_management()`](https://eduaguilera.github.io/whep/reference/split_manure_management.md)
  : Split livestock excretion across manure-management systems.
- [`apply_management_losses()`](https://eduaguilera.github.io/whep/reference/apply_management_losses.md)
  : Apply IPCC manure-management losses to the collected manure streams.
- [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)
  : Build livestock nutrient flows from realised feed intake.
- [`allocate_manure_to_land()`](https://eduaguilera.github.io/whep/reference/allocate_manure_to_land.md)
  : Allocate field-available manure to cropland and grassland by crop.
- [`allocate_manure_transport()`](https://eduaguilera.github.io/whep/reference/allocate_manure_transport.md)
  : Spill surplus manure to neighbouring cells with spare capacity.
- [`estimate_energy_demand()`](https://eduaguilera.github.io/whep/reference/estimate_energy_demand.md)
  : Estimate energy demand (Gross Energy) - Tier 2
- [`calculate_cohorts_systems()`](https://eduaguilera.github.io/whep/reference/calculate_cohorts_systems.md)
  : Calculate cohort and production system distribution.
- [`calculate_uncertainty_bounds()`](https://eduaguilera.github.io/whep/reference/calculate_uncertainty_bounds.md)
  : Calculate uncertainty bounds for livestock emissions.

## Feed intake

Estimate livestock feed demand and allocate it against feed
availability.

- [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  : Redistribute available feed supply among livestock demand.

### Feed coefficient tables

Bouwman and Krausmann feed coefficients, feed taxonomy and diet share
caps.

- [`conv_bouwman`](https://eduaguilera.github.io/whep/reference/conv_bouwman.md)
  : Bouwman feed conversion ratios.
- [`conv_krausmann`](https://eduaguilera.github.io/whep/reference/conv_krausmann.md)
  : Krausmann per head feed intake.
- [`feed_taxonomy`](https://eduaguilera.github.io/whep/reference/feed_taxonomy.md)
  : Feed taxonomy.
- [`max_intake_share`](https://eduaguilera.github.io/whep/reference/max_intake_share.md)
  : Maximum intake shares.

## Livestock coefficient tables

IPCC and GLEAM coefficient tables for livestock emission calculations.

### GLEAM supplement tables

Data from MacLeod et al. (2018) GLEAM 3.0 Supplement S1.

- [`gleam_animal_weights`](https://eduaguilera.github.io/whep/reference/gleam_animal_weights.md)
  : GLEAM animal weights.
- [`gleam_crop_residue_params`](https://eduaguilera.github.io/whep/reference/gleam_crop_residue_params.md)
  : GLEAM crop residue parameters.
- [`gleam_dressing_percentages`](https://eduaguilera.github.io/whep/reference/gleam_dressing_percentages.md)
  : GLEAM dressing percentages.
- [`gleam_enteric_params`](https://eduaguilera.github.io/whep/reference/gleam_enteric_params.md)
  : GLEAM enteric fermentation parameters.
- [`gleam_feed_categories`](https://eduaguilera.github.io/whep/reference/gleam_feed_categories.md)
  : GLEAM feed categories.
- [`gleam_feed_composition`](https://eduaguilera.github.io/whep/reference/gleam_feed_composition.md)
  : GLEAM feed use efficiency.
- [`gleam_feed_conversion_ratios`](https://eduaguilera.github.io/whep/reference/gleam_feed_conversion_ratios.md)
  : GLEAM feed conversion ratios for monogastrics.
- [`gleam_feed_digestibility`](https://eduaguilera.github.io/whep/reference/gleam_feed_digestibility.md)
  : GLEAM feed digestibility for ruminants.
- [`gleam_geographic_hierarchy`](https://eduaguilera.github.io/whep/reference/gleam_geographic_hierarchy.md)
  : GLEAM geographic hierarchy.
- [`gleam_livestock_categories`](https://eduaguilera.github.io/whep/reference/gleam_livestock_categories.md)
  : GLEAM livestock categories.
- [`gleam_milk_production`](https://eduaguilera.github.io/whep/reference/gleam_milk_production.md)
  : GLEAM milk production.
- [`gleam_mms_shares`](https://eduaguilera.github.io/whep/reference/gleam_mms_shares.md)
  : GLEAM manure management system shares.
- [`gleam_field_operation_ef`](https://eduaguilera.github.io/whep/reference/gleam_field_operation_ef.md)
  : Emission factors for field operations on feed materials.
- [`gleam_mechanization_levels`](https://eduaguilera.github.io/whep/reference/gleam_mechanization_levels.md)
  : Country-level mechanization levels for feed materials.
- [`gleam_processing_transport_ef`](https://eduaguilera.github.io/whep/reference/gleam_processing_transport_ef.md)
  : Processing and transport emission factors for feeds.
- [`gleam_crop_residue_nitrogen`](https://eduaguilera.github.io/whep/reference/gleam_crop_residue_nitrogen.md)
  : Nitrogen parameters for crop residues of feed materials.
- [`gleam_fracremove`](https://eduaguilera.github.io/whep/reference/gleam_fracremove.md)
  : Country-level fraction of crop residues removed.
- [`gleam_energy_use_ef`](https://eduaguilera.github.io/whep/reference/gleam_energy_use_ef.md)
  : Energy use emission factors for livestock production.

### IPCC 2019 Refinement tables

Emission factors and parameters from IPCC 2019, Vol 4, Ch 10.

- [`ipcc_2019_enteric_ef_cattle`](https://eduaguilera.github.io/whep/reference/ipcc_2019_enteric_ef_cattle.md)
  : IPCC 2019 enteric EF for cattle.
- [`ipcc_2019_enteric_ef_other`](https://eduaguilera.github.io/whep/reference/ipcc_2019_enteric_ef_other.md)
  : IPCC 2019 enteric EF for non-cattle.
- [`ipcc_2019_manure_ch4_ef_cattle`](https://eduaguilera.github.io/whep/reference/ipcc_2019_manure_ch4_ef_cattle.md)
  : IPCC 2019 manure CH4 EF for cattle.
- [`ipcc_2019_manure_ch4_ef_other`](https://eduaguilera.github.io/whep/reference/ipcc_2019_manure_ch4_ef_other.md)
  : IPCC 2019 manure CH4 EF for non-cattle.
- [`ipcc_2019_mcf_manure`](https://eduaguilera.github.io/whep/reference/ipcc_2019_mcf_manure.md)
  : IPCC 2019 MCF for manure management.
- [`ipcc_2019_n_excretion`](https://eduaguilera.github.io/whep/reference/ipcc_2019_n_excretion.md)
  : IPCC 2019 nitrogen excretion rates.
- [`ipcc_2019_n2o_ef_direct`](https://eduaguilera.github.io/whep/reference/ipcc_2019_n2o_ef_direct.md)
  : IPCC 2019 direct N2O emission factors.
- [`ipcc_2019_ym`](https://eduaguilera.github.io/whep/reference/ipcc_2019_ym.md)
  : IPCC Ym values.
- [`ipcc_2019_bo`](https://eduaguilera.github.io/whep/reference/ipcc_2019_bo.md)
  : IPCC 2019 Bo values (Table 10.16A).
- [`ipcc_2019_cfi`](https://eduaguilera.github.io/whep/reference/ipcc_2019_cfi.md)
  : IPCC 2019 Cfi values (Table 10.4).

### IPCC 2006 tables

Emission factors from IPCC 2006 Guidelines, Vol 4, Ch 10.

- [`ipcc_2006_enteric_ef`](https://eduaguilera.github.io/whep/reference/ipcc_2006_enteric_ef.md)
  : IPCC 2006 Tier 1 enteric emission factors.
- [`ipcc_2006_manure_ef`](https://eduaguilera.github.io/whep/reference/ipcc_2006_manure_ef.md)
  : IPCC 2006 Tier 1 manure emission factors.
- [`ipcc_2006_mcf_temp`](https://eduaguilera.github.io/whep/reference/ipcc_2006_mcf_temp.md)
  : IPCC 2006 MCF by temperature.

### Tier 2 parameters

Detailed parameters for IPCC Tier 2 calculations.

- [`ipcc_tier2_energy_coefs`](https://eduaguilera.github.io/whep/reference/ipcc_tier2_energy_coefs.md)
  : Tier 2 energy coefficients.
- [`ipcc_tier2_ym_values`](https://eduaguilera.github.io/whep/reference/ipcc_tier2_ym_values.md)
  : Tier 2 Ym values.
- [`ipcc_tier2_bo_values`](https://eduaguilera.github.io/whep/reference/ipcc_tier2_bo_values.md)
  : Tier 2 Bo values.
- [`ipcc_tier2_manure_ash`](https://eduaguilera.github.io/whep/reference/ipcc_tier2_manure_ash.md)
  : Tier 2 manure ash content.
- [`ipcc_tier2_n_retention`](https://eduaguilera.github.io/whep/reference/ipcc_tier2_n_retention.md)
  : Tier 2 nitrogen retention fractions.
- [`livestock_production_defaults`](https://eduaguilera.github.io/whep/reference/livestock_production_defaults.md)
  : Default production parameters.
- [`feed_characteristics`](https://eduaguilera.github.io/whep/reference/feed_characteristics.md)
  : Feed characteristics by diet quality.
- [`livestock_constants`](https://eduaguilera.github.io/whep/reference/livestock_constants.md)
  : Livestock physical constants.

### Other livestock tables

Climate, manure management, and uncertainty tables.

- [`climate_mcf`](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
  : Climate-zone MCF values.
- [`regional_mms_distribution`](https://eduaguilera.github.io/whep/reference/regional_mms_distribution.md)
  : Regional MMS distribution.
- [`temperature_adjustment`](https://eduaguilera.github.io/whep/reference/temperature_adjustment.md)
  : Temperature adjustment factors for NEm.
- [`grazing_energy_coefs`](https://eduaguilera.github.io/whep/reference/grazing_energy_coefs.md)
  : Grazing energy coefficients.
- [`indirect_n2o_ef`](https://eduaguilera.github.io/whep/reference/indirect_n2o_ef.md)
  : Indirect N2O emission factors.
- [`uncertainty_ranges`](https://eduaguilera.github.io/whep/reference/uncertainty_ranges.md)
  : Uncertainty ranges for emission parameters.

## Harmonization

Functions to harmonize time series items according to specified mapping.

- [`harmonize_simple()`](https://eduaguilera.github.io/whep/reference/harmonize_simple.md)
  : Harmonize rows labeled "simple" by summing values
- [`harmonize_interpolate()`](https://eduaguilera.github.io/whep/reference/harmonize_interpolate.md)
  : Harmonize advanced cases with interpolation for 1:N groups
