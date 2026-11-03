# Estimate nitrate leaching, topsoil denitrification and indirect N2O.

Four methods for partitioning a nitrogen surplus into leached nitrate,
denitrified nitrogen and nitrate retained in the soil profile.

`"meisinger_drainage"` (the default) is a transport-limited Meisinger
cascade. Each year the surplus `S` and the nitrate the profile retained
the year before, `C_in` (`retained_carried_in_n_t`), pass through it
together: the nitrate available for transport is
`A = (S + C_in) (1 - d) (1 - a_cn)`, with `d` the topsoil
denitrification share looked up from
[meisinger_denitrification](https://eduaguilera.github.io/whep/reference/meisinger_denitrification.md)
by drainage class, soil organic matter class, fertiliser class and
climate, and `a_cn` the carbon-to-nitrogen leaching attenuation. A
transport factor `f` of the annual drainage `D` sets how much of `A`
leaves the root zone, and the subsoil reduction `red`
([subsoil_no3_reduction](https://eduaguilera.github.io/whep/reference/subsoil_no3_reduction.md))
acts on what leaves: `no3_n_t = A f (1 - red)`,
`retained_profile_n_t = A (1 - f)`, and
`denitrification_n_t = S + C_in - no3_n_t - retained_profile_n_t` (all
the gaseous and subsoil sinks). The retained nitrate is next year's
`C_in` on the same row key, so every row closes
`n_surplus_t + retained_carried_in_n_t = no3_n_t + denitrification_n_t + retained_profile_n_t`.

`"ipcc_fracleach"` is the IPCC 2019 Tier 1 leached fraction, keyed on
`moisture_regime` and `water_regime`, with every factor read from
[ipcc_2019_soil_n_ef](https://eduaguilera.github.io/whep/reference/ipcc_2019_soil_n_ef.md):
`FracLEACH-(H)` is 0.24 on wet rows and on dry irrigated rows and 0 on
dry rainfed rows (IPCC 2019 Refinement Vol 4 Ch 11 Section 11.2.2.2 p.
11.25: 0.24 "for wet climates or in dry climate regions where irrigation
(other than drip irrigation) is used"), and `EF5` is 0.011 (Table 11.3
(Updated) p. 11.26). All irrigation is treated as non-drip, since no
layer says which is drip: an upper-bound assumption for dry irrigated
land. On wet and dry irrigated rows the rest of the surplus is booked as
`denitrification_n_t`. On dry rainfed rows, where FracLEACH is 0, it is
booked as `retained_profile_n_t`, nitrogen left in the soil profile, and
`denitrification_n_t` is 0: FracLEACH says that surplus does not leach,
not that it denitrifies, and no method here estimates its fate. That
retained term is a flux of the year, not carried into the next one. Each
row is stamped `"ipcc_fracleach_wet"`, `"ipcc_fracleach_dry_irrigated"`
or `"ipcc_fracleach_dry_rainfed_retained"`. Eq. 11.10 and Table 11.3
define `FracLEACH-(H)` per kg of N added
(`F_SN + F_ON + F_PRP + F_CR + F_SOM`), so it multiplies
`n_input_for_n2o_t`, the same input set that prices direct N2O in
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md),
not the surplus. `"fracleach_surplus"` applies the same fractions to
`n_surplus_t` instead, the form before whep#1535: a surplus-based
deviation from the IPCC definition, kept selectable for comparison,
stamped `"fracleach_surplus_wet"` and so on. Under both,
`no3_n_t + denitrification_n_t + retained_profile_n_t` equals
`n_surplus_t`.

`"image_gnm"` is the root-zone leaching and denitrification of IMAGE-GNM
(Beusen et al. 2015, Geosci. Model Dev. 8:4045-4067,
doi:10.5194/gmd-8-4045-2015, eqs. 8-14, pp. 4051-4052, and Table 2, p.
4053). Surface runoff first carries off `surface_runoff_n_t`, a share of
the year's nitrogen input (eq. 9). The budget left leaches in the share
`f_leach`, `no3_n_t = f_leach (n_surplus_t - surface_runoff_n_t)` (eq.
10), and the rest of it denitrifies (`f_den = 1 - f_leach`), with
`f_leach = [1 - min(f_climate + f_text + f_drain + f_soc, 1)] f_landuse`
(eq. 11). The water that sets the root-zone residence time is the part
of the total runoff `runoff_mm` that surface runoff does not carry off
(eq. 5); `drainage_mm` is not read. On grassland and natural land with a
total runoff below 3 mm/yr no water moves the surplus, which is held as
inactive N in `retained_profile_n_t`. Eroded soil carries `erosion_n_t`
from the soil's own organic nitrogen (eq. 8), which no budget pays for.
See the IMAGE-GNM section under Details. IMAGE-GNM carries no nitrate
from year to year, so every row with water closes
`max(n_surplus_t, surface_runoff_n_t) = surface_runoff_n_t + no3_n_t + denitrification_n_t`,
and every row without it `retained_profile_n_t = max(n_surplus_t, 0)`.

## Usage

``` r
calculate_n_leaching(
  x,
  drainage_mm = NULL,
  method = c("meisinger_drainage", "ipcc_fracleach", "fracleach_surplus", "image_gnm"),
  transport = c("swat_mixing", "swat_uniform", "burns", "unlimited"),
  none_class = c("retention", "waterlogged"),
  carry_over_by = NULL,
  initial_pool = c("steady_state", "zero"),
  example = FALSE
)
```

## Source

IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Vol 4, Ch 11, Section 11.2.2.2, p. 11.25
(`FracLEACH-(H)`) and Table 11.3 (Updated), p. 11.26 (`EF5`). Beusen, A.
H. W., Van Beek, L. P. H., Bouwman, A. F. et al. (2015). Coupling global
models for hydrology and nutrient loading to simulate nitrogen and
phosphorus retention in surface water - description of IMAGE-GNM and
analysis of performance. Geosci. Model Dev. 8, 4045-4067.
[doi:10.5194/gmd-8-4045-2015](https://doi.org/10.5194/gmd-8-4045-2015) .
The transport sources are cited under Details.

## Arguments

- x:

  A tibble with `n_surplus_t`. `method = "meisinger_drainage"` requires
  `fert_type`, `climate`, `irrig_cat`, `land_use`, `cn_input` (may be
  `NA`), `tillage` (checked only for synthetic rows), `som_share` and
  the soil columns its `transport` reads (see Details), plus `year` with
  `carry_over_by`; `method = "ipcc_fracleach"` requires
  `moisture_regime` (`"wet"` or `"dry"`), on dry rows `water_regime`
  (`"rainfed"` or `"irrigated"`), and `n_input_for_n2o_t`, the N added
  in IPCC Eq. 11.10; `"fracleach_surplus"` requires the first two;
  `method = "image_gnm"` requires `n_input_std_t`, `nh3_n_t`, `area_ha`,
  `runoff_mm` (the year's total runoff, mm), `surface_runoff_fraction`,
  `slope_m_per_km` (both from
  [`read_image_gnm_runoff()`](https://eduaguilera.github.io/whep/reference/read_image_gnm_runoff.md)),
  `land_use` (`"Cropland"`, `"Grassland"` or `"Natural"`),
  `temp_c_annual_mean`, `soil_field_capacity`, `soil_wilting_point`,
  `soil_texture_class`, `soil_drainage_class` and
  `soil_organic_carbon_pct`, and no `surface_runoff_n_t` (see Details).

- drainage_mm:

  A numeric vector aligned to `x`'s rows giving annual drainage (mm), or
  a single string naming a column of `x` to use instead. Kept as a
  separate argument (rather than a static `x` column) because in the
  full pipeline it flows in from Module A's gridded water balance. Read
  by `"meisinger_drainage"` only.

- method:

  `"meisinger_drainage"` (default), `"ipcc_fracleach"`,
  `"fracleach_surplus"` or `"image_gnm"`.

- transport:

  The transport factor of `"meisinger_drainage"`: `"swat_mixing"`
  (default), `"swat_uniform"`, `"burns"` or `"unlimited"`.

- none_class:

  How `"meisinger_drainage"` prices Meisinger's `None` drainage class:
  `"retention"` (default) or `"waterlogged"`.

- carry_over_by:

  Character vector of the columns that key a row across years (every key
  column but `year`), or `NULL` (default) to treat each row as a single
  year.

- initial_pool:

  The nitrate a key's first year of `"meisinger_drainage"` starts with:
  `"steady_state"` (default), the pool of that year's fluxes repeated,
  or `"zero"`.

- example:

  If `TRUE`, return a small fixture instead of computing from `x`.
  Defaults to `FALSE`.

## Value

`x` with `no3_n_t`, `denitrification_n_t`, `retained_profile_n_t`,
`n2o_indirect_no3_n_t` and `method_leaching` appended, plus
`retained_carried_in_n_t` under `"meisinger_drainage"` and
`"image_gnm"`, and `surface_runoff_n_t` and `erosion_n_t` under
`"image_gnm"`.

## Details

### Transport factor

`transport` selects `f(D)`, with `D` the annual drainage (mm), `h` the
root-zone depth `root_depth_cm`, `phi` the porosity `soil_porosity` and
`theta_fc` the volumetric field capacity `soil_field_capacity`:

- `"swat_mixing"` (default): the SWAT well-mixed layer,
  `f = 1 - exp(-D / ((1 - theta_e) phi h))` with `h` in mm and the anion
  exclusion fraction `theta_e = 0.5` (Neitsch et al. 2011, SWAT 2009
  Theoretical Documentation, TR-406, eqs. 4:2.1.2 to 4:2.1.8, pp.
  269-270; `theta_e` default from the SWAT Input/Output File
  Documentation, Version 2012, ch. 22, p. 304). The source equation is
  daily; applied to nitrate present before the year drains, with no
  other exit, the daily shares compound exactly to this form on the
  year's drainage, so the annual form assumes the surplus is in the root
  zone before the drainage season and that lateral flow is negligible.

- `"swat_uniform"`: the same daily equation for a surplus that arrives
  in proportion to the drainage through the year, integrated exactly:
  `f_new = 1 - (1 - exp(-D / M)) / (D / M)`, `M = (1 - theta_e) phi h`,
  for the surplus, and the `"swat_mixing"` `f` for the carried pool,
  which is in the profile before the year drains. It bounds the input
  timing from the other side: on 2010 LPJmL monthly seepage, an explicit
  monthly iteration with equal monthly inputs gives a surplus-weighted
  mean within 2 percentage points of it (root zones of 30 to 150 cm).

- `"burns"`: Burns' displacement for a solute incorporated uniformly
  through the root zone, `f = (P / (P + theta_fc))^(h / 2)`, with `P`
  the drainage and `h` the depth, both in cm (Burns 1976, J. Agric. Sci.
  86:305-313, doi:10.1017/S0021859600054769).

- `"unlimited"`: no transport limit, `f = 1` wherever the row drains:
  the Meisinger cascade as it stood before transport was added, kept to
  measure what the transport limit changes. Nothing is retained on a
  drained row.

Every constant is read from
[n_attenuation_constants](https://eduaguilera.github.io/whep/reference/n_attenuation_constants.md).
The columns a transport reads must be present and non-missing on every
row: `"swat_mixing"` and `"swat_uniform"` read `root_depth_cm` and
`soil_porosity`, `"burns"` `root_depth_cm` and `soil_field_capacity`,
and `"unlimited"` neither.

### Zero drainage and deep vadose storage

Where `D <= 0` nothing is transported (`f = 0`). The year's own
available nitrate `S (1 - d) (1 - a_cn)` is retained, and the carried
pool `C_in` is held as deep vadose storage: it is neither denitrified
nor leached that year and re-enters the cascade in the next year that
drains (Walvoord et al. 2003, Science, doi:10.1126/science.1086435;
Ascott et al. 2017, Nat. Commun., doi:10.1038/s41467-017-01321-w). Such
rows carry the field `vadose_storage`.

### Drainage classes

Drainage and soil organic matter bins are matched with the source's
strictly-open `s_min < s < s_max` filter: a value exactly on a shared
bin edge, or below the lowest bin, matches no bin and aborts via the
unmatched-row check rather than being pulled into an adjacent bin. The
top drainage bin is open: drainage of 3,000 mm and above takes the
`Very_high` share. Meisinger's `None` class (drainage below 0.1 mm/yr)
is set by `none_class`: `"retention"` (default) takes the `Very_low`
row's share, so what does not denitrify is retained or stored;
`"waterlogged"` keeps the table's literal `None` rows, which denitrify
the whole surplus.

Manure/organic rows (`fert_cat == "Manure"`, i.e. every `fert_type`
other than `"Synthetic"`) always join the Meisinger table's
`tillage == "Not_specified"` row regardless of `x$tillage`. Synthetic
rows join on `x$tillage`, but only `"Tillage"` exists in the source's
synthetic block: a synthetic row with `tillage == "No_tillage"` aborts.
A pooled `"Excreta"` row (excreta of every species, as
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
emits it) reads the subsoil reduction of
[subsoil_no3_reduction](https://eduaguilera.github.io/whep/reference/subsoil_no3_reduction.md),
which gives `"Excreta_cattle_monog"` and `"Excreta_other"` the same
value in every class; were they to differ, the row aborts
(`whep_leaching_excreta_species`).

### Carry-over across years

With `carry_over_by`, rows are threaded per key in year order: each
row's `C_in` is the `retained_profile_n_t` of its key's previous row. A
key's first row starts at the steady state of that year's fluxes,
`C_in = S r / (1 - r)`, where the share of the pool's input that stays
is `(1 - d) (1 - a_cn) (1 - f)`, written `r` (field
`initial_pool_steady_state`; under `"swat_uniform"` the surplus feeds
the pool with `(1 - d) (1 - a_cn) (1 - f_new)` instead, so
`C_in = S (1 - d) (1 - a_cn) (1 - f_new) / (1 - r)`).
`initial_pool = "zero"` starts every key empty instead (field
`initial_pool_zero`), the lower bound of a run with no legacy nitrate:
in a one-year run under a transport other than `"swat_uniform"` it
leaches `1 - r` times what the steady state does. A key whose first year
does not drain has no finite steady state and starts at 0 (field
`initial_vadose_zero`) under either. Later rows carry the field
`carried_pool`. A key absent from a year of the run keeps its pool on
the key: the pool is neither moved to another key nor dropped, and
re-enters when the key reappears. The row whose pool waits that way, the
key's last row before a run year it lacks, carries the field
`pool_held_absent_key`. Over a run, the sum of `n_surplus_t` equals the
sums of `no3_n_t` and `denitrification_n_t`, plus each key's last
`retained_profile_n_t`, minus each key's first
`retained_carried_in_n_t`. Without `carry_over_by` every row is its own
key's first and only year.

`method_leaching` joins these fields with `;`: `meisinger_drainage`; the
transport; `none_retention` or `none_waterlogged`; the pool field; and,
optionally, `vadose_storage` and `pool_held_absent_key`.

### IMAGE-GNM

`method = "image_gnm"` follows Beusen et al. (2015), eqs. 8-14 (pp.
4051-4052) and Table 2 (p. 4053). Where the paper is silent or
ambiguous, or disagrees with the code, it follows the authors' model
code published with the paper (doi:10.5194/gmd-8-4045-2015-supplement,
`N_model/trunk/tools/`, the code that produced their results), as noted
below.

- `f_climate = f_K T_r` (eq. 12), with
  `f_K = 7.94e12 exp(-74830 / (R K))` (eq. 13), `R = 8.3144` J/(mol K)
  and `K` the mean annual temperature `temp_c_annual_mean` + 273.15 (the
  authors' code adds 273). `f_K` is bounded at 1, as in the authors'
  code (line 97); the bound binds above about 30 degC.

- `T_r = tawc / q_eff` years (eq. 14). `tawc` is the total available
  water capacity of the top 1 m,
  `(soil_field_capacity - soil_wilting_point)` x 1,000 mm. The source
  takes the top 1 m "or less if thinner"; WHEP has no soil-thickness
  layer, so the full 1 m is used everywhere. `q_eff` is the excess water
  flow of eq. 5 (p. 4049), the water that infiltrates and recharges
  shallow and deep groundwater, as the authors' code computes it
  (`n_soil_denitrification.py` lines 356, 376 and 399):
  `q_eff = (1 - f_qsro(landuse) f_qsro) q_tot`, the total runoff less
  the share surface runoff carries off, with the `f_qsro(landuse)` and
  `f_qsro` of eq. 9 below. `q_tot` is `runoff_mm`, the year's total
  runoff: LPJmL's `mrunoff.nc`
  ([`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
  `"runoff"`), which holds the groundwater outflow the deep seepage
  feeds, as the authors' `pnet`, the hydrology model's total runoff,
  does. Below 3 mm/yr the authors' code takes no water through the root
  zone (lines 341-344; `minimal_soil_infiltration = 3.0` mm/yr,
  `parameters_history_100.ini` line 204), so on cropland `q_tot` is 0
  there; grassland and natural land below it never reach eq. 14 (see the
  inactive N below). Where `q_eff` is 0 the authors' code takes
  `T_r = 100` years (`trzd()`, lines 179-182).

- On cropland `T_r` is at most 1 year, as in the authors' code (lines
  377-378: crops must have enough water, for example from irrigation).
  The printed sentence (p. 4052) reads as raising a `T_r` below 1 to 1
  on dry agricultural land; the code lowers every cropland `T_r` above 1
  to 1, and the code is followed.

- `f_text`, `f_drain` and `f_soc` are read from
  [image_gnm_leaching_factors](https://eduaguilera.github.io/whep/reference/image_gnm_leaching_factors.md)
  (Table 2) on `soil_texture_class`, `soil_drainage_class` and the
  topsoil organic carbon `soil_organic_carbon_pct` (% by weight; each
  organic-carbon class holds its lower bound, as in the authors' class
  code). `f_landuse` is 1 on `"Cropland"` and 0.36 on `"Grassland"` and
  `"Natural"` (p. 4052, citing Keuskamp et al. 2012).

- `surface_runoff_n_t` is `N_sro,rec` of eq. 9 (p. 4052), the nitrogen
  surface runoff carries off from the year's inputs before the budget
  leaches: `f_cal f_qsro(landuse) f_qsro N_inp`, with `f_cal = 0.3` and
  `f_qsro(landuse)` 1 on cropland, 0.25 on grassland and 0.125 on
  natural vegetation (Table 3, p. 4056; the authors' code,
  `n_surface_runoff.py` lines 98, 114 and 128, uses the same values).
  `f_qsro`, the product `f_qsro(slope) f_qsro(texture)` of eqs. 3-4 (p.
  4049), is the column `surface_runoff_fraction`: the authors' own
  0.5-degree layer, which their code reads instead of computing eq. 4
  (see
  [`read_image_gnm_runoff()`](https://eduaguilera.github.io/whep/reference/read_image_gnm_runoff.md)).
  `N_inp` follows the authors' code: on cropland and grassland the input
  net of ammonia, `n_input_std_t - nh3_n_t`, floored at 0
  (`n_surface_runoff.py` lines 57-58, 96 and 112); on natural vegetation
  the surplus `n_surplus_t`, floored at 0 (line 128). In
  [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  `n_input_std_t` is biological fixation, excreta, liquid and solid
  manure, synthetic fertiliser, human N applied to land and deposition:
  it includes human N applied to land and excludes N mineralised from
  soil organic matter and recycled crop residues. The authors' input is
  the year's N applied to or deposited on the land, manure, fertiliser,
  fixation and deposition (lines 57-58), the N exposed to surface runoff
  before it enters the soil. Human N applied to land reaches the surface
  as manure does, so it is kept; mineralised soil N is released inside
  the soil and recycled residues return N the crop took from the same
  soil, so neither is a fresh input at the surface. Surface runoff is a
  share of the input, so it can exceed the surplus; eq. 10 is then
  negative, and the authors' code sets the budget to 0 (lines 106, 122
  and 136), as here: nothing leaches or denitrifies and the row books
  `max(n_surplus_t, surface_runoff_n_t)`. A negative surplus (a deficit,
  which
  [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  never passes) is floored at a budget of 0 the same way: nothing
  leaches or denitrifies, and the surface runoff is still booked.

- Inactive N. Where the total runoff `runoff_mm` is below 3 mm/yr, the
  authors' code sets the grassland and natural balances to 0 before
  surface runoff and denitrification and books them as N no water
  transports (`n_nowater.py` lines 31-46, called from `N-model.py` lines
  210-214); cropland is left alone, "for arable land it is assumed that
  there is always enough water" (line 19). Here that N is
  `retained_profile_n_t = max(n_surplus_t, 0)`, and nothing leaches or
  denitrifies. Surface runoff then reads the zeroed balance: natural
  land runs off nothing, and grassland a share of balance + uptake,
  which is its uptake alone (`n_surface_runoff.py` line 96), here the
  input net of ammonia less the surplus,
  `max(0, n_input_std_t - nh3_n_t - n_surplus_t)`. That runoff is booked
  beside the inactive N, not taken from it. Under
  [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md),
  whose surplus counts the N mineralised from soil organic matter as an
  input and soil organic matter sequestration as an output while
  `n_input_std_t` counts neither, this proxy (where the balance is in
  surplus) is the crop product, the used, bedding and burnt residue and
  the grazed forage, plus the sequestration less the mineralisation:
  uptake net of the soil organic matter terms.

- `erosion_n_t` is `N_sro,mem` of eq. 8 (p. 4052), the nitrogen of soil
  lost to erosion, computed as the authors' code does (`n_erosion.py`
  lines 47-55 and 88-135): an erodibility
  `(-1.5 + 17 / (1 + exp(2.3 - 6.1 S / 1000)))` times a texture factor
  (1.5 on medium and fine, 0.5 on the others), with `S` the median slope
  `slope_m_per_km`; a soil loss of 1,151, 99 and 29 t per km2 per unit
  erodibility on cropland, grassland and natural vegetation over
  `area_ha`; and N = soil loss x `soil_organic_carbon_pct` / C:N, with
  C:N 12, 14 and 14 (p. 4052). `area_ha` drives it: in
  [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  it is the physical agricultural land of `ag_land_support`, or, where
  that cannot be derived, the crop's harvested area, which counts
  multi-cropped land once per crop and so overstates erosion there. The
  paper subtracts the whole of eq. 8 in eq. 10; the authors' code
  subtracts only the runoff from the recent inputs and adds the eroded N
  to the river load (`N-model.py` lines 233-243 and 307), so
  `erosion_n_t` is reported and taken from no budget.

- IMAGE-GNM keeps no root-zone nitrate from one year to the next:
  `retained_carried_in_n_t` is 0 by construction, `retained_profile_n_t`
  is 0 on every row with water and is not carried into the next year,
  and `transport`, `none_class` and `carry_over_by` do not apply.

Two parts of the authors' code are not reproduced. Its separate water
capacities of dominant and suitable soils, because WHEP has one soil per
cell. And its `f_soc`, which it interpolates linearly from 0 at 0% to
0.3 at 6% organic carbon; the printed classes of Table 2 are used
instead.

`method_leaching` is `image_gnm;tawc_top_1m`, followed where they apply
by `runoff_below_min_infiltration` (a non-zero `runoff_mm` below 3 mm/yr
set to 0), `no_excess_water_residence_100yr` (`q_eff` of 0),
`crop_residence_cap_1yr` (a cropland `T_r` above 1 year held at 1),
`temperature_effect_cap_1` (an `f_K` above 1 held at 1),
`nowater_inactive_n` (a grassland or natural row below 3 mm/yr, whose
surplus is inactive N; such a row carries none of the three fields
before it), `negative_surplus_budget_0` (a negative `n_surplus_t`,
leaving a budget of 0) and `surface_runoff_exceeds_budget` (a surface
runoff above a non-negative surplus, leaving a budget of 0), joined with
`;`.

### Errors

- `whep_missing_leaching_driver` (also `whep_absent_input`): a driver
  column the transport reads is absent or holds a missing value, or
  `drainage_mm` is absent or missing.

- `whep_leaching_driver_value`: a non-positive `root_depth_cm`, a
  porosity or field capacity outside (0, 1\], or a drainage at or below
  -0.1 mm.

- `whep_leaching_carry_key`: `carry_over_by` names an absent column, `x`
  has no `year`, or a key occurs twice in one year.

For `method = "image_gnm"`:

- `whep_missing_leaching_driver` (also `whep_absent_input`): a column it
  reads, `runoff_mm` among them, is absent or holds a missing value.

- `whep_leaching_driver_value`: a field capacity outside (0, 1\], a
  wilting point outside (0, field capacity), a negative organic carbon,
  slope, area, input or ammonia, a non-finite temperature, a non-finite
  `runoff_mm` or one at or below `water_flux_floor_mm` of
  [n_attenuation_constants](https://eduaguilera.github.io/whep/reference/n_attenuation_constants.md)
  (-0.1 mm; LPJmL's float residues just below 0 pass and fall under the
  3 mm/yr threshold), or a surface runoff fraction outside `[0, 1]`.

- `whep_gnm_runoff_supplied`: `x` already holds `surface_runoff_n_t`,
  which `"image_gnm"` computes; drop it rather than have it overwritten.

- `whep_leaching_class_value`: a texture, drainage or land-use label
  [image_gnm_leaching_factors](https://eduaguilera.github.io/whep/reference/image_gnm_leaching_factors.md)
  does not hold. The authors' code books an unknown texture or drainage
  class as 0; here it aborts, because a class that cannot be read is not
  a class.

For `method = "ipcc_fracleach"`, a missing or invalid `moisture_regime`
aborts as in
[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md),
and a dry row without a `water_regime` aborts
(`whep_missing_water_regime` for an absent column,
`whep_water_regime_value` for a missing or unknown value, naming the
count): dry irrigated and dry rainfed land leach 0.24 and 0 of their
surplus, so a dry row of unknown regime has no defensible fraction. Wet
rows need no `water_regime`. `"fracleach_surplus"` checks the same.
`transport`, `none_class`, `carry_over_by` and `initial_pool` do not
apply to either.

Under `"ipcc_fracleach"` leaching is capped at `n_surplus_t`, so it
never exceeds the nitrogen left after crop uptake. IPCC Tier 1 has no
such cap; rows where it binds carry the suffix `"_capped_surplus"` on
their stamp (e.g. `"ipcc_fracleach_wet_capped_surplus"`). A missing
`n_input_for_n2o_t` column, or a missing value in it, aborts
(`whep_missing_n_added`, `whep_n_added_na`): read as zero it would leach
nothing while the partition still closed.

## Examples

``` r
calculate_n_leaching(example = TRUE)
#> # A tibble: 1 × 8
#>   n_surplus_t moisture_regime n_input_for_n2o_t no3_n_t denitrification_n_t
#>         <dbl> <chr>                       <dbl>   <dbl>               <dbl>
#> 1          60 wet                           100      24                  36
#> # ℹ 3 more variables: retained_profile_n_t <dbl>, n2o_indirect_no3_n_t <dbl>,
#> #   method_leaching <chr>
```
