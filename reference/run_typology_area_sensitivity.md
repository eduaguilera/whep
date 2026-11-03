# Reclassify typologies with Julia's and Josette's thresholds, on both area bases

Julia's and Josette's own typology methods (`Typologies_Julia.R`,
`Typologies_Josette.R`) use published livestock-density thresholds of
0.5 and 1 LU/ha respectively, against utilised agricultural area (UAA:
cropland, permanent pasture/shrubland and dehesa).
`create_typologies_spain()` instead divides by the whole province area,
with its own empirically-fit thresholds (0.25/0.3) – the two are not
directly comparable as published.

This reclassifies the baseline typology with each literature threshold
in turn (applied uniformly to `livestock_density_int`/`ext_lo`/
`ext_hi`, since neither Julia's nor Josette's method draws Alice's
intensive/extensive distinction within "specialized livestock"), on
**both** area bases – the whole-province `Livestock_density`
`create_typologies_spain()` already computes, and a UAA-recomputed one –
so the two effects can be told apart: applying a literature threshold to
the *wrong* (whole-province) denominator on its own is not the same test
as applying it together with the UAA basis it was published against.
Every other threshold (crop productivity, synthetic share,
connected/disconnected) stays at its `.typology_thresholds()` value
throughout, so only the livestock-density basis varies.

These are sensitivity scenarios, not reproductions of the complete
published decision trees: the other WHEP rules stay fixed. Threshold
sources are Le Noe et al. (2017), section 3.2.2, p.49,
[doi:10.1016/j.scitotenv.2017.02.040](https://doi.org/10.1016/j.scitotenv.2017.02.040)
, and Garnier et al. (2023), Figure 2a,
[doi:10.1016/j.jenvman.2023.117732](https://doi.org/10.1016/j.jenvman.2023.117732)
. Both use agricultural area; neither validates the empirically selected
WHEP whole-province thresholds. The studies share authors and should not
be counted as independent validation of the WHEP calibration.

## Usage

``` r
run_typology_area_sensitivity(
  n_prov_destiny = NULL,
  npp_ygpit = NULL,
  baseline = NULL
)
```

## Arguments

- n_prov_destiny:

  Nitrogen flows tibble, passed to `create_typologies_spain()` when
  `baseline` is `NULL`. If `NULL`, loaded automatically (slow).

- npp_ygpit:

  Land-use area tibble with `Year`, `Province_name`, `LandUse` and
  `Area_ygpit_ha`. If `NULL`, read from the `npp_ygpit` pin.

- baseline:

  Pre-computed indicator table from `create_typologies_spain()`,
  carrying `year`, `province_name`, `LU_total`, `Typology_base` and the
  other indicator columns `.classify_typology_base()` acts on. If
  `NULL`, computed automatically (slow).

  Inputs must resolve to dated polity identities. Duplicate aliases
  within a source polity-year, missing/non-finite UAA, zero UAA and
  unresolved territories abort with diagnostic rows; they never become
  classifications.

## Value

A tibble with columns `source` (`"Julia"`/`"Josette"`), `uaa_threshold`,
`area_basis` (`"whole_province"`/`"uaa"`), `agreement_pct` (share of
province-years keeping the baseline's `Typology_base`), and
`n_specialized_livestock` (province-years classified as either
specialized-livestock category under that source/basis combination).

## Examples

``` r
# A tiny fixture: two provinces/years, one clearly cropping-specialized
# (low density either way) and one where the UAA/whole-province area
# difference changes which side of a literature threshold it lands on.
baseline <- tibble::tribble(
  ~year, ~province_name, ~production_seminatural, ~production_crops,
  ~animal_ingestion, ~synthetic_share, ~crop_productivity, ~LU_total,
  ~Livestock_density, ~imported_feed_share, ~feed_from_seminatural_share,
  ~local_feed_share, ~Manure_share, ~Typology_base,
  2000, "Huesca", 1, 100, 5, 0.8, 40, 500,
  0.1, 0.1, 0.1,
  0.1, 0.1, "Specialized cropping systems (intensive)",
  2000, "Zaragoza", 1, 10, 50, 0.1, 40, 6000,
  0.4, 0.8, 0.1,
  0.1, 0.1, "Specialized livestock systems (extensive)"
)
npp_ygpit <- tibble::tribble(
  ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
  2000, "Huesca", "Cropland", 8000,
  2000, "Huesca", "Forest_high", 2000,
  2000, "Zaragoza", "Cropland", 4000,
  2000, "Zaragoza", "Pasture_Shrubland", 2000,
  2000, "Zaragoza", "Forest_high", 9000
)
sensitivity <- run_typology_area_sensitivity(
  npp_ygpit = npp_ygpit,
  baseline = baseline
)
```
