# Biomass coefficients for crops and livestock products

Provides dry-matter, nutrient, and energy conversion coefficients for
agricultural products and residues. Used to convert fresh-matter
production quantities into biomass flows, nutrient budgets, and energy
content.

## Usage

``` r
biomass_coefs
```

## Format

A tibble where each row corresponds to one product or item. It contains
63 columns:

- `Code`: Item code (character), corresponding to FAOSTAT production
  codes.

- `Name_biomass`: Item name as used in biomass accounting.

- `Equiv`: Reference equivalence item used when coefficients are
  borrowed from another similar commodity (e.g., `"Wheat"` for oats).

- `Category`: Broad commodity category (e.g., `"Cereals, other"`,
  `"Barley"`, `"Vegetables"`).

- `Product_kgDM_kgFM`: Product dry-matter content in kg DM per kg fresh
  matter.

- `Residue_kgDM_kgFM`: Residue dry-matter content in kg DM per kg fresh
  matter of product.

- `Conventional_kgDM_ha`: Conventional yield in kg dry matter per
  hectare.

- `Organic_kgDM_ha`: Organic yield in kg dry matter per hectare.

- `GE_product_edible_portion_MJ_kgFM`: Gross energy of the edible
  portion in MJ per kg fresh matter.

- `GE_product_residue_MJ_kgFM`: Gross energy of the residue in MJ per kg
  fresh matter (may be character due to source formatting).

- `GE_product_MJ_kgFM`: Gross energy of the whole product in MJ per kg
  fresh matter.

- `GE_residue_MJ_kg`: Gross energy of the residue in MJ per kg.

- `kg_product_kg_aerial_biomass`: Fraction of aerial biomass that is
  product (harvest index, kg/kg).

- `kg_residue_kg_aerial_biomass_FM`: Fraction of aerial biomass that is
  residue, on fresh matter basis.

- `kg_residue_kg_product_FM`: Ratio of residue to product on fresh
  matter basis.

- `Carcass_to_LW`: Carcass-to-live-weight ratio (livestock only; logical
  placeholder for crop items).

- `Edible_portion`: Edible fraction of the product (kg edible / kg fresh
  matter).

- `N_kgN_kgFM`: Nitrogen content in kg N per kg fresh matter.

- `Lipids_g_kgFM`: Lipid content in g per kg fresh matter.

- `Carbohydrates_g_kgFM`: Carbohydrate content in g per kg fresh matter.

- `Calcium_mg_kgFM`: Calcium content in mg per kg fresh matter.

- `VitaminA_microg_kgFM`: Vitamin A content in micrograms per kg fresh
  matter. The ten `Edible_*` and `NonEdible_*` nutrient columns below
  are **empty in every row**, upstream in the source workbook as well as
  here, so no edible/non-edible nutrient split can be read from them
  (#361). Use `Edible_portion` with `N_kgN_kgFM` or `Product_kgN_kgDM`
  to derive an edible basis instead, as
  [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
  does.

- `Edible_kgDM_kgFM`: Edible dry matter in kg per kg fresh matter.
  Empty.

- `Edible_kgC_kgFM`: Edible carbon in kg C per kg fresh matter. Empty.

- `Edible_N_kgFM`: Edible nitrogen in kg N per kg fresh matter. Empty.

- `Edible_kgP_kgFM`: Edible phosphorus in kg P per kg fresh matter.
  Empty.

- `Edible_K_kgFM`: Edible potassium in kg K per kg fresh matter. Empty.

- `NonEdible_kgDM_kgFM`: Non-edible dry matter, kg per kg fresh matter.
  Empty.

- `NonEdible_kgC_kgFM`: Non-edible carbon in kg C per kg fresh matter.
  Empty.

- `NonEdible_kgN_kgFM`: Non-edible nitrogen in kg N per kg fresh matter.
  Empty.

- `NonEdible_kgP_kgFM`: Non-edible phosphorus, kg P per kg fresh matter.
  Empty.

- `NonEdible_kgK_kgFM`: Non-edible potassium, kg K per kg fresh matter.
  Empty.

- `Product_kgN_kgDM`: Nitrogen content of product in kg N per kg dry
  matter.

- `Product_kgP_kgDM`: Phosphorus content of product in kg P per kg dry
  matter.

- `Product_kgK_kgDM`: Potassium content of product in kg K per kg dry
  matter.

- `Product_kgC_kgDM`: Carbon content of product in kg C per kg dry
  matter.

- `Residue_kgN_kgDM`: Nitrogen content of residue in kg N per kg dry
  matter.

- `Residue_kgP_kgDM`: Phosphorus content of residue in kg P per kg dry
  matter.

- `Residue_kgK_kgDM`: Potassium content of residue in kg K per kg dry
  matter.

- `Residue_kgC_kgDM`: Carbon content of residue in kg C per kg dry
  matter.

- `Residue_humified_kgC_kgC`: Humification coefficient of residue carbon
  (fraction of residue C stabilised as soil organic matter).

- `MgDM_m3`: Megagrams dry matter per cubic metre (bulk density proxy).

- `Root_humified_kgC_kgC`: Humification coefficient for root carbon.

- `Root_mass_kgC_kgDM`: Root carbon mass in kg C per kg crop dry matter.

- `Residue_C_N`: Carbon-to-nitrogen ratio of the residue.

- `Root_kgN_kgDM`: Nitrogen content of roots in kg N per kg root dry
  matter.

- `GE_Roots_MJ_kgDM`: Gross energy of roots in MJ per kg dry matter.

- `Fiber_g_kgFM`: Dietary fibre content in g per kg fresh matter.

- `SFA_g_kgFM`: Saturated fatty acid content in g per kg fresh matter.

- `MUFA_g_kgFM`: Monounsaturated fatty acid content in g per kg fresh
  matter.

- `PUFA_g_kgFM`: Polyunsaturated fatty acid content in g per kg fresh
  matter.

- `PUFA_n3_g_kgFM`: Omega-3 PUFA content in g per kg fresh matter.

- `Iron_mg_kgFM`: Iron content in mg per kg fresh matter.

- `Zinc_mg_kgFM`: Zinc content in mg per kg fresh matter.

- `Magnesium_mg_kgFM`: Magnesium content in mg per kg fresh matter.

- `Cadmium_microg_kgFM`: Cadmium content in micrograms per kg fresh
  matter.

- `VitaminB12_microg_kgFM`: Vitamin B12 content in micrograms per kg
  fresh matter.

- `VitaminD_microg_kgFM`: Vitamin D content in micrograms per kg fresh
  matter.

- `Folate_microg_kgFM`: Folate content in micrograms per kg fresh
  matter.

- `VitaminC_mg_kgFM`: Vitamin C content in mg per kg fresh matter.

- `VitaminE_mg_kgFM`: Vitamin E content in mg per kg fresh matter.

- `Flavonoids_mg_kgFM`: Flavonoid content in mg per kg fresh matter.

- `Carotenoids_mg_kgFM`: Carotenoid content in mg per kg fresh matter.

## Source

Compiled from multiple sources including FAO food composition data, crop
physiology literature, and IPCC Tier 1 coefficients.

## Details

This is the **single** source of biomass coefficients in the package.
Until \#489 a `biomass_coefs` pin frozen at `20250728T082553Z` was also
readable through
[`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md);
it was a narrowed 2025 export that disagreed with this table on 12 of
their 36 shared columns, so the same commodity carried different
nitrogen coefficients depending on which path read it. The pin has been
retired and all callers now read this dataset.

Five runtime-dead below-ground fields were retired from this legacy
table: `BG_Biomass_kgDM_ha`, `Root_Shoot_ratio`, `Root_kgC_kgDM`,
`Rhizodeposits_mass_kgC_kgDM`, and `Rhizodeposits_N_kgN_kgRootN`. Their
related item-keyed fields in `bio_coefs` are, respectively,
`bg_biomass_dm_kg_ha`, `root_shoot_ratio`, `root_c_kgdm`,
`rhizodeposit_mass_c_kgdm`, and `rhizodeposit_n_kgn_krootn`. This is not
a universal one-to-one row mapping.
[`calculate_crop_roots()`](https://eduaguilera.github.io/whep/reference/calculate_crop_roots.md)
uses `ipcc_root_coefs$bg_ref_dm_t_ha` and `ipcc_root_coefs$rs_default`
when they are available, with the corresponding `bio_coefs` fields as
fallbacks. `root_c_kgdm` and `rhizodeposit_n_kgn_krootn` are direct
calculation inputs. `rhizodeposit_mass_c_kgdm` is an integrity and
documentation component that is already included in `root_c_kgdm`,
rather than a separate runtime input.

Three all-caps rows of the source spreadsheet are section headers rather
than commodities and are dropped at ingestion (#752):
`TRANSFORMED PRODUCTS` and `AGRO-INDUSTRY BYPRODUCTS` are empty, and
`ANIMAL PRODUCTS` holds the VLOOKUP column-index vector the upstream
`Coefs` sheet addresses by absolute position, which read as data claims
an `Edible_portion` of 4.

## Examples

``` r
head(biomass_coefs)
#> # A tibble: 6 × 63
#>   Code  Name_biomass Equiv Category       Product_kgDM_kgFM Residue_kgDM_kgFM
#>   <chr> <chr>        <chr> <chr>                      <dbl>             <dbl>
#> 1 75    Oats         Wheat Cereals, other             0.867             0.907
#> 2 44    Barley       Wheat Barley                     0.885             0.864
#> 3 27    Rice         NA    Rice                       0.864             0.91 
#> 4 79    Millet       Maize Cereals, other             0.880             0.9  
#> 5 71    Rye          NA    Cereals, other             0.876             0.924
#> 6 83    Sorghum      Maize Cereals, other             0.865             0.87 
#> # ℹ 57 more variables: Conventional_kgDM_ha <dbl>, Organic_kgDM_ha <dbl>,
#> #   GE_product_edible_portion_MJ_kgFM <dbl>, GE_product_residue_MJ_kgFM <dbl>,
#> #   GE_product_MJ_kgFM <dbl>, GE_residue_MJ_kg <dbl>,
#> #   kg_product_kg_aerial_biomass <dbl>, kg_residue_kg_aerial_biomass_FM <dbl>,
#> #   kg_residue_kg_product_FM <dbl>, Carcass_to_LW <lgl>, Edible_portion <dbl>,
#> #   N_kgN_kgFM <dbl>, Lipids_g_kgFM <dbl>, Carbohydrates_g_kgFM <dbl>,
#> #   Calcium_mg_kgFM <dbl>, VitaminA_microg_kgFM <dbl>, …
```
