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
68 columns:

- `Code`: Item code (character), corresponding to FAOSTAT production
  codes.

- `Name_biomass`: Item name as used in biomass accounting.

- `Equiv`: Reference equivalence item used when coefficients are
  borrowed from another similar commodity (e.g., `"Wheat"` for oats).

- `Category`: Broad commodity category (e.g., `"Cereals, other"`,
  `"Barley"`, `"Vegetables"`).

- `BG_Biomass_kgDM_ha`: Below-ground biomass in kg dry matter per
  hectare.

- `Root_Shoot_ratio`: Ratio of root to aerial biomass (dimensionless).

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
  matter.

- `Edible_kgDM_kgFM`: Edible dry matter in kg per kg fresh matter.

- `Edible_kgC_kgFM`: Edible carbon in kg C per kg fresh matter.

- `Edible_N_kgFM`: Edible nitrogen in kg N per kg fresh matter.

- `Edible_kgP_kgFM`: Edible phosphorus in kg P per kg fresh matter.

- `Edible_K_kgFM`: Edible potassium in kg K per kg fresh matter.

- `NonEdible_kgDM_kgFM`: Non-edible dry matter in kg per kg fresh
  matter.

- `NonEdible_kgC_kgFM`: Non-edible carbon in kg C per kg fresh matter.

- `NonEdible_kgN_kgFM`: Non-edible nitrogen in kg N per kg fresh matter.

- `NonEdible_kgP_kgFM`: Non-edible phosphorus in kg P per kg fresh
  matter.

- `NonEdible_kgK_kgFM`: Non-edible potassium in kg K per kg fresh
  matter.

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

- `Root_kgC_kgDM`: Carbon content of roots in kg C per kg root dry
  matter.

- `Root_humified_kgC_kgC`: Humification coefficient for root carbon.

- `Root_mass_kgC_kgDM`: Root carbon mass in kg C per kg crop dry matter.

- `Rhizodeposits_mass_kgC_kgDM`: Rhizodeposit carbon in kg C per kg crop
  dry matter.

- `Residue_C_N`: Carbon-to-nitrogen ratio of the residue.

- `Root_kgN_kgDM`: Nitrogen content of roots in kg N per kg root dry
  matter.

- `GE_Roots_MJ_kgDM`: Gross energy of roots in MJ per kg dry matter.

- `Rhizodeposits_N_kgN_kgRootN`: Rhizodeposit nitrogen as a fraction of
  root nitrogen.

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
