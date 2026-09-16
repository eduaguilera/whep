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
  matter.

`N_kgN_kgFM` is the food-composition nitrogen density of the edible
part, and is what
[`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
turns into protein. Its provenance is weak and worth knowing before
trusting a row. Upstream it sits under the `NUTRIENTS IN EDIBLE PART`
group header with the four columns above, but the workbook's `Sources`
sheet has **no column for any of the five**: its columns run from
`kg_residue_kg_product_FM` straight to `Product_kgN_kgDM`, so the whole
nutrition block is undocumented at source (#500 section 6, \#1074). Most
rows are a `VLOOKUP` into the workbook's food-composition sheet
`Conversores_Dieta`, the same sheet that fills the four columns above;
that sheet derives its own nitrogen column as
`Proteinas / (6.25 * 1000)`. The derivation is missing for exactly its
five **cereal** rows – `Wheat`, `Oats`, `Rye`, `Maize` and `Rice` –
which carry a `Proteinas` value but no nitrogen, so for those five the
lookup was replaced by the agronomic
`Product_kgN_kgDM * Product_kgDM_kgFM`. Other rows, `Vegetables, other`,
`Olive` and `Honey` among them, hold a bare literal. Those eight are the
ones \#1096 tracks.

`Wheat` was the largest consequence. It carried 0.018951324393104405 kg
N per kg, 118.45 g of protein per kg at N x 6.25, which is exactly
`Product_kgN_kgDM * Product_kgDM_kgFM` – the agronomic whole-grain
nitrogen the `Sources` sheet attributes to FEDNA 2016, a *feed* table.
Applied to a commodity-balance `food` quantity that is the wheat-grain
equivalent of the milled products people eat, that counts the bran and
germ protein milling diverts away from food.

It now carries **0.01488 kg N per kg, i.e. 93 g of protein per kg: the
flour basis**. The value is read from the workbook cell, not recalled:
`Biomass_coefs.xlsx`, sheet `Conversores_Dieta`, row 4, labelled
`Harina de Trigo` (wheat flour) and keyed to `Name_biomass = "Wheat"`,
column `Proteinas` = 93 g/kg. That row is where this table already takes
the wheat lipids (12 g/kg), carbohydrates (800 g/kg), calcium (150
mg/kg) and vitamin A (0) from – all four ship today – so the wheat
nutrition block was flour composition everywhere except protein, and the
change makes the row internally consistent rather than importing a
foreign number.

**Neither 118.45 nor 93 has a `Sources` entry**, so 93 is corroborated,
not cited upstream. What "flour basis" means numerically is protein per
kg of *grain equivalent*, which is flour protein times the milling
extraction rate, so the published tables were read on that basis:

- FAO, *Food composition tables for international use*, 2nd ed., Rome,
  1953, <https://www.fao.org/4/x5557e/x5557e04.htm> (FAO flags it as
  historical). Medium wheat: whole meal 12.2 g/100 g at 100% extraction,
  flour 11.7 at 85%, 10.9 at 72% – so 99.4 and 78.5 g per kg of grain
  equivalent, which bracket 93.

- USDA FoodData Central 168894 (SR Legacy), "Wheat flour, white,
  all-purpose, enriched, bleached": protein 10.33 g/100 g, so 103.3 g
  per kg of flour, 72-88 g per kg of grain equivalent at 70-85%
  extraction.

- FAO, *Technical conversion factors for agricultural commodities*,
  Rome,
  <https://www.fao.org/fileadmin/templates/ess/documents/methodology/tcf.pdf>
  – its per-country `Flour of Wheat` extraction rates run 70-97% with a
  median of 75%, which is why no single flour figure maps to one
  grain-equivalent density.

The oracle is FAOSTAT FBS itself, which builds its protein the same way:
composition factors on the products eaten, over the standardised
primary-equivalent quantity that WHEP's `food_t` also is (FAO, *Food
balance sheets: a handbook*, section III,
<https://www.fao.org/4/X9892E/X9892e03.htm>). For item 2511 its **World
row (area 5000)** implies 91.71-93.15 g/kg over 2010-2023, 92.99 in
2010; summed instead over the 171 countries reporting both elements it
is 94.96-96.90, 96.60 in 2010. So 93 lands within 0.02% of the World row
and 3.7% below the country sum, where 118.45 was above both: 27.4% above
the World row and 22.6% above the country sum (whep#796).

One global coefficient cannot carry the extraction pattern country by
country: FBS's own per-country implied density for 2511 in 2010 spans
41-115 g/kg (q10 69, median 81, q90 90), so 93 is the mass-weighted
world point of that distribution and leaves the median country high. The
previous value stays selectable as
`build_food_supply(protein_basis = "product_nitrogen")`, which reads the
agronomic route directly and returns exactly 0.11844577745690253 kg
protein per kg; the choice is recorded per row in
`method_protein_basis`. The other seven rows are left alone and tracked
in \#1096: for `Rice` the basis question was settled in \#751/#755, and
for `Oats` and `Maize` the food-composition figure moves *away* from the
FBS oracle, so they need the expert rather than this edit.

The ten `Edible_*` and `NonEdible_*` nutrient columns below are **empty
in every row**, upstream in the source workbook as well as here, so no
edible/non-edible nutrient split can be read from them (#361). Use
`Edible_portion` with `N_kgN_kgFM` or `Product_kgN_kgDM` to derive an
edible basis instead, as
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

The synthetic feed-additive rows take this column from the FEDNA feed
tables as crude protein divided by 6.25, the same way `Threonine`,
`Tryptophan`, `Valine` and `Urea` still do. `Methionine` carried 0.1143
instead, which is 21.7% above the most nitrogen the molecule can hold:
methionine is C5H11NO2S with one nitrogen atom, so its mass fraction is
14.007 over 149.208, or 0.0939 kg N per kg. It is now FEDNA's
DL-Metionina entry, 58.5% crude protein and better than 99% purity,
giving 0.0936 (whep#931;
<https://fundacionfedna.org/ingredientes-para-piensos>). The alternative
is the other commercial methionine source, the hydroxy analogue FEDNA
lists as HIDROXI-ANAL MET, which is C5H10O3S and holds no nitrogen at
all; that is the product `codes_coefs_items_full` names for this item
and the value the retired pin carried. `Lysine` at 0.2015 is the other
hand-entered override and is still 5.2% above the free base's own
0.1916; FEDNA's L-Lisina HCl would give 0.1512.

- `Product_kgP_kgDM`: Phosphorus content of product in kg P per kg dry
  matter.

- `Product_kgK_kgDM`: Potassium content of product in kg K per kg dry
  matter.

- `Product_kgC_kgDM`: Carbon content of product in kg C per kg dry
  matter.

- `Residue_kgN_kgDM`: Nitrogen content of residue in kg N per kg dry
  matter.

For the wood and forest rows this column prices two physically different
quantities through one cell.
[`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md)
sends the harvested `Wood` item to `Average wood`, and also relabels the
residue production of forest and shrubland land as `Firewood`, which
resolves to the same row. Harvested wood is stemwood; forest residue is
branches, bark and foliage, and the two differ by a factor of five. In
the trembling-aspen budget of Morrison and Foster (1979, reproduced in
Hacker 2005, "Effects of Logging Residue Removal on Forest Sites")
stemwood holds 84 kg N in 119 t of dry matter, 0.00071 kg N per kg,
against 0.0042 for bark, 0.0049 for branches and 0.024 for foliage.
`Average wood` ships 0.0030, the mean of a beech, a conifer and a
holm-oak anchor; the retired pin carried 0.00095 for the same rows.
Which end the term should sit at is open in whep#932, so the value must
not be moved without settling that first.

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
