#' Animal codes and classifications
#'
#' Maps live animal CBS items to their livestock classifications, process codes,
#' and associated product items used in livestock modeling.
#'
#' @format
#' A tibble where each row corresponds to one live animal CBS item.
#' It contains the following columns:
#' - `Item_Code`: Numeric FAOSTAT item code for the live animal.
#' - `item_cbs`: Name of the CBS item (e.g., `"Cattle"`, `"Asses"`).
#' - `proc_code`: Short process code used internally (e.g., `"p092"`).
#' - `proc`: Descriptive process name (e.g., `"Asses"`).
#' - `item_code_cbs`: Numeric CBS item code (often equal to `Item_Code`).
#' - `Farm_class`: Broad farm classification grouping the animal. One of
#'   `"Cattle"`, `"Dairy_cows"`, `"Monogastric"`, `"Sheep_goats"`,
#'   `"Bees"`, `"Game"`.
#' - `Item_product`: Name of the primary product derived from this animal, if
#'   applicable (e.g., milk for dairy cows).
#' - `Item_Code_product`: Numeric FAOSTAT code for the associated product item.
#' - `Liv_prod_cat`: Livestock product category the animal belongs to.
#' - `Graniv_grazers`: Broad feeding behaviour classification. One of
#'   `"Grazers"`, `"Granivores"`, `"Bees"`, `"Game"`.
#' - `Livestock_name`: Internal livestock identifier used across datasets
#'   (e.g., `"Cattle"`, `"Dairy_cows"`, `"Asses"`).
#' - `Animal_class`: Fine-grained animal class, including production type
#'   distinctions (e.g., `"Broilers"`, `"Hens"`, `"Hogs"`, `"Dairy_cows"`).
#' - `Item_FAOmanure`: Name of the corresponding FAOSTAT manure management item.
#' - `Item_Code_FAOmanure`: Numeric code of the FAOSTAT manure management item.
#' - `Cat_Labour`: Labour category used in labour-related analyses. One of
#'   `"Cattle"`, `"Equines"`, `"Dairy_cows"`, `"Birds"`,
#'   `"Small_ruminants"`, `"Pigs"`, `"Bees"`.
#' - `Cat_FAO1`: Top-level FAO category. Currently always `"Animal"`.
#' - `item_bouwman`: Item name used in Bouwman et al. livestock datasets.
#' @source Derived from [FAOSTAT data](https://www.fao.org/faostat/en/#data/QA)
#'   and internal livestock classification work.
"animals_codes"

#' Biomass coefficients for crops and livestock products
#'
#' Provides dry-matter, nutrient, and energy conversion coefficients for
#' agricultural products and residues. Used to convert fresh-matter production
#' quantities into biomass flows, nutrient budgets, and energy content.
#'
#' @format
#' A tibble where each row corresponds to one product or item. It contains
#' 68 columns:
#' - `Code`: Item code (character), corresponding to FAOSTAT production codes.
#' - `Name_biomass`: Item name as used in biomass accounting.
#' - `Equiv`: Reference equivalence item used when coefficients are borrowed
#'   from another similar commodity (e.g., `"Wheat"` for oats).
#' - `Category`: Broad commodity category (e.g., `"Cereals, other"`,
#'   `"Barley"`, `"Vegetables"`).
#' - `BG_Biomass_kgDM_ha`: Below-ground biomass in kg dry matter per hectare.
#' - `Root_Shoot_ratio`: Ratio of root to aerial biomass (dimensionless).
#' - `Product_kgDM_kgFM`: Product dry-matter content in kg DM per kg fresh
#'   matter.
#' - `Residue_kgDM_kgFM`: Residue dry-matter content in kg DM per kg fresh
#'   matter of product.
#' - `Conventional_kgDM_ha`: Conventional yield in kg dry matter per hectare.
#' - `Organic_kgDM_ha`: Organic yield in kg dry matter per hectare.
#' - `GE_product_edible_portion_MJ_kgFM`: Gross energy of the edible portion
#'   in MJ per kg fresh matter.
#' - `GE_product_residue_MJ_kgFM`: Gross energy of the residue in MJ per kg
#'   fresh matter (may be character due to source formatting).
#' - `GE_product_MJ_kgFM`: Gross energy of the whole product in MJ per kg
#'   fresh matter.
#' - `GE_residue_MJ_kg`: Gross energy of the residue in MJ per kg.
#' - `kg_product_kg_aerial_biomass`: Fraction of aerial biomass that is
#'   product (harvest index, kg/kg).
#' - `kg_residue_kg_aerial_biomass_FM`: Fraction of aerial biomass that is
#'   residue, on fresh matter basis.
#' - `kg_residue_kg_product_FM`: Ratio of residue to product on fresh matter
#'   basis.
#' - `Carcass_to_LW`: Carcass-to-live-weight ratio (livestock only; logical
#'   placeholder for crop items).
#' - `Edible_portion`: Edible fraction of the product (kg edible / kg fresh
#'   matter).
#' - `N_kgN_kgFM`: Nitrogen content in kg N per kg fresh matter.
#' - `Lipids_g_kgFM`: Lipid content in g per kg fresh matter.
#' - `Carbohydrates_g_kgFM`: Carbohydrate content in g per kg fresh matter.
#' - `Calcium_mg_kgFM`: Calcium content in mg per kg fresh matter.
#' - `VitaminA_microg_kgFM`: Vitamin A content in micrograms per kg fresh
#'   matter.
#' - `Edible_kgDM_kgFM`: Edible dry matter in kg per kg fresh matter.
#' - `Edible_kgC_kgFM`: Edible carbon in kg C per kg fresh matter.
#' - `Edible_N_kgFM`: Edible nitrogen in kg N per kg fresh matter.
#' - `Edible_kgP_kgFM`: Edible phosphorus in kg P per kg fresh matter.
#' - `Edible_K_kgFM`: Edible potassium in kg K per kg fresh matter.
#' - `NonEdible_kgDM_kgFM`: Non-edible dry matter in kg per kg fresh matter.
#' - `NonEdible_kgC_kgFM`: Non-edible carbon in kg C per kg fresh matter.
#' - `NonEdible_kgN_kgFM`: Non-edible nitrogen in kg N per kg fresh matter.
#' - `NonEdible_kgP_kgFM`: Non-edible phosphorus in kg P per kg fresh matter.
#' - `NonEdible_kgK_kgFM`: Non-edible potassium in kg K per kg fresh matter.
#' - `Product_kgN_kgDM`: Nitrogen content of product in kg N per kg dry
#'   matter.
#' - `Product_kgP_kgDM`: Phosphorus content of product in kg P per kg dry
#'   matter.
#' - `Product_kgK_kgDM`: Potassium content of product in kg K per kg dry
#'   matter.
#' - `Product_kgC_kgDM`: Carbon content of product in kg C per kg dry matter.
#' - `Residue_kgN_kgDM`: Nitrogen content of residue in kg N per kg dry
#'   matter.
#' - `Residue_kgP_kgDM`: Phosphorus content of residue in kg P per kg dry
#'   matter.
#' - `Residue_kgK_kgDM`: Potassium content of residue in kg K per kg dry
#'   matter.
#' - `Residue_kgC_kgDM`: Carbon content of residue in kg C per kg dry matter.
#' - `Residue_humified_kgC_kgC`: Humification coefficient of residue carbon
#'   (fraction of residue C stabilised as soil organic matter).
#' - `MgDM_m3`: Megagrams dry matter per cubic metre (bulk density proxy).
#' - `Root_kgC_kgDM`: Carbon content of roots in kg C per kg root dry matter.
#' - `Root_humified_kgC_kgC`: Humification coefficient for root carbon.
#' - `Root_mass_kgC_kgDM`: Root carbon mass in kg C per kg crop dry matter.
#' - `Rhizodeposits_mass_kgC_kgDM`: Rhizodeposit carbon in kg C per kg crop
#'   dry matter.
#' - `Residue_C_N`: Carbon-to-nitrogen ratio of the residue.
#' - `Root_kgN_kgDM`: Nitrogen content of roots in kg N per kg root dry
#'   matter.
#' - `GE_Roots_MJ_kgDM`: Gross energy of roots in MJ per kg dry matter.
#' - `Rhizodeposits_N_kgN_kgRootN`: Rhizodeposit nitrogen as a fraction of
#'   root nitrogen.
#' - `Fiber_g_kgFM`: Dietary fibre content in g per kg fresh matter.
#' - `SFA_g_kgFM`: Saturated fatty acid content in g per kg fresh matter.
#' - `MUFA_g_kgFM`: Monounsaturated fatty acid content in g per kg fresh
#'   matter.
#' - `PUFA_g_kgFM`: Polyunsaturated fatty acid content in g per kg fresh
#'   matter.
#' - `PUFA_n3_g_kgFM`: Omega-3 PUFA content in g per kg fresh matter.
#' - `Iron_mg_kgFM`: Iron content in mg per kg fresh matter.
#' - `Zinc_mg_kgFM`: Zinc content in mg per kg fresh matter.
#' - `Magnesium_mg_kgFM`: Magnesium content in mg per kg fresh matter.
#' - `Cadmium_microg_kgFM`: Cadmium content in micrograms per kg fresh matter.
#' - `VitaminB12_microg_kgFM`: Vitamin B12 content in micrograms per kg fresh
#'   matter.
#' - `VitaminD_microg_kgFM`: Vitamin D content in micrograms per kg fresh
#'   matter.
#' - `Folate_microg_kgFM`: Folate content in micrograms per kg fresh matter.
#' - `VitaminC_mg_kgFM`: Vitamin C content in mg per kg fresh matter.
#' - `VitaminE_mg_kgFM`: Vitamin E content in mg per kg fresh matter.
#' - `Flavonoids_mg_kgFM`: Flavonoid content in mg per kg fresh matter.
#' - `Carotenoids_mg_kgFM`: Carotenoid content in mg per kg fresh matter.
#' @source Compiled from multiple sources including FAO food composition data,
#'   crop physiology literature, and IPCC Tier 1 coefficients.
"biomass_coefs"

#' Commodity balance sheet processing fractions
#'
#' Specifies the product fractions obtained when CBS items are processed,
#' linking processed items to their output CBS categories.
#'
#' @format
#' A tibble where each row corresponds to one processed-item / output-category
#' combination. It contains the following columns:
#' - `ProcessedItem`: Name of the CBS item being processed (e.g.,
#'   `"Apples and products"`, `"Barley and products"`).
#' - `item_cbs`: Name of the output CBS category produced by processing (e.g.,
#'   `"Alcohol, Non-Food"`).
#' - `Product_fraction`: Fraction of the processed item that yields the output
#'   product (numeric, 0–1).
#' - `Value_fraction`: Economic value fraction associated with the output
#'   product (numeric; largely `NA` in current data).
#' - `Required`: Reserved column, currently all `NA`.
#' @source Derived from FAOSTAT commodity balance sheet processing assumptions.
"cb_processing"

#' CBS to trade item code mapping
#'
#' Maps detailed FAOSTAT trade item codes to their corresponding CBS item
#' categories, enabling aggregation of bilateral trade data into the CBS
#' framework.
#'
#' @format
#' A tibble where each row corresponds to one trade item. It contains the
#' following columns:
#' - `item_code_trade`: Numeric FAOSTAT trade item code (e.g., `15` for
#'   wheat).
#' - `item_trade`: Name of the trade item (e.g., `"Wheat"`,
#'   `"Flour, wheat"`, `"Bran, wheat"`).
#' - `item_cbs`: Name of the CBS category this trade item belongs to (e.g.,
#'   `"Wheat and products"`).
#' - `item_check`: Cross-validation column repeating the mapped CBS name;
#'   used to flag mapping inconsistencies during data processing.
#' @source Derived from [FAOSTAT Detailed Trade Matrix](https://www.fao.org/faostat/en/#data/TM)
#'   and commodity balance sheet correspondence tables.
"cbs_trade_codes"

#' Eurostat crop classification codes
#'
#' Maps Eurostat crop codes to their full crop category names, used when
#' integrating Eurostat agricultural statistics.
#'
#' @format
#' A tibble where each row corresponds to one Eurostat crop category.
#' It contains the following columns:
#' - `Crop`: Eurostat crop code (e.g., `"G0000"`, `"G1000"`).
#' - `Name_Eurostat`: Full name of the crop category as used in Eurostat
#'   (e.g., `"Plants harvested green from arable land"`,
#'   `"Temporary grasses and grazings"`).
#' @source [Eurostat Agricultural Statistics](https://ec.europa.eu/eurostat/statistics-explained/index.php/Agricultural_statistics).
"crops_eurostat"

#' Full CBS item table
#'
#' Extended item reference table covering all CBS items, including their
#' process and commodity codes, feed type classifications, and default
#' material flow destinations.
#'
#' @format
#' A tibble where each row corresponds to one CBS item. It contains the
#' following columns:
#' - `item_cbs`: Name of the CBS item.
#' - `item_code_cbs`: Numeric CBS item code.
#' - `comm_code`: Commodity code used in process-based modelling (may contain
#'   `"#N/A"` when not applicable).
#' - `proc_code`: Process code (may contain `"#N/A"` when not applicable).
#' - `proc`: Process name (may contain `"#N/A"` when not applicable).
#' - `unit`: Measurement unit (typically `"tonnes"`).
#' - `group`: Broad item group. Common values include `"Additives"`,
#'   `"Crop products"`, `"Crop residues"`, `"Draught"`, `"Fish"`,
#'   `"Forestry"`, `"Grass"`, `"Livestock"`, and others.
#' - `feedtype_graniv`: Feed type classification for granivores
#'   (e.g., `"additives"`, `"concentrates"`, `"roughages"`).
#' - `feedtype_grazers`: Feed type classification for grazers.
#' - `comm_group`: Sub-group of the commodity (e.g., `"Additives"`,
#'   `"Alcohol"`, `"Ethanol"`, `"Oil cakes"`,
#'   `"Other processing residues"`).
#' - `Cat_1`: Primary category label used in material flow accounting.
#' - `Name_biomass`: Corresponding item name in `biomass_coefs`, enabling
#'   joins with the biomass coefficient table.
#' - `dbMFA_items`: Item identifier used in the material flow analysis
#'   database.
#' - `FEDNA`: Item name used in FEDNA feed composition tables.
#' - `default_destiny`: Default CBS use category for this item. One of
#'   `"Feed"`, `"Food"`, `"Other_uses"`, `"Processing"`, or `NA`.
#' @source Derived from [FAOSTAT data](https://www.fao.org/faostat/en/#data/FBS)
#'   and internal commodity classification work.
"items_full"

#' Primary production items linked to CBS
#'
#' Maps FAOSTAT primary production items and crop products to their CBS
#' counterparts, along with farm and labour classifications.
#'
#' @format
#' A tibble where each row corresponds to one production item. It contains
#' the following columns:
#' - `item_prod`: Name of the production item (e.g., `"Wheat"`, `"Rice"`).
#' - `item_code_prod`: FAOSTAT production item code (character).
#' - `item_cbs`: Name of the corresponding CBS item.
#' - `item_code_cbs`: Numeric CBS item code.
#' - `Farm_class`: Farm system classification. Crop items use codes such as
#'   `"COP"` (cereals, oilseeds, protein crops), `"Vegetables"`,
#'   `"Fruits"`, `"Olive"`, `"Grapevine"`, `"Other_crops"`. Livestock
#'   items use `"Dairy_cows"`, `"Cattle"`, `"Monogastric"`,
#'   `"Sheep_goats"`, `"Bees"`, `"Game"`. `NA` for non-farm items.
#' - `Cat_Labour`: Labour category used in agricultural labour analyses.
#' - `Cat_FAO1`: Top-level FAO commodity category.
#' - `group`: Item group classification. One of `"Primary crops"`,
#'   `"Crop products"`, `"Livestock products"`, `"Grass"`,
#'   `"Crop residues"`, `"Scavenging"`, `"Livestock"`.
#' @source Derived from [FAOSTAT Production data](https://www.fao.org/faostat/en/#data/QCL).
"items_prim"

#' Full production item table
#'
#' Comprehensive reference table for all production items, combining CBS
#' linkages, biomass names, multiple classification schemes, and crop
#' ecological traits.
#'
#' @format
#' A tibble where each row corresponds to one production item. It contains
#' the following columns:
#' - `item_prod`: Name of the production item.
#' - `item_code_prod`: FAOSTAT production item code (character).
#' - `item_cbs`: Name of the corresponding CBS item.
#' - `item_code_cbs`: Numeric CBS item code.
#' - `group`: Item group. One of `"Primary crops"`, `"Crop products"`,
#'   `"Livestock products"`, `"Crop residues"`, `"Grassland"`,
#'   `"Scavenging"`.
#' - `Live_anim`: Name of the parent live animal for livestock-derived items
#'   (`NA` for crop items).
#' - `Live_anim_code`: Numeric CBS code of the parent live animal (`NA` for
#'   crop items).
#' - `Cat_Krausmann`: Item category used in Krausmann et al. biomass flow
#'   accounting.
#' - `Name_biomass`: Corresponding item name in `biomass_coefs`, enabling
#'   joins for biomass coefficients.
#' - `Name_Eurostat`: Corresponding item name in Eurostat agricultural
#'   statistics.
#' - `Name`: Alternative or display name for the item.
#' - `Cat_Labour`: Labour category used in agricultural labour analyses.
#' - `Cat_FAO1`: Top-level FAO commodity category (e.g., `"Cereals"`,
#'   `"Oilcrops"`).
#' - `Cat_Origin`: Origin-based commodity category. One of `"Cereals"`,
#'   `"Vegetables and Fruits"`, `"Sugar and Stimulants"`, `"Oil Crops"`,
#'   `"Fodder crops"`, `"Fibres and Crude Materials"`, or `NA`.
#' - `Cat_Use`: Use-based commodity category (e.g., `"Grains"`,
#'   `"Oils and Fats"`, `"Fodder crops"`, `"Beverages, sugar and
#'   stimulants"`).
#' - `Order`: Numeric ordering field used for sorting items consistently
#'   across outputs.
#' - `Categ`: General category label used in some analyses.
#' - `Farm_class`: Farm system classification (see `items_prim` for values).
#' - `c3_c4`: Photosynthetic pathway. One of `"c3"`, `"c4"`, or `NA`.
#' - `ann_per_nfx`: Annual/perennial and nitrogen fixation trait. One of
#'   `"ann"` (annual), `"per"` (perennial), `"nfx"` (nitrogen-fixing),
#'   or `NA`.
#' - `Cat_1`: Primary category label for material flow accounting.
#' - `Cat_2`: Secondary category label.
#' - `Cat_3`: Tertiary category label.
#' - `Cat_4`: Quaternary category label.
#' - `Herb_Woody`: Plant growth form. One of `"Herbaceous"`, `"Woody"`, or
#'   `NA`.
#' - `Crop_irrig`: Irrigation category used in water-use analyses.
#' - `Cat_Org`: Organic farming category classification.
#' - `Cat_Ymax`: Maximum attainable yield category.
#' - `Cat_Ymax_leg`: Legend label for the `Cat_Ymax` category.
#' @source Derived from [FAOSTAT Production data](https://www.fao.org/faostat/en/#data/QCL)
#'   and multiple classification schemes from the literature.
"items_prod_full"

#' Livestock unit coefficients
#'
#' Provides livestock unit (LU) conversion factors per head for each animal
#' class, used to express heterogeneous livestock populations in comparable
#' units.
#'
#' @format
#' A tibble where each row corresponds to one animal class. It contains the
#' following columns:
#' - `Animal_class`: Animal class identifier (e.g., `"Dairy_cows"`,
#'   `"Cattle"`, `"Sheep_goats"`, `"Broilers"`, `"Hens"`, `"Pigs"`).
#' - `LU_head`: Livestock units per head (numeric). Dairy cows have a value
#'   of 1.0 by convention; smaller animals have proportionally lower values.
#' @source Based on standard livestock unit definitions from FAO and
#'   European agricultural statistics.
"liv_lu_coefs"

#' Polity categories and regional classifications
#'
#' Reference table for countries and political entities (polities) with
#' identifiers from multiple data sources and assignments to various regional
#' groupings used in the literature and international databases.
#'
#' @format
#' A tibble where each row corresponds to one polity (country or territory).
#' It contains the following columns:
#' - `polity_code`: ISO 3166-1 alpha-3 country code used as the primary
#'   identifier (e.g., `"AFG"`, `"ALB"`).
#' - `polity_name`: Common country or territory name.
#' - `V1`: Internal row index from the source table.
#' - `code`: Numeric FAOSTAT country code.
#' - `iso3c`: ISO 3166-1 alpha-3 code (character; may duplicate
#'   `polity_code` or differ for aggregates).
#' - `FAOSTAT_name`: Country name as used in FAOSTAT.
#' - `EU27`: Logical flag; `TRUE` if the polity is a member of the EU27.
#' - `name`: Country name used in other external databases.
#' - `eia`: Country name or code used by the US Energy Information
#'   Administration (EIA).
#' - `iea`: Country identifier used by the International Energy Agency (IEA).
#' - `water_code`: Numeric code used in water statistics datasets.
#' - `water_area`: Country/area name used in water statistics.
#' - `baci`: Numeric BACI trade database country code.
#' - `fish`: Numeric code used in fisheries datasets.
#' - `region_code`: Numeric regional grouping code.
#' - `cbs`: Logical flag; `TRUE` if the polity is included in the CBS
#'   dataset.
#' - `fabio_code`: Numeric country code used in the FABIO database.
#' - `ADB_Region`: Asian Development Bank regional classification.
#' - `region`: General world region (e.g., `"South Asia"`,
#'   `"Eastern Europe"`).
#' - `uISO3c`: Numeric Unicode / UN M49 country code.
#' - `Lassaletta`: Country grouping used in Lassaletta et al. nitrogen flow
#'   studies.
#' - `region_krausmann`: Regional grouping from Krausmann et al. biomass
#'   flow accounting.
#' - `region_HANPP`: Regional grouping used in human appropriation of net
#'   primary production (HANPP) studies.
#' - `region_krausmann2`: Alternative Krausmann regional grouping.
#' - `region_UN_sub`: UN sub-regional classification (M49 sub-region).
#' - `region_UN`: UN macro-regional classification (M49 region).
#' - `region_ILO1`: ILO primary regional grouping.
#' - `region_ILO2`: ILO secondary regional grouping.
#' - `region_ILO3`: ILO tertiary regional grouping.
#' - `region_IEA`: IEA regional grouping.
#' - `region_IPCC`: IPCC regional grouping used in climate assessments.
#' - `region_labour`: Labour-focused regional grouping.
#' - `region_labour_agg`: Aggregated labour-focused regional grouping.
#' - `region_labour_mech`: Labour mechanisation regional grouping.
#' - `region_test`: Experimental/test regional grouping (may be incomplete).
#' @source Compiled from [FAOSTAT](https://www.fao.org/faostat/), UN M49,
#'   ILO, IEA, and other international statistical sources.
#' @note Five trailing columns containing only Excel `#REF!` errors in the
#'   source CSV are dropped at load time and are not part of this dataset.
"polities_cats"

#' Items with double-counting in production statistics
#'
#' Identifies production items that appear both as primary crop products and
#' as harvested-area items, requiring special treatment to avoid
#' double-counting in production and biomass accounting.
#'
#' @format
#' A tibble where each row corresponds to one item pair with a
#' double-counting relationship. It contains the following columns:
#' - `Item_area`: Name of the item as it appears in harvested-area statistics
#'   (e.g., `"Seed cotton, unginned"`).
#' - `item_prod`: Name of the derived production item (e.g.,
#'   `"Cotton lint, ginned"`, `"Cotton seed"`).
#' - `item_code_prod`: Numeric FAOSTAT production code of the derived item.
#' - `Multi_type`: Classification of the double-counting type:
#'   - `"Primary"`: The area item is the primary crop; product is a direct
#'     output.
#'   - `"Primary_area"`: Area is recorded under a primary aggregate crop
#'     name.
#'   - `"Multi"`: Multiple products share the same harvested area.
#'   - `"Multi_area"`: Multiple products share a recorded area aggregate.
#' @source Derived from FAOSTAT production methodology documentation.
"primary_double"

#' Full polity and region reference table
#'
#' Extended reference table covering all polities and aggregate regions,
#' including countries, territories, and statistical composites that appear
#' in international databases but may lack standard ISO codes.
#'
#' @format
#' A tibble where each row corresponds to one polity or aggregate region. It
#' contains the following columns (same definitions as `polities_cats`,
#' minus the five trailing `0...36`–`0...40` artefact columns):
#' - `polity_code`: Primary polity identifier (ISO 3166-1 alpha-3 or `NA`
#'   for non-sovereign aggregates).
#' - `polity_name`: Polity name (`NA` for aggregates not matched to a
#'   standard polity).
#' - `V1`: Internal row index.
#' - `code`: Numeric FAOSTAT country/region code.
#' - `iso3c`: ISO 3166-1 alpha-3 code (`NA` for aggregates).
#' - `FAOSTAT_name`: Name used in FAOSTAT (may be `"#N/A"` for aggregates).
#' - `EU27`: Logical EU27 membership flag.
#' - `name`: Name used in external databases.
#' - `eia`: EIA country identifier.
#' - `iea`: IEA country identifier.
#' - `water_code`: Water statistics numeric code.
#' - `water_area`: Name used in water statistics.
#' - `baci`: BACI trade database country code.
#' - `fish`: Fisheries dataset numeric code.
#' - `region_code`: Numeric regional code.
#' - `cbs`: Logical CBS dataset membership flag.
#' - `fabio_code`: FABIO database numeric code.
#' - `ADB_Region`: Asian Development Bank region.
#' - `region`: General world region.
#' - `uISO3c`: UN M49 numeric code.
#' - `Lassaletta`: Lassaletta et al. nitrogen study grouping.
#' - `region_krausmann`: Krausmann regional grouping.
#' - `region_HANPP`: HANPP study regional grouping.
#' - `region_krausmann2`: Alternative Krausmann grouping.
#' - `region_UN_sub`: UN M49 sub-region.
#' - `region_UN`: UN M49 macro-region.
#' - `region_ILO1`: ILO primary region.
#' - `region_ILO2`: ILO secondary region.
#' - `region_ILO3`: ILO tertiary region.
#' - `region_IEA`: IEA region.
#' - `region_IPCC`: IPCC region.
#' - `region_labour`: Labour-focused region.
#' - `region_labour_agg`: Aggregated labour region.
#' - `region_labour_mech`: Labour mechanisation region.
#' - `region_test`: Experimental regional grouping.
#' @seealso [polities_cats] for the subset restricted to sovereign countries.
#' @source Compiled from [FAOSTAT](https://www.fao.org/faostat/), UN M49,
#'   ILO, IEA, and other international statistical sources.
"regions_full"
