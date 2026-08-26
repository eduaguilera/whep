# Livestock Coefficient Tables Documentation
#
# Roxygen2 documentation for all livestock coefficient data
# objects stored in data/livestock_coefs.rda.

# GLEAM Excel Supplement Tables ----
#
# Provenance of the twelve tables in this section: they are parsed from
# data-raw/GLEAM_3.0_Supplement_S1.xlsx, which is byte-identical (149119
# bytes, md5 207e3e928c176b2189e520bddcb0c5f6) to the workbook FAO
# publishes at
# https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx
# Its own file metadata names "FAO of the UN" and is dated 2022-12-20; the
# parent document, "Global Livestock Environmental Assessment Model,
# Version 3.0 - Model description", is dated 2022-12-10 on
# https://www.fao.org/gleam/resources/en/. Neither carries a DOI: Crossref
# holds no GLEAM 3.0 model-description record and the FAO-style DOI
# 10.4060/cd8425en does not resolve, so these tables are cited by title and
# URL. The DOI they used to cite, 10.1088/1748-9326/aad4d8, is not
# registered at all (doi.org 404, Crossref "Resource not found") -- it was
# never a real reference. See whep#607.

#' GLEAM crop residue parameters.
#'
#' @description
#' Dry matter content and parameters for calculating crop
#' residue yield by crop type.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{crop}{Crop name.}
#'   \item{dry_matter_pct}{Dry matter content (percent).}
#'   \item{slope}{Slope for residue yield calculation.}
#'   \item{intercept}{Intercept for residue yield calculation.}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Table S.3.1:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_crop_residue_params
"gleam_crop_residue_params"

#' GLEAM geographic hierarchy.
#'
#' @description
#' Maps countries (ISO3) to GLEAM regions, FAOSTAT regions,
#' and classification indicators.
#'
#' This is GLEAM's own registry of the countries that exist today, so
#' [polity_identity_conventions()] types it `"present_day_polity"` and it
#' carries the polity its `iso3` resolves to in the present day, as
#' [regions_full] does. All 204 rows now resolve: ATF, SGS and WLF used to keep
#' `NA` for want of any WHEP polity, and upstream supplied one for each. A gap
#' would still stay visible rather than being guessed at. The 204 GLEAM regions
#' themselves are unchanged.
#'
#' `oecd` is the one column not carried through verbatim. Cell G41 of the
#' published sheet flags Comoros as an OECD member; it is not one, so
#' `data-raw/livestock_coefficients.R` rebuilds the whole column from the
#' OECD's own list of its 38 Members (oecd.org, "Members and partners", read
#' 2026-08-25; the URL is in the builder, which is not URL-checked, because the
#' host answers `R CMD check --as-cran` with a 403) and
#' aborts if the sheet ever stops listing a Member. Every other flagged code
#' already matched that list, Colombia (2020) and Costa Rica (2021) included,
#' and `eu27` beside it is exactly the 27 real EU members, so the cell reads as
#' a data-entry slip rather than a GLEAM grouping that borrows the name. The
#' flag is load-bearing: [build_energy_co2_extension()] reads it for two of
#' GLEAM's three country-grouping schemes, and as shipped it priced Comoros'
#' meat at OECD energy intensity, up to 129 percent high (whep#574).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{iso3}{ISO3 country code.}
#'   \item{country}{Country name.}
#'   \item{continent}{Continent.}
#'   \item{faostat_region}{FAOSTAT regional grouping.}
#'   \item{gleam_region}{GLEAM regional grouping.}
#'   \item{eu27}{1 for an EU-27 member, 0 otherwise.}
#'   \item{oecd}{1 for an OECD member, 0 otherwise.}
#'   \item{reporting_polity_code}{The [polities] code `iso3` resolves to in the
#'     present day, `NA` where WHEP has no polity for the territory.}
#'   \item{reporting_polity_name}{The name of that polity.}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Tables S.A1-S.A2:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_geographic_hierarchy
"gleam_geographic_hierarchy"

#' GLEAM feed use efficiency.
#'
#' @description
#' Regional feed use efficiency (FUE) values for forages and
#' crop residues of ruminant species.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{feed_group}{Feed material group (1-6 or 9-15).}
#'   \item{feed_type}{Feed type (mixed, grassland, or all).}
#'   \item{gleam_region}{GLEAM geographic region.}
#'   \item{feed_use_efficiency}{FUE value (0-1 fraction).}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Table S.3.2:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_feed_composition
"gleam_feed_composition"

#' GLEAM feed digestibility for ruminants.
#'
#' @description
#' Nutritional values for feed materials of ruminant species,
#' including gross energy, nitrogen content, and digestibility.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{number}{Feed material number.}
#'   \item{material}{Feed material code.}
#'   \item{gross_energy_mj_kg}{Gross energy (MJ per kg DM).}
#'   \item{n_content_g_kg}{Nitrogen content (g per kg DM).}
#'   \item{digestibility_pct}{Digestibility (percent).}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Table S.3.3:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_feed_digestibility
"gleam_feed_digestibility"

#' GLEAM feed conversion ratios for monogastrics.
#'
#' @description
#' Nutritional values for feed materials of monogastric
#' species (chicken and pigs).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{number}{Feed material number.}
#'   \item{material}{Feed material code.}
#'   \item{gross_energy_j_kg}{Gross energy (J per kg).}
#'   \item{n_content_g_kg}{Nitrogen content (g per kg DM).}
#'   \item{me_chicken_j_kg}{Metabolisable energy for chicken
#'     (J per kg).}
#'   \item{me_pigs_j_kg}{Metabolisable energy for pigs
#'     (J per kg).}
#'   \item{digestibility_pct}{Digestibility (percent).}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Table S.3.4:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_feed_conversion_ratios
"gleam_feed_conversion_ratios"

#' Emission factors for field operations on feed materials.
#'
#' @description
#' CO2-equivalent emissions per hectare from field operations
#' for ruminant and monogastric feed materials.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{material_number}{Sequential material identifier.}
#'   \item{material}{Feed material code
#'     (e.g. \code{"GRASSF"}, \code{"WHEAT"}).}
#'   \item{emission_factor_kg_co2eq_ha}{Emission factor in
#'     kg CO2-eq per hectare.}
#'   \item{species_group}{\code{"ruminant"} or
#'     \code{"monogastric"}.}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Tables S.6.1 and S.6.2:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_field_operation_ef
"gleam_field_operation_ef"

#' Country-level mechanization levels for feed materials.
#'
#' @description
#' Mechanization level by country for each feed material,
#' for ruminant and monogastric species.
#'
#' @format A tibble in long format with columns:
#' \describe{
#'   \item{country}{Country name.}
#'   \item{continent}{Continent.}
#'   \item{region}{GLEAM region.}
#'   \item{feed_material}{Feed material code in lowercase.}
#'   \item{mechanization_level}{Numeric mechanization level.}
#'   \item{species_group}{\code{"ruminant"} or
#'     \code{"monogastric"}.}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Tables S.6.3 and S.6.4:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_mechanization_levels
"gleam_mechanization_levels"

#' Processing and transport emission factors for feeds.
#'
#' @description
#' Emission factors for processing and transport of feed
#' materials, for ruminant and monogastric species.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{material_number}{Sequential material identifier.}
#'   \item{material}{Feed material code.}
#'   \item{processing_g_co2eq_kg_dm}{Processing emission
#'     factor in g CO2-eq per kg dry matter.}
#'   \item{transport_g_co2eq_kg_dm}{Transport emission factor
#'     in g CO2-eq per kg dry matter.}
#'   \item{species_group}{\code{"ruminant"} or
#'     \code{"monogastric"}.}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Tables S.6.5 and S.6.6:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_processing_transport_ef
"gleam_processing_transport_ef"

#' Nitrogen parameters for crop residues of feed materials.
#'
#' @description
#' Nitrogen content of above- and below-ground residues and
#' root-to-shoot ratios for feed materials.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{material_number}{Sequential material identifier.}
#'   \item{material}{Feed material code.}
#'   \item{n_ag}{Nitrogen content of above-ground residues.}
#'   \item{rbg_bio}{Ratio of below-ground residues to
#'     above-ground biomass.}
#'   \item{n_bg}{Nitrogen content of below-ground residues.}
#'   \item{species_group}{\code{"ruminant"} or
#'     \code{"monogastric"}.}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Tables S.6.7 and S.6.8:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_crop_residue_nitrogen
"gleam_crop_residue_nitrogen"

#' Country-level fraction of crop residues removed.
#'
#' @description
#' Countries whose FracReMove value differs from the GLEAM
#' default.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{country}{Country name.}
#'   \item{continent}{Continent.}
#'   \item{region}{GLEAM region.}
#'   \item{fracremove}{Fraction of crop residues removed
#'     (0 to 1).}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Table S.6.9:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_fracremove
"gleam_fracremove"

#' Energy use emission factors for livestock production.
#'
#' @description
#' Emission factors for embedded (feed-production) and direct
#' (on-farm) energy use in livestock production, from GLEAM 3.0
#' tables S.7.1 through S.7.7. Note that the factors are
#' expressed per kilogram of \emph{live weight}, \emph{milk} or
#' \emph{egg} depending on the species and herd: see the
#' \code{denominator} column. The GLEAM footnotes are
#' materialised as derived rows: embedded energy for meat
#' (non-dairy) cattle and all buffalo is half of dairy cattle
#' (S.7.1 note a); embedded energy for non-dairy small ruminants
#' is half of the listed values (S.7.2 note a); direct energy
#' for dairy small ruminants is double the dairy cattle values
#' (S.7.5 note a).
#'
#' @format A tibble in long format with columns:
#' \describe{
#'   \item{species}{Animal species or group (\code{"cattle"},
#'     \code{"buffalo"}, \code{"large_ruminants"},
#'     \code{"small_ruminants"}, \code{"pigs"},
#'     \code{"chickens"}).}
#'   \item{herd}{Herd or product line (\code{"dairy"},
#'     \code{"non_dairy"}, \code{"broilers"}, \code{"layers"},
#'     \code{"all"}). \code{NA} for pigs.}
#'   \item{grouping}{Country or country group as reported by
#'     GLEAM (e.g. \code{"OECD"}, \code{"EU 27"},
#'     \code{"Least developed countries"}).}
#'   \item{grouping_scheme}{Which country grouping the
#'     \code{grouping} belongs to: \code{"development3"}
#'     (OECD / least developed / others), \code{"region5"}
#'     (OECD / four non-OECD regions) or \code{"detailed15"}
#'     (individual OECD members plus world regions).}
#'   \item{system}{Production system (e.g.
#'     \code{"grassland_based"}, \code{"industrial"}).
#'     \code{NA} when not applicable.}
#'   \item{climate}{Climate zone (\code{"arid"},
#'     \code{"humid"}, \code{"temperate"}). \code{NA} when
#'     not applicable.}
#'   \item{energy_type}{\code{"embedded"} or
#'     \code{"direct"}.}
#'   \item{denominator}{Reporting basis of the factor:
#'     \code{"lw"} (live weight), \code{"milk"} or
#'     \code{"egg"}.}
#'   \item{emission_factor}{Emission factor in kg CO2-eq per
#'     kg of the \code{denominator}.}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Tables S.7.1 through S.7.7:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_energy_use_ef
"gleam_energy_use_ef"

#' GLEAM dressing percentages.
#'
#' @description
#' Carcass weight as percentage of live weight by species,
#' production system, cohort, and GLEAM region. Includes
#' country-specific overrides for industrial pig systems in
#' Western Europe.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{species}{Animal species (Cattle, Buffaloes, Sheep,
#'     Goats, Pigs, Chicken).}
#'   \item{production_system}{Production system (Dairy, Beef,
#'     Backyard, Intermediate, Industrial, Layers, Broilers).
#'     NA for species without system breakdown.}
#'   \item{cohort}{Cohort (e.g. Adult and replacement female).
#'     NA for species without cohort breakdown.}
#'   \item{country}{Country name for country-specific values.
#'     NA for regional values.}
#'   \item{gleam_region}{GLEAM region abbreviation (NA, RUS,
#'     WE, EE, NENA, ESEA, OCE, SA, LAC, SSA).}
#'   \item{dressing_percent}{Dressing percentage.}
#' }
#'
#' @source FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO
#'   workbook; no DOI is issued for it), Table S.9.1:
#'   \url{https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx}
#'
#' @examples
#' gleam_dressing_percentages
"gleam_dressing_percentages"

#' GLEAM livestock categories.
#'
#' @description
#' Species, production systems, and cohort definitions from
#' GLEAM 3.0.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{species}{Animal species.}
#'   \item{production_system}{Dairy, Beef, Meat, etc.}
#'   \item{cohort}{Age/sex cohort.}
#'   \item{description}{Cohort description.}
#' }
#'
#' @source Partly traced (whep#881). The cohort vocabulary corresponds to
#'   FAO. 2022. *Global Livestock Environmental Assessment Model, Model
#'   Description, Version 3.0*. Rome, FAO, Table 2.1 "Summary of cohorts in
#'   GLEAM", p. 10 (document code `cd8425en`, served from `www.fao.org/3/` and
#'   `openknowledge.fao.org`), identical to Table 2.1
#'   p. 9 of the Version 2.0 Revision 5 description
#'   (<https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Model_description.pdf>).
#'   The shipped table is **not** a transcription of it: GLEAM names cohorts
#'   AF/RF/AM/RM/MF/MM (plus MFr/MMr/MFf/MMf for feedlots) within one herd,
#'   whereas this table renames them, crosses them with a
#'   Dairy/Beef/Meat/Other production system GLEAM's Table 2.1 does not have,
#'   and supplies its own `description` strings. GLEAM publishes no table of
#'   cohort shares; it derives herd structure from the replacement, fertility,
#'   mortality and age-at-first-calving rates in Supplement S1 of the
#'   Version 2.0 description. `calculate_cohorts_systems()` instead splits a
#'   herd equally, `1 / n` per row of this table, so the row COUNT per
#'   (species, production system) is a result-affecting, unsourced assumption:
#'   Cattle Dairy 6 cohorts (16.7% each), Cattle Beef 5 (20% each). Treat the
#'   layout and the implied equal split as unverified.
#'
#' @examples
#' gleam_livestock_categories
"gleam_livestock_categories"

#' GLEAM feed categories.
#'
#' @description
#' Feed classification used in GLEAM 3.0.
#'
#' @format A tibble with `feed_category`, `feed_type`,
#'   `description`.
#'
#' @source Not traced to any GLEAM document (whep#881). Searched and ruled
#'   out: the GLEAM 3.0 Supplement S1 workbook (no sheet holds it); FAO. 2022.
#'   *Model Description, Version 3.0*, Table 3.1/3.3 (ruminants, 27 feed
#'   materials grouped as Roughages / Cereals / By-products / Concentrates,
#'   pp. 32-37) and Table 3.5 (monogastrics, 42 materials grouped as Swill and
#'   scavenging / Locally-produced / Non-local, p. 45); and Tables 3.2, 3.3
#'   and 3.14 of the Version 2.0 Revision 5 description. None of those
#'   groupings is the six-way Grass / Crop residues / Concentrates / Fodder
#'   crops / Processed feeds / Animal products split shipped here, and
#'   "Animal products - Milk, fish meal" has no counterpart in GLEAM's
#'   ruminant material list at all. Treat the classification as a WHEP-local
#'   convenience taxonomy, not a GLEAM table. No function in `R/` reads it.
#'
#' @examples
#' gleam_feed_categories
"gleam_feed_categories"

#' GLEAM enteric fermentation parameters.
#'
#' @description
#' Ym (% GE) values by species and production system.
#' Feedlot cattle use 3.0% per IPCC 2019 Table 10.12.
#'
#' @format A tibble with `species`, `system`, `ym_percent`,
#'   `notes`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.
#'
#' @examples
#' gleam_enteric_params
"gleam_enteric_params"

#' GLEAM manure management system shares.
#'
#' @description
#' Regional MMS allocation by species and system.
#'
#' @format A tibble with `region`, `species`, `system`, `mms`,
#'   `share_percent`.
#'
#' @source The shipped values are unsourced placeholders, but the real GLEAM
#'   table has been located (whep#881): FAO. 2018. *Global Livestock
#'   Environmental Assessment Model, Model description, Version 2.0,
#'   Revision 5*. Rome, FAO, Supplement S1, Tables 4.2-4.11 (regional MMS
#'   averages for dairy cattle, beef cattle, feedlot cattle, dairy and
#'   non-dairy buffalo, small ruminants, backyard/intermediate/industrial pigs
#'   and chickens), workbook
#'   <https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Supplement_S1.xlsx>
#'   (md5 `72fd2ea477dfe8b30cd3657b2baa4af1`, retrieved 2026-08-26). The
#'   Version 3.0 description and its Supplement S1 publish MMS *definitions*
#'   (Tables 4.1-4.5) but no regional shares. The shipped values disagree
#'   materially with Table 4.2/4.3: Western Europe dairy cattle is
#'   liquid/slurry 42%, solid storage 30%, pasture 27%, daily spread 1% in
#'   GLEAM against 60/30/10 here, and the Sub-Saharan Africa (90/10), Latin
#'   America (95/5) and South Asia (60/30/10) rows omit the drylot share
#'   GLEAM gives as 35%, 42% and 54% respectively. Not re-ingested here
#'   because no function in `R/` reads this object -- the manure chain uses
#'   `regional_mms_distribution`, which is separately unsourced.
#'
#' @examples
#' gleam_mms_shares
"gleam_mms_shares"

#' GLEAM animal weights.
#'
#' @description
#' Typical live weights by region, species, system, and cohort.
#'
#' @format A tibble with `region`, `species`, `system`,
#'   `cohort`, `weight_kg`.
#'
#' @source The shipped values are unsourced placeholders, but the real GLEAM
#'   table has been located (whep#881): FAO. 2018. *Global Livestock
#'   Environmental Assessment Model, Model description, Version 2.0,
#'   Revision 5*. Rome, FAO, Supplement S1, "Live weights (kg)" block of
#'   Tables 2.4 (dairy cattle), 2.5 (beef cattle), 2.6 (feedlot cattle),
#'   2.7 (buffaloes), 2.10 (sheep), 2.11 (goats) and 2.14-2.16 (backyard,
#'   intermediate and industrial pigs), workbook
#'   <https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Supplement_S1.xlsx>
#'   (md5 `72fd2ea477dfe8b30cd3657b2baa4af1`, retrieved 2026-08-26). The
#'   Version 3.0 description drops those regional herd-parameter tables, which
#'   is why the committed GLEAM 3.0 Supplement S1 workbook has no sheet for
#'   them.
#'
#'   The shipped values do **not** match that source and have not been
#'   replaced here, because doing so is a science decision that moves
#'   published numbers: `.join_weights()` in `R/livestock_energy.R` uses this
#'   table as the Tier 2 live weight, and gross energy scales as
#'   `weight^0.75` for maintenance and `weight` for activity. Measured per
#'   cohort against the GLEAM values (system-averaged the way
#'   `.join_weights()` averages them), gross energy -- hence Tier 2 enteric
#'   CH4 -- would move by: Global cattle adult female 400 -> 457 kg, +6.9%;
#'   Global cattle fattening 300 -> 399 kg, +14.2%; Western Europe cattle
#'   adult male 1000 -> 732 kg, -16.4%; North America cattle adult female
#'   615 -> 700 kg, +7.3%; Global sheep adult female 45 -> 53 kg, +7.2%.
#'   GLEAM also publishes no live weight for the Replacement cohorts (its
#'   herd module derives them), so a re-ingest cannot remove every assumption.
#'   Separately, the `"Latin America"` rows are unreachable: `.gleam_region_of()`
#'   emits GLEAM 3.0 region labels, in which that region is
#'   `"Central & South America"`, so those rows silently take the Global
#'   weights.
#'
#' @examples
#' gleam_animal_weights
"gleam_animal_weights"

#' GLEAM milk production.
#'
#' @description
#' Average annual milk yields and lactation lengths by region.
#'
#' @format A tibble with `region`, `species`, `system`,
#'   `milk_kg_head_yr`, `lactation_days`.
#'
#' @source Not traced to any GLEAM document, and GLEAM appears to publish no
#'   such table (whep#881). Searched and ruled out: the GLEAM 3.0 Supplement
#'   S1 workbook; the herd-parameter tables of Supplement S1 of the
#'   Version 2.0 Revision 5 description (Tables 2.4-2.21 give live weights and
#'   replacement, fertility, mortality and age-at-first-calving rates, but no
#'   milk yield and no lactation length); and both model descriptions, in
#'   which `MILKyield` (Equation 9.1) and the lactation period are
#'   country-level inputs taken from FAOSTAT and national data rather than
#'   defaults -- Table 1.2 of the Version 2.0 description lists them under
#'   national/sub-national resolution. GLEAM-i likewise asks the user for
#'   "annual average milk yield per milking cow". Treat the values as
#'   unverified placeholders. No function in `R/` reads this object: the Tier
#'   2 lactation energy term takes `milk_yield_kg_day` from the caller, and
#'   `.build_demand_energy()` derives it from the FAOSTAT `t_head` rows.
#'
#' @examples
#' gleam_milk_production
"gleam_milk_production"

# IPCC 2019 Refinement Tables ----

#' IPCC 2019 enteric EF for cattle.
#'
#' @description
#' Tier 1 enteric fermentation emission factors for cattle by region
#' (kg CH4/head/yr). Regional cattle factors are Table 10.11 in both
#' the 2006 Guidelines and the 2019 Refinement; Table 10.10 holds the
#' non-cattle species (see [ipcc_2019_enteric_ef_other]).
#'
#' @format A tibble with `region`, `category`, `ef_kg_head_yr`.
#'
#' @source Predominantly the 2006 Guidelines, Vol 4, Ch 10, Table 10.11,
#'   not the 2019 Refinement's Table 10.11 (Updated). Verified against both
#'   published tables:
#'   - 2006 values, differing from the 2019 Refinement: North America
#'     128/53 (2019: 138/64), Western Europe 117/57 (126/52), Eastern Europe
#'     99/58 (93/58), Latin America 72/56 (87/56), Asia 68/47 (78/54),
#'     Africa 46/31 (76/52), Middle East other cattle 31 (60).
#'   - Matching neither edition: Oceania dairy 90 (2006: 100;
#'     2019: 93), Middle East dairy 63 (2006 groups Africa and the Middle
#'     East at 46; 2019: 76), Indian Subcontinent 68/47 (2006: 58/27;
#'     2019: 73/46).
#'   - The `"Global"` fallback row (80/47) appears in no IPCC table in
#'     either edition. **Assumed, unverified.**
#'   The 2019 Refinement also moved buffalo into this table (78 Western
#'   Europe, 68 Eastern Europe / Latin America / Asia, 81 Africa, 67 Middle
#'   East, 85 Indian Subcontinent), which is not reflected here.
#'   Tracked in whep#601.
#'
#' @examples
#' ipcc_2019_enteric_ef_cattle
"ipcc_2019_enteric_ef_cattle"

#' IPCC 2019 enteric EF for non-cattle.
#'
#' @description
#' Tier 1 enteric fermentation emission factors for non-cattle species
#' (kg CH4/head/yr). Non-cattle species are Table 10.10 in both the
#' 2006 Guidelines and the 2019 Refinement; regional cattle factors are
#' Table 10.11 (see [ipcc_2019_enteric_ef_cattle]).
#'
#' @format A tibble with `category`, `ef_kg_head_yr`.
#'
#' @source The 2006 Guidelines, Vol 4, Ch 10, Table 10.10,
#'   developed-countries column (buffalo 55, sheep 8, goats 5, camels 46,
#'   horses 18, mules and asses 10, swine 1.5), not the 2019 Refinement.
#'   The Refinement's Table 10.10 (Updated) splits every ruminant and swine
#'   factor by productivity system (sheep 9 high / 5 low, goats 9 / 5,
#'   swine 1.5 / 1.0), leaves camels, horses and mules unchanged, and moves
#'   buffalo out of this table into the regional Table 10.11. Poultry is
#'   stored as `0`; both editions say "insufficient data for calculation",
#'   so the zero is a project choice rather than a published factor.
#'   Tracked in whep#601.
#'
#' @examples
#' ipcc_2019_enteric_ef_other
"ipcc_2019_enteric_ef_other"

#' IPCC 2019 manure CH4 EF for cattle.
#'
#' @description
#' Tier 1 manure management CH4 emission factors for cattle by region
#' (kg CH4/head/yr).
#'
#' @format A tibble with `region`, `category`,
#'   `ef_kg_head_yr`.
#'
#' @source Not the 2019 Refinement. Its Table 10.14 (Updated) publishes
#'   manure CH4 per kilogram of volatile solids (g CH4 kg VS-1), by
#'   productivity class and ten climate zones; the Refinement contains no
#'   per-head Tier 1 manure CH4 table at all (its only per-head CH4 tables
#'   are the enteric Tables 10.10/10.11 and Table 10.15 for deer, reindeer,
#'   rabbits, ostrich and fur-bearing animals). The per-head quantity stored
#'   here is the form of the 2006 Guidelines Table 10.14, but the values do
#'   not match it either (North American dairy cattle 27/42/60 for
#'   cool/temperate/warm against 48/78/112 in 2006; Latin American dairy
#'   cattle 47 against 2). **The provenance of these values is unknown and
#'   unverified**; tracked in whep#601.
#'
#' @examples
#' ipcc_2019_manure_ch4_ef_cattle
"ipcc_2019_manure_ch4_ef_cattle"

#' IPCC 2019 manure CH4 EF for non-cattle.
#'
#' @description
#' Tier 1 manure management CH4 emission factors for non-cattle species
#' (kg CH4/head/yr).
#'
#' @format A tibble with `category`, `ef_kg_head_yr`.
#'
#' @source 2006 Guidelines, Vol 4, Ch 10, Tables 10.14 (buffalo, swine) and
#'   10.15 (sheep, goats, poultry, horses, mules and asses, camels), not the
#'   2019 Refinement, which publishes no per-head Tier 1 manure CH4 table.
#'   The temperature column each value is taken from varies by species
#'   (sheep 0.19 and goats 0.13 are the developed-country cool column, while
#'   horses 1.64, mules 0.90 and camels 1.92 are the developing-country
#'   temperate column); tracked in whep#601.
#'
#' @examples
#' ipcc_2019_manure_ch4_ef_other
"ipcc_2019_manure_ch4_ef_other"

#' IPCC 2019 MCF for manure management.
#'
#' @description
#' Methane conversion factors (percent) by manure management system and
#' climate zone, using `"All"` for the systems that take a single factor.
#'
#' @format A tibble with `system`, `climate_zone`, `mcf_percent`.
#'
#' @source Predominantly the 2006 Guidelines, Vol 4, Ch 10, Table 10.17,
#'   whose cool/temperate/warm structure this table follows. The 2019
#'   Refinement's Table 10.17 (Updated) is resolved by ten climate zones and
#'   by liquid-system retention time instead, and differs in level: it gives
#'   a single 0.47 percent for pasture/range/paddock against 1.0/1.5/2.0
#'   here, and 1.0/2.0/2.5 for static-pile and passive-windrow composting
#'   against 0.5/0.5/0.5 and 1.0/1.0/1.5 here. Some cells match neither
#'   edition: dry lot 1.5/2.5/4.0 (both editions give 1.0/1.5/2.0),
#'   intensive-windrow composting 0.5/0.5/0.5 (both give 0.5/1.0/1.5) and
#'   pit storage under one month 3/3/5 (2006 gives 3/3/30). Where a 2006 row
#'   is resolved per degree Celsius the value taken is not always the
#'   mid-point of the class (uncovered anaerobic lagoon temperate 73 percent
#'   is the 14 degree column, not the 78 percent of 20 degrees);
#'   tracked in whep#601.
#'
#' @examples
#' ipcc_2019_mcf_manure
"ipcc_2019_mcf_manure"

#' IPCC 2019 nitrogen excretion rates.
#'
#' @description
#' Default nitrogen excretion by animal category and region, stored as
#' annual excretion per head (kg N/head/yr). That is the form the Tier 1
#' manure N2O path consumes, and the same quantity the Tier 2 path
#' derives from the energy balance.
#'
#' @format A tibble with `region`, `category`, `nex_kg_n_head_yr`.
#'
#' @source Unverified. IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.19
#'   (Updated) publishes the excretion *rate* per 1000 kg animal mass per
#'   day, not an annual per-head amount, and the values stored here do not
#'   follow from it. The Refinement does supply the missing conversion
#'   factor: Table 10A.1 (New) gives the regional typical weight of dairy
#'   cattle, so rate x weight x 365 is derivable and gives 140 kg N/head/yr
#'   for North America (0.59 x 650 kg x 365 / 1000) against the 105 stored,
#'   118 for Western Europe (100 stored), 84 for Eastern Europe (80), 128
#'   for Oceania (80), 72 for Latin America (50), 62 for Asia (50), 42 for
#'   Africa (40), 64 for the Middle East (40) and 68 for the Indian
#'   Subcontinent (50). Other cattle would need the cohort population mix of
#'   Table 10A.2 (New) to be weighted the same way. Tracked in whep#601.
#'
#' @examples
#' ipcc_2019_n_excretion
"ipcc_2019_n_excretion"

#' IPCC 2019 direct N2O emission factors.
#'
#' @description
#' Table 10.21: EF3 values (kg N2O-N per kg N excreted) by manure
#' management system.
#'
#' @format A tibble with `system`, `ef_kg_n2o_n_per_kg_n`.
#'
#' @source Mixed, and not consistently the 2019 Refinement's Table 10.21.
#'   Verified against both editions of Vol 4, Ch 10, Table 10.21:
#'   - Matching both editions: liquid/slurry with crust 0.005, in-vessel
#'     composting 0.006, poultry with and without litter 0.001.
#'   - Matching the 2006 Guidelines but not the 2019 Refinement: solid
#'     storage 0.005 (2019: 0.010), static-pile composting 0.006
#'     (2019: 0.010), passive-windrow composting 0.01 (2019: 0.005),
#'     anaerobic digester 0 (2019: 0.0006).
#'   - Matching neither edition: daily spread 0.01 and liquid/slurry
#'     without crust 0.002 and uncovered anaerobic lagoon 0.001 (all three
#'     are 0 in both editions), dry lot 0.005 (0.02 in both),
#'     intensive-windrow composting 0.006 (2019: 0.005; 2006: 0.1).
#'   Pasture/range/paddock is not in Table 10.21 in either edition, which
#'   defers it to Ch 11. Its stored 0.01 is the 2006 Ch 11 Table 11.1
#'   EF3PRP,SO for sheep and other animals; the 2019 Refinement's Table 11.1
#'   (Updated) gives 0.004 for cattle, poultry and pigs and 0.003 for sheep
#'   and other animals. Tracked in whep#601.
#'
#' @examples
#' ipcc_2019_n2o_ef_direct
"ipcc_2019_n2o_ef_direct"

#' IPCC Ym values.
#'
#' @description
#' Methane conversion rate (% GE) by species and feed situation.
#' - Cattle feedlot (>90% concentrate): 3.0%.
#' - Sheep: a single 6.7%, irrespective of feed quality (no
#'   body-weight split).
#'
#' @format A tibble with `category`, `feed_situation`,
#'   `ym_percent`.
#'
#' @source Mixed across editions.
#'   - Sheep 6.7% and goats 5.5% are the 2019 Refinement, Vol 4, Ch 10,
#'     Table 10.13 (Updated).
#'   - Cattle and buffalo 6.5% on pasture/range and mixed rations are the
#'     2006 Guidelines Table 10.12, which gives 6.5% for every non-feedlot
#'     cattle and buffalo class. The 2019 Refinement's Table 10.12 (Updated)
#'     resolves cattle and buffalo Ym by production level and feed
#'     digestibility instead: 5.7 / 6.0 / 6.3 / 6.5 for dairy cows by yield
#'     class, 7.0 for >75 percent forage non-dairy, 6.3 for mixed rations,
#'     4.0 for grain feedlots and 3.0 for steam-flaked-corn feedlots. The
#'     stored feedlot 3.0% is therefore the 2006 ">=90 percent concentrate"
#'     value, which in the 2019 Refinement applies only to the
#'     steam-flaked-corn case.
#'   - Camels 5.0% appears in no IPCC table. Both editions instead direct
#'     compilers to reuse the other-cattle or buffalo Ym for camels, which
#'     would be 6.5%. **Assumed, unverified.**
#'   Tracked in whep#601.
#'
#' @examples
#' ipcc_2019_ym
"ipcc_2019_ym"

#' IPCC 2019 Bo values (Table 10.16A).
#'
#' @description
#' Maximum CH4 producing capacity of manure
#' (m3 CH4/kg VS). Dairy cattle (0.24) differs from
#' other cattle (0.18).
#'
#' @format A tibble with `category`, `bo_m3_kg_vs`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16A (Updated) --
#'   Table 10.16 in that edition is the manure CH4 factors for deer and
#'   similar species. Every row matches its high-productivity column except
#'   `"Swine - Breeding"` 0.27: Table 10.16A publishes one swine Bo
#'   (0.48 North America, 0.45 other high-productivity regions, 0.29 low
#'   productivity) and the 2006 Annex 10A.2 derivation tables give breeding
#'   swine the same Bo as market swine, so 0.27 appears in neither edition;
#'   it coincides with the North American *market swine volatile-solids
#'   rate* of 0.27 kg VS head-1 day-1 in 2006 Annex 10A.2. `"Other Cattle"`
#'   0.18 is the Western European non-dairy column (North America is 0.19,
#'   Eastern Europe and Oceania 0.17). Tracked in whep#601.
#'
#' @examples
#' ipcc_2019_bo
"ipcc_2019_bo"

#' IPCC 2019 Cfi values (Table 10.4).
#'
#' @description
#' Net energy maintenance coefficients (MJ/day/kg^0.75).
#' Dairy (lactating) cattle use 0.386; non-dairy 0.322.
#'
#' @format A tibble with `category`, `subcategory`,
#'   `cfi_mj_day_kg075`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.4 (Updated), which
#'   repeats the 2006 Guidelines values and adds the goat row (0.315). Two
#'   rows published in both editions are absent here, and the
#'   `"Non-lactating/Bulls"` row conflates them: intact bulls take 0.370,
#'   not the 0.322 of non-lactating cows, steers and juveniles, and lambs
#'   under one year take 0.236 rather than the 0.217 of mature sheep.
#'   Tracked in whep#601.
#'
#' @examples
#' ipcc_2019_cfi
"ipcc_2019_cfi"

# IPCC 2006 Tables ----

#' IPCC 2006 Tier 1 enteric emission factors.
#'
#' @description
#' Table 10.11 (2006): Tier 1 regional EFs for enteric
#' fermentation, with the non-cattle species of Table 10.10 appended
#' under a `"Global"` region.
#'
#' @format A tibble with `region`, `category`,
#'   `ef_kg_head_yr`.
#'
#' @source IPCC 2006, Vol 4, Ch 10, Table 10.11 for cattle and Table 10.10
#'   (developed-countries column) for the non-cattle rows. Two departures
#'   from the published table: Oceania dairy cattle is stored as 90 where
#'   Table 10.11 gives 100, and the published table groups Africa **and**
#'   the Middle East in one row (46 dairy / 31 other) which is repeated here
#'   as two regions. The Indian Subcontinent row of Table 10.11
#'   (58 dairy / 27 other) is absent. Tracked in whep#601.
#'
#' @examples
#' ipcc_2006_enteric_ef
"ipcc_2006_enteric_ef"

#' IPCC 2006 Tier 1 manure emission factors.
#'
#' @description
#' Table 10.14 (2006): Tier 1 regional EFs for manure CH4.
#'
#' @format A tibble with `region`, `category`,
#'   `ef_kg_head_yr`, `temp_zone`.
#'
#' @source IPCC 2006, Vol 4, Ch 10, Table 10.14 for cattle, swine and
#'   buffalo and Table 10.15 for sheep, goats and poultry. Table 10.14 is
#'   resolved per degree Celsius, and the value taken for a `temp_zone` is
#'   not always the bound of that class, nor always present in the row:
#'   North American dairy cows 53 is the 12 degree column rather than the 48
#'   of the cool class, Asian dairy cows 16 is the 18 degree column rather
#'   than the 31 of the warm class, Latin American dairy cows 1 is the cool
#'   value where the warm class gives 2, and Western European dairy cows 20
#'   appears in no column of that row (its cool value is 21).
#'   Tracked in whep#601.
#'
#' @examples
#' ipcc_2006_manure_ef
"ipcc_2006_manure_ef"

#' IPCC 2006 MCF by temperature.
#'
#' @description
#' Table 10.17 (2006): MCF values by MMS type and
#' annual temperature.
#'
#' @format A tibble with `system`, `temp_c`, `mcf_percent`.
#'
#' @source IPCC 2006, Vol 4, Ch 10, Table 10.17.
#'
#' @examples
#' ipcc_2006_mcf_temp
"ipcc_2006_mcf_temp"

# IPCC Tier 2 Parameters ----

#' Tier 2 energy coefficients.
#'
#' @description
#' Coefficients for IPCC Tier 2 GE calculation including
#' Cfi (maintenance), Ca (activity), Cp (pregnancy),
#' Cw (work), and energy content of weight gain. Now
#' includes `subcategory` column to differentiate dairy
#' (lactating) vs non-dairy cattle.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{category}{Species (Cattle, Buffalo, Sheep, etc.).}
#'   \item{subcategory}{Dairy, Non-Dairy, or All.}
#'   \item{cfi_mj_day_kg075}{NEm coefficient (MJ/day/kg^0.75).}
#'   \item{ca_pasture}{Activity coefficient for grazing.}
#'   \item{ca_feedlot}{Activity coefficient for confined.}
#'   \item{cp}{Pregnancy coefficient.}
#'   \item{cw}{Work coefficient.}
#'   \item{energy_content_gain_mj_kg}{Energy per kg gain.}
#' }
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Eq 10.3-10.16, with
#'   `cfi_mj_day_kg075` from Table 10.4 (Updated) and `ca_pasture` from
#'   Table 10.5 (Updated). Sheep and goats take their own rows in both
#'   tables and must not be conflated: Cfi is 0.217 for sheep older than
#'   one year and 0.315 for goats, and Ca is 0.0107 for sheep grazing flat
#'   pasture and 0.019 for lowland goats. Neither goat value exists in the
#'   2006 Guidelines, whose Table 10.4 has no goat row at all. `ca_pasture`
#'   takes the lowland/flat-pasture value as the global default; the
#'   published hill and mountain goat coefficient (0.024) is not stored and
#'   must be supplied per row by the caller.
#'
#' @examples
#' ipcc_tier2_energy_coefs
"ipcc_tier2_energy_coefs"

#' Tier 2 Ym values.
#'
#' @description
#' Methane conversion rate by species and feed situation
#' for Tier 2 enteric CH4. Includes feedlot distinction
#' and sheep body weight differentiation.
#'
#' @format A tibble with `category`, `feed_situation`,
#'   `ym_percent`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.
#'
#' @examples
#' ipcc_tier2_ym_values
"ipcc_tier2_ym_values"

#' Tier 2 Bo values.
#'
#' @description
#' Maximum CH4 producing capacity by detailed category.
#' Dairy cattle 0.24 vs other cattle 0.18.
#'
#' @format A tibble with `category`, `bo_m3_kg_vs`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16.
#'
#' @examples
#' ipcc_tier2_bo_values
"ipcc_tier2_bo_values"

#' Tier 2 manure ash content.
#'
#' @description
#' Ash content of manure as percent of dry matter,
#' used in VS calculation (Eq 10.24).
#'
#' @format A tibble with `category`, `ash_percent`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10.
#'
#' @examples
#' ipcc_tier2_manure_ash
"ipcc_tier2_manure_ash"

#' Tier 2 nitrogen retention fractions.
#'
#' @description
#' Fraction of N intake retained in animal products.
#' Dairy cattle 0.20 vs other cattle 0.07.
#'
#' @format A tibble with `category`, `n_retention_frac`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.20.
#'
#' @examples
#' ipcc_tier2_n_retention
"ipcc_tier2_n_retention"

#' Default production parameters.
#'
#' @description
#' Default values for fat%, protein%, lactose%, weight gain,
#' work hours, and pregnancy fraction by species.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{category}{Species or animal class.}
#'   \item{fat_percent}{Milk fat content (percent).}
#'   \item{protein_percent}{Milk protein content (percent).}
#'   \item{lactose_percent}{Milk lactose content (percent).}
#'   \item{weight_gain_kg_day}{Average daily weight gain
#'     (kg/day).}
#'   \item{work_hours_day}{Hours of draft work per day.}
#'   \item{pregnant_fraction}{Fraction of females pregnant.}
#' }
#'
#' @source NRC 2001; IPCC 2019, Vol 4, Ch 10.
#'
#' @examples
#' livestock_production_defaults
"livestock_production_defaults"

#' Feed characteristics by diet quality.
#'
#' @description
#' DE%, NDF%, GE content, and crude protein percentage
#' for High/Medium/Low diet quality levels.
#'
#' @format A tibble with `diet_quality`, `de_percent`,
#'   `ndf_percent`, `ge_content_mj_kg_dm`, `cp_percent`.
#'
#' @source IPCC 2019, Vol 4, Ch 10.
#'
#' @examples
#' feed_characteristics
"feed_characteristics"

#' Climate-zone MCF values.
#'
#' @description
#' Methane Conversion Factors by MMS type and climate zone
#' (Cool/Temperate/Warm).
#'
#' @format A tibble with `mms_type`, `climate_zone`,
#'   `mcf_percent`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.17.
#'
#' @examples
#' climate_mcf
"climate_mcf"

#' Regional MMS distribution.
#'
#' @description
#' Fraction of manure managed in each MMS type by region
#' and species.
#'
#' @format A tibble with `region`, `species`, `mms_type`,
#'   `fraction`.
#'
#' @source Unverified. The values are a hardcoded `tribble()` in
#'   `generate_ipcc_tier2_params()` in `data-raw/livestock_coefficients.R`,
#'   annotated there "GLEAM 3.0 / FAO statistics (simplified)"; they are round
#'   to the nearest 5 percentage points and match no table of the committed
#'   GLEAM 3.0 Supplement S1 workbook, which carries no MMS shares. Unlike
#'   `gleam_mms_shares` this object **is** result-affecting: it is the MMS
#'   split `.resolve_mms_shares()` hands to the Tier 2 manure CH4 (MCF
#'   weighting) and direct N2O (EF3) engines. The published GLEAM source is
#'   Supplement S1, Tables 4.2-4.11 of FAO. 2018. *GLEAM Model description,
#'   Version 2.0, Revision 5* (see `gleam_mms_shares`); re-ingesting it moves
#'   manure numbers and is tracked separately. Treat as unverified until then.
#'
#' @examples
#' regional_mms_distribution
"regional_mms_distribution"

#' Temperature adjustment factors for NEm.
#'
#' @description
#' Adjustment multipliers for net energy maintenance
#' under cold stress, thermoneutral, and heat stress
#' conditions.
#'
#' @format A tibble with `temp_range`, `temp_min`,
#'   `temp_max`, `adjustment_factor`.
#'
#' @source NRC 2001; IPCC 2019.
#'
#' @examples
#' temperature_adjustment
"temperature_adjustment"

#' Indirect N2O emission factors.
#'
#' @description
#' Parameters for indirect N2O emissions from manure
#' management: EF4 (volatilization), EF5 (leaching),
#' FracGasMS, FracLeach.
#'
#' @format A tibble with `parameter`, `value`,
#'   `description`.
#'
#' @source IPCC 2019, Vol 4, Ch 10, Table 10.22;
#'   Vol 4, Ch 11, Table 11.3.
#'
#' @examples
#' indirect_n2o_ef
"indirect_n2o_ef"

#' Uncertainty ranges for emission parameters.
#'
#' @description
#' Lower and upper multipliers for key emission
#' parameters (Ym, MCF, Bo, EF_N2O, Nex).
#'
#' @format A tibble with `parameter`, `lower_mult`,
#'   `upper_mult`, `distribution`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10.
#'
#' @examples
#' uncertainty_ranges
"uncertainty_ranges"

#' Grazing energy coefficients.
#'
#' @description
#' Walking energy cost for grazing animals
#' (MJ/kg body weight/km).
#'
#' @format A tibble with `parameter`, `value_mj_kg_km`,
#'   `source`.
#'
#' @source NRC 2001 (0.00045 Mcal/kg/km converted to MJ).
#'
#' @examples
#' grazing_energy_coefs
"grazing_energy_coefs"

#' Livestock physical constants.
#'
#' @description
#' Named list of physical constants used in livestock
#' emission calculations:
#' - `energy_content_ch4_mj_kg`: 55.65 MJ/kg CH4.
#' - `ch4_density_kg_m3`: 0.67 kg/m3.
#' - `vs_energy_content_mj_kg`: 18.45 MJ/kg DM.
#' - `n_to_n2o`: 44/28 (N to N2O molecular mass ratio).
#' - `days_in_year`: 365.
#' - `default_de_percent`: 65%.
#' - `default_ue_fraction`: 0.04 (urinary energy as fraction of GE).
#' - `ev_wool_mj_kg`: 24.0 MJ/kg clean wool.
#'
#' @format A named list.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10.
#'
#' @examples
#' str(livestock_constants)
"livestock_constants"
