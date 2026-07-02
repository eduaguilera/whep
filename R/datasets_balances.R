# Soil-balance Coefficient Tables Documentation
#
# Roxygen2 documentation for the soil-organic-carbon turnover and
# soil carbon-to-nitrogen coefficient datasets (Module B, Task B1),
# stored in data/soc_turnover_params.rda, data/amg_h_by_input_type.rda
# and data/soil_cn_ratios.rda.

#' Soil organic carbon turnover parameters by model.
#'
#' @description
#' Per-model structural parameters for the five soil organic carbon
#' turnover models ported in Module B: HSOC (Spain two-pool plus inert
#' organic matter), RothC (five pools), ICBM (two pools), AMG (active
#' plus stable), and Century (five pools). Stored in long form so a
#' builder can assemble each model's pool rate constants, initial pool
#' fractions, inter-pool transfer fractions, and texture/lignin
#' defaults. Climate rate modifiers are model-native and live in the
#' turnover functions, not in this table.
#'
#' @format A tibble in long form with columns:
#' \describe{
#'   \item{model}{Model namespace: one of \code{"hsoc"}, \code{"rothc"},
#'     \code{"icbm"}, \code{"amg"}, \code{"century"}.}
#'   \item{component}{Pool or structural component the parameter belongs
#'     to (e.g. \code{"fresh"}, \code{"dpm"}, \code{"young"},
#'     \code{"active"}, \code{"str"}, \code{"defaults"}).}
#'   \item{parameter}{Parameter name (e.g.
#'     \code{"decomposition_rate"}, \code{"init_active_fraction"},
#'     \code{"transfer_fraction"}, \code{"base_rate_weekly"}).}
#'   \item{value}{Numeric parameter value.}
#'   \item{unit}{Unit of the value (e.g. \code{"per_year"},
#'     \code{"fraction"}, \code{"per_week"}).}
#'   \item{description}{Human-readable description of the parameter.}
#' }
#'
#' @source RothC: Coleman, K. & Jenkinson, D. S. (1996). RothC-26.3: A
#'   model for the turnover of carbon in soil. In D. S. Powlson et al.
#'   (Eds.), *Evaluation of Soil Organic Matter Models* (NATO ASI
#'   Series I, Vol. 38, pp. 237-246). Springer.
#'   \doi{10.1007/978-3-642-61094-3_17}.
#'   ICBM: Andren, O. & Katterer, T. (1997). ICBM: The introductory
#'   carbon balance model for exploration of soil carbon balances.
#'   *Ecological Applications*, 7(4), 1226-1236.
#'   \doi{10.1890/1051-0761(1997)007[1226:ITICBM]2.0.CO;2}.
#'   AMG: Saffih-Hdadi, K. & Mary, B. (2008). Modeling consequences of
#'   straw residues export on soil organic carbon. *Soil Biology and
#'   Biochemistry*, 40(3), 594-607.
#'   \doi{10.1016/j.soilbio.2007.08.022}.
#'   Century: Parton, W. J., Schimel, D. S., Cole, C. V. & Ojima, D. S.
#'   (1987). Analysis of factors controlling soil organic matter levels
#'   in Great Plains grasslands. *Soil Science Society of America
#'   Journal*, 51(5), 1173-1179.
#'   \doi{10.2136/sssaj1987.03615995005100050015x}; SoilR Century
#'   implementation: Sierra, C. A., Mueller, M. & Trumbore, S. E.
#'   (2012). Models of soil organic matter decomposition: the SoilR
#'   package, version 1.0. *Geoscientific Model Development*, 5,
#'   1045-1060. \doi{10.5194/gmd-5-1045-2012}.
#'   Inert organic matter initialisation (RothC, HSOC): Falloon, P.,
#'   Smith, P., Coleman, K. & Marshall, S. (1998). Estimating the size
#'   of the inert organic matter pool from total soil organic carbon
#'   content for use in the Rothamsted carbon model. *Soil Biology and
#'   Biochemistry*, 30(8-9), 1207-1211.
#'   \doi{10.1016/S0038-0717(97)00256-3}.
#'
#' @examples
#' soc_turnover_params
"soc_turnover_params"

#' AMG humification coefficient by carbon input type.
#'
#' @description
#' Lookup table giving the AMG humification coefficient \code{h} (the
#' fraction of carbon inputs entering the active pool) as a function of
#' the carbon input type. Rows are matched in ascending
#' \code{match_order} against the lowercased input type using the
#' \code{pattern} regular expression; the last row (input type
#' \code{"default"}, with a missing pattern) is the fallthrough.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{match_order}{Integer matching priority (lower is tried
#'     first).}
#'   \item{input_type}{Canonical input type label (e.g.
#'     \code{"green_manure"}, \code{"manure"}, \code{"residue"},
#'     \code{"default"}).}
#'   \item{pattern}{Regular expression matched against the lowercased
#'     input type; \code{NA} for the fallthrough row.}
#'   \item{h}{Humification coefficient (fraction of input carbon
#'     stabilised).}
#' }
#'
#' @source Saffih-Hdadi, K. & Mary, B. (2008). Modeling consequences of
#'   straw residues export on soil organic carbon. *Soil Biology and
#'   Biochemistry*, 40(3), 594-607.
#'   \doi{10.1016/j.soilbio.2007.08.022}.
#'
#' @examples
#' amg_h_by_input_type
"amg_h_by_input_type"

#' Soil carbon-to-nitrogen ratios for organic-matter balances.
#'
#' @description
#' Soil carbon-to-nitrogen ratios used to convert a soil organic carbon
#' stock change into net nitrogen mineralization (when carbon is lost)
#' or net nitrogen sequestration (when carbon accumulates), by cropland
#' class and management system.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{cropland_class}{Land class: \code{"Cropland"} or
#'     \code{"NonCropland"}.}
#'   \item{management}{Management system: \code{"Conventional"} or
#'     \code{"Organic"}.}
#'   \item{cn_ratio}{Soil organic-matter carbon-to-nitrogen ratio.}
#'   \item{cn_mineralization}{Carbon-to-nitrogen ratio applied when soil
#'     organic carbon is mineralized (net carbon loss).}
#'   \item{cn_sequestration}{Carbon-to-nitrogen ratio applied when soil
#'     organic carbon is sequestered (net carbon gain).}
#' }
#'
#' @source Soil carbon-to-nitrogen ratios from the Spain historical SOC
#'   pipeline coefficient set, consistent with the RothC framework of
#'   Coleman, K. & Jenkinson, D. S. (1996).
#'   \doi{10.1007/978-3-642-61094-3_17}.
#'
#' @examples
#' soil_cn_ratios
"soil_cn_ratios"

#' Humification fraction by carbon input type.
#'
#' @description
#' Per-input-type fraction of soil carbon input that is stabilised
#' directly into humus (rather than entering the labile, fresh pool).
#' The HSOC turnover model takes a scalar humification fraction; this
#' table supplies the type-specific values so a carbon-input builder can
#' compute a carbon-weighted effective humification fraction per
#' cell-year before running the SOC model. Other models (RothC, ICBM,
#' AMG, Century) use their native carbon partition and ignore this table;
#' AMG instead uses \code{amg_h_by_input_type}. Values are transcribed
#' from the Spain historical agroecosystem \code{Biomass_coefs}
#' \code{Residue_humified_kgC_kgC} column (manure and roots from the
#' corresponding manure and \code{Root_humified_kgC_kgC} entries).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{input_type}{Canonical carbon input type:
#'     \code{"crop_residue"}, \code{"root"}, \code{"weed"},
#'     \code{"woody_residue"}, \code{"manure"}, \code{"excreta"},
#'     \code{"urban"}, \code{"urban_compost"}, \code{"compost"},
#'     \code{"green_manure"}.}
#'   \item{humified_fraction}{Fraction of input carbon stabilised
#'     directly into humus (kg humified carbon per kg carbon input).}
#'   \item{description}{Human-readable description of the input type and
#'     its Spain historical provenance.}
#' }
#'
#' @source Spain historical agroecosystem coefficient set
#'   (\code{Biomass_coefs.xlsx}, \code{Residue_humified_kgC_kgC} and
#'   \code{Root_humified_kgC_kgC} columns), itself compiled from soil
#'   organic carbon turnover and litter humification studies including
#'   Andren, O. & Katterer, T. (1997).
#'   \doi{10.1890/1051-0761(1997)007[1226:ITICBM]2.0.CO;2}; Katterer, T.,
#'   Bolinder, M. A., Andren, O., Kirchmann, H. & Menichetti, L. (2011).
#'   Roots contribute more to refractory soil organic matter than above-
#'   ground crop residues, as revealed by a long-term field experiment.
#'   *Agriculture, Ecosystems & Environment*, 141(1-2), 184-192.
#'   \doi{10.1016/j.agee.2011.02.029}.
#'
#' @examples
#' residue_humification
"residue_humification"

#' Disaggregated direct soil N2O emission factors by climate and irrigation.
#'
#' @description
#' Direct nitrous-oxide emission factors (fraction of applied nitrogen
#' emitted as N2O-N) disaggregated by climate zone and irrigation type.
#' Atlantic strata use the IPCC Tier 1 default of 0.01 (row
#' \code{irrig_type == "Tier_1"}); Mediterranean strata use the
#' Cayuela et al. (2017) meta-analytic factors that resolve the strong
#' effect of water management on N2O. Atlantic non-flooded irrigation
#' rows carry a missing factor because Atlantic strata are routed to the
#' \code{"Tier_1"} factor; the Atlantic flooded row keeps its own value.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{irrig_type}{Irrigation / management stratum: one of
#'     \code{"Tier_1"}, \code{"Rainfed"}, \code{"Traditional"},
#'     \code{"Drip"}, \code{"Sprinkler"}, \code{"Flooded"},
#'     \code{"Med_average"}.}
#'   \item{climate}{Climate zone: \code{"MED"} (Mediterranean) or
#'     \code{"ATL"} (Atlantic).}
#'   \item{ef}{Direct N2O emission factor (kg N2O-N per kg applied N);
#'     \code{NA} for Atlantic non-flooded irrigation strata.}
#' }
#'
#' @source Cayuela, M. L., Aguilera, E., Sanz-Cobena, A., Adams, D. C.,
#'   Abalos, D., Barton, L., Ryals, R., Silver, W. L., Alfaro, M. A.,
#'   Pappa, V. A., Smith, P., Garnier, J., Billen, G., Bouwman, L.,
#'   Bondeau, A. & Lassaletta, L. (2017). Direct nitrous oxide emissions
#'   in Mediterranean climate cropping systems: emission factors based on
#'   a meta-analysis of available measurement data. *Agriculture,
#'   Ecosystems & Environment*, 238, 25-35.
#'   \doi{10.1016/j.agee.2016.10.006}. Precursor Mediterranean N2O
#'   review: Aguilera, E., Lassaletta, L., Sanz-Cobena, A., Garnier, J.
#'   & Vallejo, A. (2013). The potential of organic fertilizers and water
#'   management to reduce N2O emissions in Mediterranean climate cropping
#'   systems. A review. *Agriculture, Ecosystems & Environment*, 164,
#'   32-52. \doi{10.1016/j.agee.2012.09.006}. Atlantic Tier 1 default:
#'   IPCC (2019), 2019 Refinement to the 2006 IPCC Guidelines for
#'   National Greenhouse Gas Inventories, Vol. 4, Chapter 11.
#'
#' @examples
#' n2o_efs_disaggregated
"n2o_efs_disaggregated"

#' Fertiliser-type modifying factors for direct soil N2O.
#'
#' @description
#' Multiplicative modifying factors applied to the disaggregated direct
#' N2O emission factor by fertiliser type and climate zone. They scale
#' the climate-by-irrigation emission factor up or down according to the
#' nitrogen source. Mediterranean factors follow the Cayuela et al.
#' (2017) meta-analysis for synthetic, solid and liquid sources and the
#' IPCC (2019) dry-area values for excreta; Atlantic factors follow the
#' IPCC (2019) wet-area values. Missing factors mark sources whose N2O
#' is not modelled through this pathway in the Mediterranean (recycled
#' organic fertilisers, soil organic matter, urban N).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{fert_type}{Nitrogen source: one of \code{"Synthetic"},
#'     \code{"Solid"}, \code{"Liquid"}, \code{"Recycling"},
#'     \code{"Excreta_cattle_monog"}, \code{"Excreta_other"},
#'     \code{"SOM"}, \code{"Urban"}.}
#'   \item{climate}{Climate zone: \code{"MED"} or \code{"ATL"}.}
#'   \item{mf}{Multiplicative modifying factor on the N2O emission
#'     factor; \code{NA} where the source is not modelled through this
#'     pathway.}
#'   \item{source}{Provenance note transcribed from the coefficient
#'     workbook (Cayuela et al. 2017 or IPCC 2019).}
#' }
#'
#' @source Cayuela, M. L. et al. (2017). Direct nitrous oxide emissions
#'   in Mediterranean climate cropping systems. *Agriculture, Ecosystems
#'   & Environment*, 238, 25-35. \doi{10.1016/j.agee.2016.10.006}. IPCC
#'   (2019), 2019 Refinement to the 2006 IPCC Guidelines for National
#'   Greenhouse Gas Inventories, Vol. 4, Chapter 11.
#'
#' @examples
#' fertiliser_n2o_modifiers
"fertiliser_n2o_modifiers"

#' Meisinger and Randall topsoil denitrification share matrix.
#'
#' @description
#' Fraction of nitrogen surplus lost through topsoil denitrification, as
#' a function of fertiliser category, soil organic matter content,
#' drainage class and climate. The full matrix is keyed by fertiliser
#' category (synthetic versus manure), tillage regime, soil organic
#' matter class, climate category and drainage class. Where drainage is
#' \code{"None"} (waterlogged), the entire surplus is denitrified
#' (share 1.00).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{fert_cat}{Fertiliser category: \code{"Synthetic"} or
#'     \code{"Manure"}.}
#'   \item{tillage}{Tillage regime: \code{"Tillage"},
#'     \code{"No_tillage"} or \code{"Not_specified"} (manure).}
#'   \item{som_content}{Soil organic matter class: \code{"Low"},
#'     \code{"Medium"} or \code{"High"}.}
#'   \item{climate_cat}{Climate category label used in the source
#'     matrix: \code{"Semiarid"} or \code{"Humid"}.}
#'   \item{drainage_rate}{Drainage class: \code{"Very_high"},
#'     \code{"High"}, \code{"Medium"}, \code{"Low"}, \code{"Very_low"}
#'     or \code{"None"}.}
#'   \item{denit_share}{Fraction of nitrogen surplus denitrified in the
#'     topsoil.}
#'   \item{climate}{Climate zone the row applies to: \code{"MED"} or
#'     \code{"ATL"}.}
#' }
#'
#' @source Meisinger, J. J. & Randall, G. W. (1991). Estimating nitrogen
#'   budgets for soil-crop systems. In R. F. Follett, D. R. Keeney & R.
#'   M. Cruse (Eds.), *Managing Nitrogen for Groundwater Quality and Farm
#'   Profitability* (pp. 85-124). Soil Science Society of America.
#'   \doi{10.2136/1991.managingnitrogen.c5}. Values transcribed from the
#'   Spain historical nitrogen coefficient workbook
#'   (\code{N_coefficients.xlsx}, sheet \code{Denitrification_Meisinger}).
#'
#' @examples
#' meisinger_denitrification
"meisinger_denitrification"

#' Drainage-class bins keyed on annual soil drainage.
#'
#' @description
#' Half-open bins that map an annual soil drainage flux (mm) to a
#' drainage class. A drainage value \code{s} is assigned to the class
#' whose interval satisfies \code{s_min < s <= s_max}. The classes key the
#' Meisinger denitrification matrix and the subsoil NO3 reduction table.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{drainage_rate}{Drainage class: \code{"Very_high"},
#'     \code{"High"}, \code{"Medium"}, \code{"Low"}, \code{"Very_low"}
#'     or \code{"None"}.}
#'   \item{s_min}{Lower bound of the drainage flux interval (mm).}
#'   \item{s_max}{Upper bound of the drainage flux interval (mm).}
#' }
#'
#' @source Spain historical nitrogen coefficient workbook
#'   (\code{N_coefficients.xlsx}, sheet \code{Drainage_ranges}),
#'   companion to the Meisinger & Randall (1991) denitrification matrix.
#'   \doi{10.2136/1991.managingnitrogen.c5}.
#'
#' @examples
#' drainage_ranges
"drainage_ranges"

#' Subsoil nitrate reduction shares by source, climate and irrigation.
#'
#' @description
#' Fraction of leaching nitrate reduced (denitrified) in the subsoil
#' below the rooting zone, by nitrogen source, climate zone and
#' irrigation category. Applied after topsoil denitrification to compute
#' the nitrate that reaches groundwater.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{fert_type}{Nitrogen source: one of \code{"Synthetic"},
#'     \code{"SOM"}, \code{"Deposition"}, \code{"Solid"},
#'     \code{"Excreta_cattle_monog"}, \code{"Excreta_other"},
#'     \code{"Liquid"}, \code{"Urban"}, \code{"BNF"}.}
#'   \item{climate}{Climate zone: \code{"MED"} or \code{"ATL"}.}
#'   \item{irrig_cat}{Irrigation category: \code{"Rainfed"} or
#'     \code{"Irrigated"}.}
#'   \item{no3_red}{Fraction of leaching nitrate reduced in the subsoil.}
#' }
#'
#' @source Spain historical nitrogen coefficient workbook
#'   (\code{N_coefficients.xlsx}, sheet \code{Subsoil_NO3_denitrif}),
#'   parameterised from Mediterranean and Atlantic subsoil
#'   denitrification literature consistent with the IPCC (2019) indirect
#'   N2O framework.
#'
#' @examples
#' subsoil_no3_reduction
"subsoil_no3_reduction"

#' MANNER process-based ammonia-volatilisation factors.
#'
#' @description
#' Long-form lookup of the discrete factor tables of the MANNER ammonia
#' volatilisation model. Each row gives one factor, identified by its
#' table (\code{category}), the lookup key (\code{key}) and, where the
#' table is two-dimensional, a second key (\code{sub_key}). The tables
#' are: the per-fertiliser maximum ammonia-loss ceiling
#' (\code{max_nh3}); the soil-pH factor for synthetic fertiliser
#' (\code{ph}, sub-keyed by pH band); the incorporation / land-use factor
#' (\code{incorporation}); the manure application-technique factor
#' (\code{technique}); the wind-speed factor (\code{windspeed}); and the
#' combined rainfall-by-wetness factor (\code{rainfall_wet}). Continuous
#' fertiliser-rate, temperature and rainfall response surfaces are
#' computed in the MANNER function and are not part of this table.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{category}{Factor table: one of \code{"max_nh3"}, \code{"ph"},
#'     \code{"incorporation"}, \code{"technique"}, \code{"windspeed"},
#'     \code{"rainfall_wet"}.}
#'   \item{key}{Primary lookup key (fertiliser, technique, wind-speed
#'     class, or rainfall-wetness class).}
#'   \item{sub_key}{Secondary lookup key (pH band for the \code{ph}
#'     table); \code{NA} for one-dimensional tables.}
#'   \item{factor}{Numeric multiplicative factor.}
#' }
#'
#' @source Nicholson, F. A., Bhogal, A., Chadwick, D., Gill, E., Gooday,
#'   R. D., Lord, E., Misselbrook, T., Rollett, A. J., Sagoo, E., Smith,
#'   K. A., Thorman, R. E., Williams, J. R. & Chambers, B. J. (2013). An
#'   enhanced software tool to support better use of manure nutrients:
#'   MANNER-NPK. *Soil Use and Management*, 29(4), 473-484.
#'   \doi{10.1111/sum.12078}. Underlying mass-flow ammonia framework:
#'   Webb, J. & Misselbrook, T. H. (2004). A mass-flow model of ammonia
#'   emissions from UK livestock production. *Atmospheric Environment*,
#'   38(14), 2163-2176. \doi{10.1016/j.atmosenv.2004.01.023}. Values
#'   transcribed from the Spain historical MANNER implementation
#'   (\code{MANNER_model.R}).
#'
#' @examples
#' manner_params
"manner_params"

#' Nitrogen leaching-attenuation and indirect-N2O constants.
#'
#' @description
#' Scalar constants for the nitrogen-loss cascade: the input C:N leaching
#' attenuation parameters (\code{a_cn_*}) and the IPCC indirect-emission
#' factors. The C:N attenuation reduces nitrate leaching for
#' carbon-rich, low-availability inputs; the indirect factors convert
#' volatilised ammonia and leached nitrate into indirect N2O and set the
#' ammonia-volatilisation fractions.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{constant}{Constant name: \code{"a_cn_min_cn"},
#'     \code{"a_cn_span"}, \code{"a_cn_span_other"}, \code{"a_cn_max"},
#'     \code{"ef5_no3_to_n2o"}, \code{"ef4_nh3_to_n2o_atl"},
#'     \code{"nh3_frac_synthetic"}, \code{"nh3_frac_organic"}.}
#'   \item{value}{Numeric value of the constant.}
#'   \item{description}{Human-readable description of the constant.}
#' }
#'
#' @source C:N attenuation parameters from the Spain historical nitrogen
#'   pipeline (\code{n_fun.r}). Indirect emission factors and ammonia
#'   volatilisation fractions: IPCC (2019), 2019 Refinement to the 2006
#'   IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 4,
#'   Chapter 11 (EF5 = 0.011 for nitrate leaching to N2O; EF4 = 0.016 for
#'   Atlantic ammonia to N2O; NH3 volatilisation fractions 0.11 synthetic
#'   and 0.21 organic, Table 11.3).
#'
#' @examples
#' n_attenuation_constants
"n_attenuation_constants"

#' Spain historical urban nitrogen applied to agriculture.
#'
#' @description
#' National-total nitrogen from Spanish urban human excreta and municipal
#' waste actually applied to agricultural land, at benchmark years. Used by
#' [build_urban_n()] as the global default per-capita urban-N-to-agriculture
#' rate (a documented placeholder, see that function's Details): this is
#' Spain's own historical series applied everywhere, not a
#' globally-calibrated estimate.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{area_code}{ISO3 country code (currently only \code{"ESP"}).}
#'   \item{year}{Benchmark calendar year.}
#'   \item{urban_n_gg}{National-total urban nitrogen applied to agriculture
#'     (Gg N/year).}
#' }
#'
#' @source Aguilera, E. (WHEP project team). Own estimation, transcribed
#'   from the Spain_Hist repository (private project data, not a public
#'   DOI): \code{input/Urban_waste.xlsx} sheet \code{UrbanN} and
#'   \code{input/updates/UrbanN_update.csv}.
#'
#' @examples
#' urban_n_reference
"urban_n_reference"

#' Spain historical per-capita urban nitrogen rate.
#'
#' @description
#' The per-capita urban-nitrogen-to-agriculture rate,
#' \code{urban_n_reference$urban_n_gg * 1e6 / spain_urban_population}, at
#' each \code{urban_n_reference} benchmark year `build_urban_n()` could
#' compute a verified rate for. See
#' \code{data-raw/build_urban_kgn_cap.R} for the derivation and its
#' provenance caveat: the committed rows use World Bank urban-population
#' figures (verified back to 1990 only), not HYDE, because no local HYDE
#' data was available when this table was built; the 1860, 1900 and 1950
#' \code{urban_n_reference} benchmark years are deliberately absent (no
#' verified denominator), not fabricated.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{year}{Benchmark calendar year.}
#'   \item{urban_kgn_cap}{Per-capita urban nitrogen applied to agriculture
#'     (kg N per person per year).}
#' }
#'
#' @source Derived from \code{urban_n_reference} and World Bank indicator
#'   \code{SP.URB.TOTL} (Spain urban population); see
#'   \code{data-raw/build_urban_kgn_cap.R}.
#'
#' @examples
#' urban_kgn_cap_reference
"urban_kgn_cap_reference"

#' MANNER synthetic-fertiliser application-rate factor.
#'
#' @description
#' Multiplicative ammonia-volatilisation factor for synthetic fertiliser as a
#' function of the nitrogen application rate, keyed by fertiliser, soil type
#' (calcareous / non-calcareous) and application-rate bin (kg N/ha). Used by
#' [calculate_manner_nh3()]'s synthetic-fertiliser path. AN and CAN are 1 for
#' every soil type and rate bin; Urea follows the same rate-response curve on
#' both soil types; AS follows the Urea curve on calcareous soils and is 1 on
#' non-calcareous soils.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{fertiliser}{Synthetic fertiliser: \code{"Urea"}, \code{"AN"},
#'     \code{"CAN"} or \code{"AS"}.}
#'   \item{soil_type}{\code{"calcareous"} or \code{"non-calcareous"}.}
#'   \item{rate_bin}{Application-rate bin (kg N/ha): one of \code{"0-30"},
#'     \code{"30-60"}, \code{"60-90"}, \code{"90-120"}, \code{"120-150"},
#'     \code{"150-180"}, \code{"180-200"}, \code{">200"}.}
#'   \item{factor}{Numeric multiplicative rate factor.}
#' }
#'
#' @source WHEP project-internal coefficient workbook (not a public DOI):
#'   Spain historical MANNER implementation, \code{NH3_model.xlsx}, sheet
#'   "synthetic fertilisers".
#'
#' @examples
#' manner_rate_factor
"manner_rate_factor"

#' MANNER synthetic-fertiliser rainfall factor.
#'
#' @description
#' Multiplicative ammonia-volatilisation factor for synthetic fertiliser as a
#' function of soil pH class and the same rainfall/wetness classification
#' used by the organic-manure \code{rainfall_wet} table in
#' [manner_params]. Used by [calculate_manner_nh3()]'s synthetic-fertiliser
#' path. AN and CAN are 1 for every pH class and rainfall class; AS is 1 for
#' every \code{pH<7} row.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{fertiliser}{Synthetic fertiliser: \code{"Urea"}, \code{"AN"},
#'     \code{"CAN"} or \code{"AS"}.}
#'   \item{ph_class}{Soil pH class: \code{"pH<7"} or \code{"other pH"}.}
#'   \item{rainfall_class}{Combined rainfall-level/wetness class (e.g.
#'     \code{"noraindry"}, \code{"heavyrainwet"}), the same 9 classes as
#'     [manner_params]'s \code{rainfall_wet} table.}
#'   \item{factor}{Numeric multiplicative rainfall factor.}
#' }
#'
#' @source WHEP project-internal coefficient workbook (not a public DOI):
#'   Spain historical MANNER implementation, \code{NH3_model.xlsx}, sheet
#'   "synthetic fertilisers".
#'
#' @examples
#' manner_rain_factor
"manner_rain_factor"

#' MANNER organic-manure incorporation-delay factor.
#'
#' @description
#' Multiplicative ammonia-volatilisation factor for organic manure as a
#' function of the delay between surface application and soil incorporation,
#' by manure type. Used by [calculate_manner_nh3()]'s organic-manure path,
#' distinct from [manner_params]'s \code{incorporation} table (a land-use
#' factor reused by the synthetic-fertiliser path; see that dataset's
#' description). The delay bins are monotonically increasing; a supplied
#' delay is assigned to the first (shortest) bin whose \code{delay_hours} is
#' greater than or equal to it, and a missing or infinite delay maps to
#' \code{"No incorporation"} (factor 1, no volatilisation reduction).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{manure_type}{Organic manure type: \code{"cattle_slurry"},
#'     \code{"pig_slurry"}, \code{"FYM"} or \code{"poultry_manure"}.}
#'   \item{delay_bin}{Incorporation-delay bin label (e.g. \code{"<2 h"},
#'     \code{"6-12 days"}, \code{"No incorporation"}).}
#'   \item{delay_hours}{Upper bound of the delay bin in hours; \code{NA} for
#'     \code{"No incorporation"}.}
#'   \item{factor}{Numeric multiplicative incorporation factor.}
#' }
#'
#' @source WHEP project-internal coefficient workbook (not a public DOI):
#'   Spain historical MANNER implementation, \code{NH3_model.xlsx}, sheet
#'   "manures".
#'
#' @examples
#' manner_incorporation_factor
"manner_incorporation_factor"

#' Inorganic (mineral) nitrogen fraction of excreted manure by species.
#'
#' @description
#' Fraction of total excreted nitrogen that is in inorganic (ammoniacal) form
#' and therefore available for ammonia volatilisation, by livestock species
#' and manure stream (whole excreta, or after separation into a liquid and a
#' solid fraction). Used by [calculate_manner_nh3()]'s organic-manure path to
#' scale the realised emission factor down to the ammoniacal nitrogen actually
#' applied. The mapping from [calculate_manner_nh3()]'s \code{manure_type}
#' argument to this table's \code{species}/\code{manure_stream} keys
#' (\code{cattle_slurry}/\code{pig_slurry} to the \code{"Liquid"} stream,
#' \code{FYM} to Cattle \code{"Solid"}, \code{poultry_manure} to Poultry
#' \code{"Solid"}) is a documented modelling choice made when porting this
#' table, not a literal Spain_Hist crosswalk; see
#' [calculate_manner_nh3()]'s Details.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{species}{Livestock species: \code{"Sheep"}, \code{"Goats"},
#'     \code{"Poultry"}, \code{"Cattle"}, \code{"Pigs"}, \code{"Horses"},
#'     \code{"Donkeys_mules"} or \code{"Rabbits"}.}
#'   \item{manure_stream}{Manure stream: \code{"Excreta"} (whole, unseparated
#'     excreta), \code{"Liquid"} or \code{"Solid"} (after mechanical
#'     separation); not every species has a \code{"Liquid"} row.}
#'   \item{inorganic_n_fraction}{Fraction of the stream's total nitrogen that
#'     is inorganic (ammoniacal).}
#'   \item{source}{Short author-year provenance string as cited in the
#'     Spain_Hist \code{Livestock.xlsx} \code{Manure_inorganic_N} sheet for
#'     that coefficient. These are secondary citations transcribed from that
#'     workbook, not independently DOI-verified full bibliographic entries.}
#' }
#'
#' @source WHEP project-internal coefficient workbook (not a public DOI):
#'   Spain historical livestock coefficient workbook, \code{Livestock.xlsx},
#'   sheet \code{Manure_inorganic_N}. That sheet in turn cites: Van Soest, P.
#'   J. (1994); Nahm, K. H. (2003); Nahm, K. H. (2005); Smith, K. A. & Frost,
#'   J. P. (2000); Chambers, B. J. et al. (1999); Chambers, B. J. et al.
#'   (2000); Nicholson, F. A. et al. (1996); Canh, T. T. et al. (1997);
#'   Sommer, S. G. et al. (2004); Burton, C. H. & Turner, C. (2003);
#'   Martinez, J. & Burton, C. H. (2003); Rotz, C. A. (2004); Wheeler, E. F.
#'   et al. (2011); Gungor-Demirci, G. & Demirer, G. N. (2004); Lebas, F.
#'   (1975); Lebas, F. (2004). These secondary citations are transcribed as
#'   recorded in the workbook and have not been independently verified
#'   against the primary sources.
#'
#' @examples
#' manure_inorganic_n
"manure_inorganic_n"

#' IPCC 2006 Tier 1 direct soil N2O emission factors by climate and
#' irrigation.
#'
#' @description
#' Direct nitrous-oxide emission factors (fraction of applied nitrogen
#' emitted as N2O-N) under the IPCC 2006 Guidelines Tier 1 defaults,
#' disaggregated by irrigation/management stratum and climate zone. Unlike
#' [n2o_efs_disaggregated()] (the Cayuela et al. 2017 meta-analytic
#' Mediterranean factors plus the IPCC 2019 Atlantic default), every stratum
#' here carries the flat IPCC 2006 default (0.010), except flooded rice
#' (0.003) in both climate zones. Used by [calculate_soil_n2o()]'s
#' `method = "ipcc2006"`.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{irrig_type}{Irrigation / management stratum: one of
#'     \code{"Tier_1"}, \code{"Rainfed"}, \code{"Traditional"},
#'     \code{"Drip"}, \code{"Sprinkler"}, \code{"Flooded"},
#'     \code{"Med_average"}.}
#'   \item{climate}{Climate zone: \code{"MED"} (Mediterranean) or
#'     \code{"ATL"} (Atlantic).}
#'   \item{ef}{Direct N2O emission factor (kg N2O-N per kg applied N).}
#' }
#'
#' @source WHEP project-internal coefficient workbook (not independently
#'   DOI-verified): \code{N_coefficients.xlsx}, sheet
#'   \code{N2O_EFs_IPCC2006}, itself transcribed from IPCC (2006), 2006 IPCC
#'   Guidelines for National Greenhouse Gas Inventories, Vol. 4, Chapter 11,
#'   Tier 1 defaults.
#'
#' @examples
#' n2o_efs_ipcc2006
"n2o_efs_ipcc2006"

#' Soil organic matter content bins.
#'
#' @description
#' Half-open bins that map a soil organic matter share (fraction) to a
#' soil organic matter class. A share \code{s} is assigned to the class
#' whose interval satisfies \code{s_min < s <= s_max}; the top (\code{High})
#' bin has no finite ceiling in the source and is treated as open-ended
#' (any share above its \code{s_min} is \code{"High"}), so \code{s_max = 1}
#' here is a wide ceiling, not a literal source value. The classes key the
#' Meisinger denitrification matrix ([meisinger_denitrification]).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{som_content}{Soil organic matter class: \code{"High"},
#'     \code{"Medium"} or \code{"Low"}.}
#'   \item{som_min}{Lower bound of the soil organic matter share interval
#'     (fraction).}
#'   \item{som_max}{Upper bound of the soil organic matter share interval
#'     (fraction); the \code{"High"} row's value is a wide ceiling, not a
#'     literal source bound (see Description).}
#' }
#'
#' @source Spain historical nitrogen coefficient workbook
#'   (\code{N_coefficients.xlsx}, sheet \code{SOM_ranges}), companion to the
#'   Meisinger & Randall (1991) denitrification matrix.
#'   \doi{10.2136/1991.managingnitrogen.c5}.
#'
#' @examples
#' som_ranges
"som_ranges"
