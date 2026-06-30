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
