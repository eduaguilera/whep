#' Livestock coefficients
#'
#' Coefficients used for manure emissions calculations by species, region and
#' temperature.
#'
#' @format A tibble with 7 rows and 5 columns:
#' - `species`: character. Common name of the livestock species.
#' - `region`: character. Spatial aggregation (for example, "Global").
#' - `temperature`: character. Climatic category (for example, "Temperate",
#'   "Tropical").
#' - `ef_manure_ch4`: numeric. Methane emission factor from manure. Units are
#'   those used by the source (see @source).
#' - `ef_manure_n2o`: numeric. Nitrous oxide emission factor from manure. Units
#'   are those used by the source (see @source).
#'
#' @source IPCC (2006) emission factor guidance and FAOSTAT data.
#'   https://www.ipcc-nggip.iges.or.jp/public/2006gl/ and
#'   https://www.fao.org/faostat/en/#data.
#'
#' @examples
#' livestock_coefs
"livestock_coefs"
