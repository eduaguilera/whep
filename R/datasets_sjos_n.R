# SJOS-N package-data documentation (Module 0, Task 0.4).
#
# Roxygen2 docs for the Safe and Just Operating Space (nitrogen axis)
# boundary and nourishment constants, stored in
# data/n_boundary_params.rda, data/nourishment_thresholds.rda,
# data/sjos_levels.rda and data/nourish_levels.rda. Constants are
# transcribed from plans/2026-07-10-sjos-nitrogen.md (Module 0); level
# labels and colours are ported by value from afsetools::load_vectors().

#' Planetary reactive-nitrogen boundary parameters.
#'
#' @description
#' Parameters defining the planetary boundary for anthropogenic reactive
#' nitrogen and its agri-food-system apportionment, used by the SJOS-N
#' per-capita boundary axis. The low, high and top values bracket the
#' published planetary reactive-nitrogen limit; the per-capita cap, the
#' synthetic-to-total agricultural ratio and the food share of agricultural
#' nitrogen scale that global limit to a comparable per-capita agricultural
#' basis.
#'
#' @format A tibble in long form with columns:
#' \describe{
#'   \item{parameter}{Parameter name (e.g. \code{"boundary_low"},
#'     \code{"per_capita_cap"}, \code{"syn_tot_agri_ratio"}).}
#'   \item{value}{Numeric parameter value.}
#'   \item{unit}{Unit of the value (e.g. \code{"Tg N/yr"},
#'     \code{"kg N/cap/yr"}, \code{"ratio"}, \code{"fraction"}).}
#'   \item{description}{Human-readable description of the parameter.}
#' }
#'
#' @source Planetary reactive-nitrogen boundary literature: de Vries, W.,
#'   Kros, J., Kroeze, C. & Seitzinger, S. P. (2013). Assessing planetary and
#'   regional nitrogen boundaries related to food security and adverse
#'   environmental impacts. *Current Opinion in Environmental
#'   Sustainability*, 5(3-4), 392-402. \doi{10.1016/j.cosust.2013.07.004};
#'   Campbell, B. M., Beare, D. J., Bennett, E. M., Hall-Spencer, J. M.,
#'   Ingram, J. S. I., Jaramillo, F., Ortiz, R., Ramankutty, N., Sayer, J. A.
#'   & Shindell, D. (2017). Agriculture production as a major driver of the
#'   Earth system exceeding planetary boundaries. *Ecology and Society*,
#'   22(4):8. \doi{10.5751/ES-09595-220408}; Springmann, M. et al. (2018).
#'   Options for keeping the food system within environmental limits.
#'   *Nature*, 562, 519-525. \doi{10.1038/s41586-018-0594-0}; regional
#'   agricultural-nitrogen boundaries: Schulte-Uebbing, L. F., Beusen, A. H.
#'   W., Bouwman, A. F. & de Vries, W. (2022). From planetary to regional
#'   boundaries for agricultural nitrogen pollution. *Nature*, 610, 507-512.
#'   \doi{10.1038/s41586-022-05158-2}. The specific low/high/top boundary
#'   values, the per-capita cap and the agricultural-apportionment ratios are
#'   transcribed from the Global SJOS-N analysis synthesising these sources;
#'   verify against Zotero before any manuscript use.
#'
#' @examples
#' n_boundary_params
"n_boundary_params"

#' Nourishment protein and energy thresholds.
#'
#' @description
#' Protein and dietary-energy floors and ceilings for the SJOS-N nourishment
#' ("just") axis, plus the waste-and-inequality factor that lifts the raw
#' per-capita protein bounds to supply-side thresholds and the normalised-score
#' class cutoffs. Raw protein floor and ceiling (46 and 63 g/cap/day) are
#' scaled by the 1.35 waste-and-inequality factor to the supply-side protein
#' floor and ceiling (62.1 and 85.05 g/cap/day). The class cutoffs (1 and 2)
#' split a normalised nourishment score into the Under, Adequate and Over
#' classes.
#'
#' **Only one of these numbers is sourced.** `provenance` records which, per
#' row, so nothing here can look verified when it is not:
#' the protein floor of 46 g/cap/day is WHO/FAO/UNU TRS 935 Table 46, the safe
#' intake of a 55 kg adult at a PDCAAS of 1.0 — and that safe level is a
#' 97.5th-percentile *individual* value, which TRS 935 (p.41) says is incorrect
#' to apply to a population. The 63 ceiling, the 2300 and 2900 energy bounds
#' and the 1.35 factor are `inherited_unsourced`: they come from the Global
#' SJOS-N analysis and no source has been produced for them, the author having
#' confirmed 1.35 was a preliminary presentation figure (whep#753).
#'
#' The energy bounds are **not** a second axis WHEP publishes. Nothing in the
#' package reads them, and WHEP's own energy column is gross combustion energy
#' where a dietary kcal threshold is metabolisable, so the two are not
#' comparable as they stand. See [normalize_nourishment()].
#'
#' @format A tibble in long form with columns:
#' \describe{
#'   \item{metric}{Metric name: \code{"protein_raw"}, \code{"protein"},
#'     \code{"energy"}, \code{"waste_inequality"} or \code{"class"}.}
#'   \item{bound}{Which bound the value is: \code{"floor"}, \code{"ceiling"},
#'     \code{"factor"}, \code{"under"} or \code{"over"}. The upper protein and
#'     energy bound is named \code{"ceiling"} because
#'     [normalize_nourishment()] uses it as the top of the Adequate band, above
#'     which a country is classified Over — not as something to aim at
#'     (whep#753).}
#'   \item{value}{Numeric threshold value.}
#'   \item{unit}{Unit of the value (\code{"g/cap/day"},
#'     \code{"kcal/cap/day"}, \code{"ratio"} or \code{"score"}).}
#'   \item{provenance}{Where the value comes from:
#'     \code{"trs935_table46_55kg_safe_level"},
#'     \code{"derived_raw_times_waste_inequality"},
#'     \code{"inherited_unsourced"} or \code{"definition"}.}
#' }
#'
#' @source The protein floor: WHO/FAO/UNU (2007). *Protein and amino acid
#'   requirements in human nutrition*, WHO Technical Report Series 935, Table
#'   46. The food-system environmental-limits framing: Springmann, M. et al.
#'   (2018). Options for keeping the food system within environmental limits.
#'   *Nature*, 562, 519-525. \doi{10.1038/s41586-018-0594-0}. Every other value
#'   is transcribed from the Global SJOS-N analysis and carries
#'   \code{provenance == "inherited_unsourced"}; do not cite one without
#'   establishing a source first.
#'
#' @examples
#' nourishment_thresholds
"nourishment_thresholds"

#' Safe-and-just nitrogen classification levels and colours.
#'
#' @description
#' The ordered levels of the SJOS-N 2-way classification: the reactive-nitrogen
#' boundary axis (\code{Within_boundary} versus \code{Exceedance}) crossed with
#' the nourishment axis (\code{Under}, \code{Adequate}, \code{Over}), with a
#' plotting colour per combined level. Used to classify and colour polities on
#' the safe-and-just nitrogen space.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{level}{Combined classification level (e.g.
#'     \code{"Within_boundary Under"}, \code{"Exceedance Over"}).}
#'   \item{order}{Integer plotting/factor order (1 to 6).}
#'   \item{colour}{Plotting colour (an R colour name).}
#' }
#'
#' @source Boundary axis from the agricultural reactive-nitrogen boundary
#'   framework of Schulte-Uebbing, L. F., Beusen, A. H. W., Bouwman, A. F. &
#'   de Vries, W. (2022). \doi{10.1038/s41586-022-05158-2} and de Vries, W. et
#'   al. (2013). \doi{10.1016/j.cosust.2013.07.004}; nourishment axis from the
#'   nourishment thresholds (see \link{nourishment_thresholds}). The level
#'   labels and colours are ported by value from \code{afsetools::load_vectors()}
#'   (\code{SJOS_levels} and \code{SJOS_colours}); verify against Zotero before
#'   any manuscript use.
#'
#' @examples
#' sjos_levels
"sjos_levels"

#' Nourishment classification levels and colours.
#'
#' @description
#' The ordered levels of the nourishment ("just") axis (\code{Over},
#' \code{Adequate}, \code{Under}) with a plotting colour per level, used to
#' classify and colour polities by per-capita nourishment adequacy.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{level}{Nourishment level: \code{"Over"}, \code{"Adequate"} or
#'     \code{"Under"}.}
#'   \item{order}{Integer plotting/factor order (1 to 3).}
#'   \item{colour}{Plotting colour (an R colour name).}
#' }
#'
#' @source Level labels and colours ported by value from
#'   \code{afsetools::load_vectors()} (\code{Nour_levels} and
#'   \code{Nourish_colours}). The nourishment adequacy framing follows
#'   Springmann, M. et al. (2018). Options for keeping the food system within
#'   environmental limits. *Nature*, 562, 519-525.
#'   \doi{10.1038/s41586-018-0594-0}; verify against Zotero before any
#'   manuscript use.
#'
#' @examples
#' nourish_levels
"nourish_levels"
