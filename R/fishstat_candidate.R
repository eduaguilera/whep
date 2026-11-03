# Classified raw product-weight input for whep#1616/#1617, not a CBS pin.
# Source: FI_Trade_2025.1.0, FSJ_UNIT.csv: Q_tpw is tonnes net product weight.
# https://www.fao.org/fishery/static/Data/FI_Trade_2025.1.0.zip
# Species classification is not standardization to live-weight accounts.
# The historical FAO workflow treats commodity form, extraction trees and
# country/period links separately; no universal conversion is assumed here:
# https://sws-methodology.github.io/faoswsFisheriesSUAFBSdocumentation/

# No current standardized-candidate schema has been established. Reject even
# partial metadata rather than letting a missing marker make raw tonnes look
# like legacy CBS inputs. Future adoption needs an explicitly tested contract.
.check_fishstat_cbs_basis <- function(dt) {
  markers <- c("Artifact Type", "Weight Basis", "Source Measure")
  if (any(rlang::has_name(dt, markers))) {
    cli::cli_abort(
      c(
        "FishStat raw or weight-labelled candidates are not CBS trade inputs.",
        "i" = paste(
          "Product-weight standardization and account scope must be",
          "validated first (whep#1617)."
        )
      ),
      class = "whep_fishstat_unconverted"
    )
  }
  invisible(NULL)
}


.prepare_fishstat_candidate <- function(trade, commodities, countries, bridge) {
  rows <- .fishstat_candidate_items(trade, commodities, bridge)
  rows <- .fishstat_resolve_reporters(rows, countries)
  .fishstat_candidate_rows(rows)
}

# One row per original observation: never aggregate away the conversion key
# or silently turn an unknown quantity into zero by summing with na.rm = TRUE.
.fishstat_candidate_rows <- function(rows) {
  out <- rows |>
    dplyr::transmute(
      `Artifact Type` = "fishstat_classified_raw_v1",
      `Weight Basis` = "net_product_weight",
      Year = .data$year,
      `Reporter Code` = .data$reporter_code,
      `Area Code` = .data$area_code,
      `Polity Code` = .data$polity_code,
      `Coverage Status` = .data$coverage_status,
      `Source Commodity Code` = .data$fao_code,
      `Source ISSCAAP Group` = .data$isscaap_group,
      `Source Measure` = .data$source_measure,
      `Source Flag` = .data$source_flag,
      `Product Form` = .data$product_form,
      `Species Item Code` = .data$species_item_cbs_code,
      `Item Code` = .data$item_cbs_code,
      `Classification Status` = .data$classification_status,
      Element = dplyr::if_else(
        .data$flow == "I",
        "Import quantity",
        "Export quantity"
      ),
      Value = .data$value,
      Unit = "tonnes"
    )
  # fread inputs can carry a process-local data.table self-reference through
  # dplyr. Rebuild only the container; preserve each column exactly.
  tibble::as_tibble(lapply(out, identity))
}

# Fingerprint the objects actually resolved in this loaded namespace. Ambient
# WHEP_POLITIES_* files are build references, not runtime identity evidence.
.fishstat_runtime_provenance <- function() {
  tables <- list(
    polities = polities,
    polity_area_crosswalk = polity_area_crosswalk,
    polity_containment = polity_containment,
    regions_full = regions_full,
    resolved_crosswalk = .polity_crosswalk()
  )
  namespace <- asNamespace("whep")
  objects <- mget(ls(namespace, all.names = TRUE), envir = namespace)
  functions <- Filter(is.function, objects)
  list(
    table_rows = lapply(tables, nrow),
    table_column_hash = lapply(tables, .fishstat_table_hash),
    package_data_files = as.list(.whep_data_digests()),
    runtime_code_hash = rlang::hash(lapply(
      functions,
      .fishstat_code_signature
    )),
    unfold_rest_of_world = .unfold_rest_of_world_mode(),
    unfold_predecessor_bucket = .predecessor_unfold_mode()
  )
}

.fishstat_table_hash <- function(table) {
  # All named columns and their attributes; no process-local container pointer.
  rlang::hash(lapply(table, identity))
}

.fishstat_code_signature <- function(fun) {
  list(
    formals = formals(fun),
    body = paste(deparse(body(fun)), collapse = "\n")
  )
}
