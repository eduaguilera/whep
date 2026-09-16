#' Split livestock excretion across manure-management systems.
#'
#' @description
#' Splits the excreted nitrogen, carbon and volatile solids from
#' [estimate_n_excretion()] across manure-management systems (MMS), separating
#' the in-situ grazing stream (pasture/range/paddock, deposited where it falls)
#' from the collected/housed streams routed to storage. The split conserves mass:
#' the per-species MMS shares sum to one.
#'
#' @param excretion A tibble from [estimate_n_excretion()] with `year`,
#'   `territory` (a stringified `area_code`, see [estimate_n_excretion()]),
#'   `sub_territory`, `livestock_category`, `n_excretion`, `c_excretion` and
#'   `vs_excretion`.
#' @param options A named list. `mms_source` selects how the MMS shares in
#'   `regional_mms_distribution` are read:
#'   * `"regional_default"` (default): every territory takes the table's
#'     `region == "Global"` rows, the IPCC/GLEAM global default.
#'   * `"region_specific"`: each territory takes the rows of the region it
#'     resolves to, and the Global rows when its region has none. Only four
#'     `(region, species)` pairs carry region-specific rows (North America
#'     cattle and swine, Western Europe cattle, Latin America cattle), so
#'     every other row is unchanged.
#'
#' @return A tibble with one row per
#'   `year x territory x sub_territory x livestock_category x mms_type`, plus
#'   `species_gen`, `loss_category`, `stream` (`"grazing"` or `"collected"`),
#'   `n_stream`, `c_stream`, `vs_stream` and `method_mms`.
#' @export
#' @examples
#' excretion <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~livestock_category,
#'   ~n_excretion, ~c_excretion, ~vs_excretion,
#'   2020L, "203", NA, "Cattle_milk", 100, 1900, 60,
#'   2020L, "203", NA, "Pigs", 30, 270, 20
#' )
#' split_manure_management(excretion)
split_manure_management <- function(excretion, options = list()) {
  opt <- utils::modifyList(list(mms_source = "regional_default"), options)
  mms_source <- opt$mms_source
  opt$mms_source <- rlang::arg_match(
    mms_source,
    c("regional_default", "region_specific")
  )
  .check_excretion_cols(excretion)
  bridge <- dplyr::select(
    .species_taxonomy_bridge(),
    "livestock_category",
    "species_gen",
    "loss_category",
    "cn_species"
  )

  joined <- excretion |>
    tibble::as_tibble() |>
    dplyr::left_join(bridge, by = "livestock_category") |>
    .attach_mms_shares(opt$mms_source)
  if (anyNA(joined$mms_type)) {
    bad <- unique(joined$species_gen[is.na(joined$mms_type)])
    cli::cli_abort("No MMS distribution for species {.val {bad}}.")
  }

  joined |>
    dplyr::mutate(
      stream = dplyr::if_else(
        .data$mms_type == "Pasture/Range/Paddock",
        "grazing",
        "collected"
      ),
      n_stream = .data$n_excretion * .data$fraction,
      c_stream = .data$c_excretion * .data$fraction,
      vs_stream = .data$vs_excretion * .data$fraction,
      method_mms = opt$mms_source
    ) |>
    dplyr::select(
      "year",
      "territory",
      "sub_territory",
      "livestock_category",
      "species_gen",
      "loss_category",
      "cn_species",
      "mms_type",
      "stream",
      "n_stream",
      "c_stream",
      "vs_stream",
      "method_mms"
    )
}

# Private helpers ----

.check_excretion_cols <- function(excretion) {
  req <- c(
    "year",
    "territory",
    "sub_territory",
    "livestock_category",
    "n_excretion",
    "c_excretion",
    "vs_excretion"
  )
  miss <- req[!purrr::map_lgl(req, ~ rlang::has_name(excretion, .x))]
  if (length(miss) > 0) {
    cli::cli_abort("{.arg excretion} is missing column{?s}: {.val {miss}}.")
  }
  invisible(NULL)
}

# Attach the MMS shares to the excretion rows, one row per (input row, MMS).
# "regional_default" gives every territory the Global rows. "region_specific"
# resolves each territory's region and hands it to the shared resolver below.
.attach_mms_shares <- function(rows, source) {
  if (identical(source, "regional_default")) {
    return(.resolve_mms_shares(rows))
  }
  rows |>
    dplyr::mutate(mms_region = .mms_region_of(.data$territory)) |>
    .resolve_mms_shares("mms_region")
}

# The one MMS-share resolver, shared by both manure engines (#679): this
# function and the Tier-2 methane / direct-N2O engine in R/livestock_manure.R,
# which calls it with region_col = "region".
#
# Rows are expanded to one row per (input row, MMS type) by joining
# `regional_mms_distribution` on `species_gen`. With no region column, or with
# `region_col` absent from `rows`, every row takes the `region == "Global"`
# distribution. With a region column, a row takes its own region's rows when
# the table has any for that (region, species) -- only four pairs do -- and the
# Global rows for that species otherwise. The fallback is a left_join on
# species only, so a row whose region is unknown or unmatched keeps the Global
# split rather than losing its rows or collapsing to a flat default (#201).
.resolve_mms_shares <- function(rows, region_col = NULL) {
  global <- .mms_global_shares()
  if (is.null(region_col) || !rlang::has_name(rows, region_col)) {
    return(.join_mms_shares(rows, global))
  }
  regional <- .mms_regional_shares()
  by <- c(
    c("species_gen" = "species"),
    rlang::set_names("region", region_col)
  )
  dplyr::bind_rows(
    dplyr::inner_join(rows, regional, by = by, relationship = "many-to-many"),
    .join_mms_shares(dplyr::anti_join(rows, regional, by = by), global)
  )
}

.join_mms_shares <- function(rows, shares) {
  dplyr::left_join(
    rows,
    shares,
    by = c("species_gen" = "species"),
    relationship = "many-to-many"
  )
}

# The shares are renormalised to sum to one within each (region, species), so
# the split conserves mass whatever the table holds. On the shipped
# `regional_mms_distribution` every group already sums to exactly 1, so the
# division is by 1.0 and leaves each fraction bit-identical.
.mms_global_shares <- function() {
  whep::regional_mms_distribution |>
    dplyr::filter(.data$region == "Global") |>
    dplyr::mutate(
      fraction = .data$fraction / sum(.data$fraction),
      .by = "species"
    ) |>
    dplyr::select("species", "mms_type", "fraction")
}

.mms_regional_shares <- function() {
  whep::regional_mms_distribution |>
    dplyr::filter(.data$region != "Global") |>
    dplyr::mutate(
      fraction = .data$fraction / sum(.data$fraction),
      .by = c("region", "species")
    ) |>
    dplyr::select("region", "species", "mms_type", "fraction")
}

# `regional_mms_distribution`'s non-Global regions are IPCC region labels --
# the vocabulary .add_ipcc_region() emits -- so the territory is resolved
# through the same GLEAM-region lookup whep#465 built for the emission-factor
# tables, rather than through a second crosswalk.
#
# `territory` is a stringified `area_code` (see estimate_n_excretion()) but an
# ISO3 literal is still accepted there, so a non-numeric territory is tried as
# an ISO3. Anything that is neither resolves to NA and takes the Global rows,
# which is why the region is resolved here and not required upstream.
#
# The area code needs no ISO3 attached to it: since whep#678 the shared
# resolver derives the ISO3 from `area_code` itself.
.mms_region_of <- function(territory) {
  .add_ipcc_region(.mms_region_keys(territory))$region
}

.mms_region_keys <- function(territory) {
  code <- suppressWarnings(as.integer(territory))
  tibble::tibble(
    area_code = code,
    iso3 = dplyr::if_else(
      is.na(code),
      toupper(as.character(territory)),
      NA_character_
    )
  )
}

#' Apply IPCC manure-management losses to the collected manure streams.
#'
#' @description
#' Nets the nitrogen surviving manure management onto the field, applying the
#' IPCC 2019 management-loss fractions to the collected/housed streams from
#' [split_manure_management()]: `applied_n = n_stream * (1 - FracLossMS)` where
#' `FracLossMS = FracGasMS + FracLeachMS + EF3 + FracN2MS`. The grazing
#' (pasture/range/paddock) stream is deposited in situ and keeps its full
#' nitrogen (its in-situ soil losses belong to the soil stage). Indirect N2O is
#' reported as a labelled sub-flux of the already-removed volatilized and leached
#' nitrogen (the same N is not removed twice). Carbon applied to the field is
#' `c_stream * (1 - c_loss_fraction)`, the stream's carbon less what its
#' management system mineralises in storage, and the volatile solids are scaled
#' by the same ratio. The applied C:N is therefore a RESULT of that loss rather
#' than an imposed cap. Loss fractions and their sources are in
#' `inst/extdata/manure/manure_storage_c_loss.csv`: nothing for grazing or daily
#' spread, which have no storage stage; 0.420 of initial carbon for solid
#' storage and 0.424 for poultry litter (Pardo et al. 2015,
#' \doi{10.1111/gcb.12806}); 0.110 (cattle) and 0.128 (pigs) for slurry,
#' derived from Kupper et al. 2020 (\doi{10.1016/j.agee.2020.106963}) using
#' this package's own 0.47 kg C per kg volatile solids.
#'
#' Until whep#1006 this was a cap, `pmin(c_stream, applied_n * post-storage
#' C:N)`, which produced a loss only as a side effect of holding the applied
#' C:N down. That made the loss depend on the excreted composition rather than
#' on the storage system: when excreted carbon moved, the reported loss for
#' cattle solid storage fell from 40.3% to 6.9% with no coefficient changing.
#'
#' @param split A tibble from [split_manure_management()].
#' @param options A named list. `method` selects the loss method
#'   (`"ipcc_2019_tier2"`).
#'
#' @return The input rows with `manure_type`, `applied_n`, `applied_c`,
#'   `applied_vs`, `n_volatilized`, `n_leached`, `n2o_direct_n`, `n2_n`,
#'   `n2o_indirect_n`, `c_lost`, `vs_destroyed` and `method_losses`.
#' @export
#' @examples
#' excretion <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~livestock_category,
#'   ~n_excretion, ~c_excretion, ~vs_excretion,
#'   2020L, "203", NA, "Cattle_milk", 100, 1900, 60
#' )
#' apply_management_losses(split_manure_management(excretion))
apply_management_losses <- function(split, options = list()) {
  opt <- utils::modifyList(list(method = "ipcc_2019_tier2"), options)
  if (!identical(opt$method, "ipcc_2019_tier2")) {
    cli::cli_abort("Unknown {.arg method} {.val {opt$method}}.")
  }
  .check_split_cols(split)

  ind <- whep::indirect_n2o_ef
  ef4 <- ind$value[ind$parameter == "ef4_volatilization"]
  ef5 <- ind$value[ind$parameter == "ef5_leaching"]
  n2_ratio <- .n2_to_n2o_ratio()

  out <- split |>
    tibble::as_tibble() |>
    dplyr::left_join(
      .manure_loss_fractions(),
      by = c("mms_type", "loss_category" = "animal_category")
    ) |>
    dplyr::left_join(.manure_ef3(), by = "mms_type")
  if (anyNA(out$frac_gas_ms) || anyNA(out$ef3)) {
    cli::cli_abort("Missing loss fraction or EF3 for some MMS.")
  }

  out <- out |>
    dplyr::mutate(
      n_volatilized = dplyr::if_else(
        .data$stream == "grazing",
        0,
        .data$n_stream * .data$frac_gas_ms
      ),
      n_leached = dplyr::if_else(
        .data$stream == "grazing",
        0,
        .data$n_stream * .data$frac_leach_ms
      ),
      n2o_direct_n = dplyr::if_else(
        .data$stream == "grazing",
        0,
        .data$n_stream * .data$ef3
      ),
      n2_n = .data$n2o_direct_n * n2_ratio,
      n2o_indirect_n = .data$n_volatilized * ef4 + .data$n_leached * ef5,
      applied_n = dplyr::if_else(
        .data$stream == "grazing",
        .data$n_stream,
        pmax(
          0,
          .data$n_stream -
            .data$n_volatilized -
            .data$n_leached -
            .data$n2o_direct_n -
            .data$n2_n
        )
      )
    ) |>
    dplyr::left_join(.mms_manure_type(), by = "mms_type") |>
    .attach_storage_c_loss()

  out |>
    dplyr::mutate(
      applied_c = .data$c_stream * (1 - .data$c_loss_fraction),
      c_lost = .data$c_stream - .data$applied_c,
      applied_vs = dplyr::if_else(
        .data$c_stream > 0,
        .data$vs_stream * .data$applied_c / .data$c_stream,
        .data$vs_stream
      ),
      vs_destroyed = .data$vs_stream - .data$applied_vs,
      method_losses = opt$method
    ) |>
    dplyr::select(
      "year",
      "territory",
      "sub_territory",
      "livestock_category",
      "species_gen",
      "mms_type",
      "manure_type",
      "stream",
      "applied_n",
      "applied_c",
      "applied_vs",
      "n_volatilized",
      "n_leached",
      "n2o_direct_n",
      "n2_n",
      "n2o_indirect_n",
      "c_lost",
      "vs_destroyed",
      "method_losses"
    )
}

.check_split_cols <- function(split) {
  req <- c(
    "mms_type",
    "loss_category",
    "cn_species",
    "stream",
    "n_stream",
    "c_stream",
    "vs_stream"
  )
  miss <- req[!purrr::map_lgl(req, ~ rlang::has_name(split, .x))]
  if (length(miss) > 0) {
    cli::cli_abort("{.arg split} is missing column{?s}: {.val {miss}}.")
  }
  invisible(NULL)
}

# EF3 (direct-N2O from management) per engine MMS type, reusing
# ipcc_2019_n2o_ef_direct with a name crosswalk (its rows use finer system
# labels than the six MMS this engine carries).
.manure_ef3 <- function() {
  ef <- whep::ipcc_2019_n2o_ef_direct
  pick <- function(sys) ef$ef_kg_n2o_n_per_kg_n[ef$system == sys]
  tibble::tibble(
    mms_type = c(
      "Pasture/Range/Paddock",
      "Daily Spread",
      "Solid Storage",
      "Liquid/Slurry",
      "Anaerobic Lagoon",
      "Poultry Manure"
    ),
    ef3 = c(
      pick("Pasture/Range/Paddock"),
      pick("Daily Spread"),
      pick("Solid Storage"),
      pick("Liquid/Slurry"),
      pick("Uncovered Anaerobic Lagoon"),
      pick("Poultry Manure - Deep Litter")
    )
  )
}

# Storage carbon loss, per management system, from a sourced table.
#
# This REPLACED a C:N cap (`applied_c = pmin(c_stream, applied_n * cn_post)`),
# which produced a storage loss only as a side effect of the applied C:N being
# held below a tabulated post-storage value. That coupling failed the moment
# excreted carbon changed: when whep#1006 took excreted C from the volatile
# solids instead of a dung C:N, the cap stopped binding for cattle solid
# storage and the reported loss collapsed from 40.3% (beef) and 26.7% (dairy)
# to 6.9% and 8.1%, with no coefficient having changed. A loss term that moves
# because something else moved is not a modelled quantity.
#
# So storage loss is now its own coefficient with its own source, and the
# post-storage C:N falls out as a RESULT rather than being imposed. The
# `bio_coefs` Solid/Liquid C:N values are no longer read here.
#
# Values and provenance live in inst/extdata/manure/manure_storage_c_loss.csv.
# In summary: grazing and daily spread lose nothing because neither has a
# storage stage (IPCC 2019 Table 10.18, definitional); solid storage loses
# 0.420 of initial carbon and poultry litter 0.424 (Pardo et al. 2015,
# doi:10.1111/gcb.12806, Table 2 -- a systematic review, on a carbon basis);
# slurry loses 0.110 (cattle) and 0.128 (pigs), DERIVED from Kupper et al.
# 2020 (doi:10.1016/j.agee.2020.106963) gas-per-VS figures using this
# package's own 0.47 kg C per kg VS, so one carbon fraction is used
# throughout. IPCC publishes no carbon-loss fraction for any system and puts
# manure CO2 out of scope, which is why none of this is cited to it.
.storage_c_loss_coefs <- function() {
  system.file(
    "extdata",
    "manure",
    "manure_storage_c_loss.csv",
    package = "whep"
  ) |>
    utils::read.csv(stringsAsFactors = FALSE) |>
    tibble::as_tibble()
}

# Join the loss fraction on (mms_type, species), falling back to the table's
# own `All_species` row. The fallback is a row in the table, not a default in
# the code, so a species with no published value is visible in the data rather
# than being silently absorbed here.
.attach_storage_c_loss <- function(out) {
  coefs <- .storage_c_loss_coefs()
  by_species <- coefs |>
    dplyr::filter(.data$species != "All_species") |>
    dplyr::select("mms_type", cn_species = "species", "c_loss_fraction")
  by_mms <- coefs |>
    dplyr::filter(.data$species == "All_species") |>
    dplyr::select("mms_type", any_species = "c_loss_fraction")
  joined <- out |>
    dplyr::left_join(by_species, by = c("mms_type", "cn_species")) |>
    dplyr::left_join(by_mms, by = "mms_type") |>
    dplyr::mutate(
      c_loss_fraction = dplyr::coalesce(
        .data$c_loss_fraction,
        .data$any_species
      )
    ) |>
    dplyr::select(-"any_species")
  missing <- sort(unique(joined$mms_type[is.na(joined$c_loss_fraction)]))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "No storage carbon-loss fraction for MMS {.val {missing}}.",
      i = "Add a row to
           {.file inst/extdata/manure/manure_storage_c_loss.csv} with its
           source. A missing system must not silently lose nothing."
    ))
  }
  joined
}

# Map each engine MMS to the bio_coefs manure_type whose post-storage C:N applies:
# grazing deposition is fresh excreta; solid systems use the solid C:N; slurry
# and lagoon use the liquid C:N. The applied C:N is therefore the post-storage
# value, not the fresh-excreta value.
.mms_manure_type <- function() {
  tibble::tribble(
    ~mms_type,
    ~manure_type,
    "Pasture/Range/Paddock",
    "Excreta",
    "Daily Spread",
    "Solid",
    "Solid Storage",
    "Solid",
    "Liquid/Slurry",
    "Liquid",
    "Anaerobic Lagoon",
    "Liquid",
    "Poultry Manure",
    "Solid"
  )
}
