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
#'   `bedding` optionally traces crop-residue bedding into the managed manure
#'   streams. `NULL` (default) leaves every published value where it was: the
#'   bedding columns are reported as zero and the streams stay excreta-only.
#'   Supply the [build_residue_bedding_supply()] contract (`year`, `territory`,
#'   `sub_territory`, `bedding_dm_t`, `bedding_c_t`, `bedding_n_t`) to add it.
#'
#'   Bedding reaches only the **litter-using collected** systems --
#'   `"Solid Storage"`, `"Daily Spread"` and `"Poultry Manure"`, the last being
#'   the deep-litter system whose own IPCC loss row is labelled "Poultry manure
#'   with litter". `"Liquid/Slurry"` and `"Anaerobic Lagoon"` use little or no
#'   litter and get none, and the grazing (`"Pasture/Range/Paddock"`) stream
#'   keeps its excreta-only composition because animals at pasture are not
#'   bedded (whep#1005). Within a `(year, territory, sub_territory)` the
#'   bedding is split across those systems in proportion to their excreted
#'   nitrogen, the same N-proportional share [allocate_manure_to_land()] uses
#'   for its carbon and volatile-solids splits.
#'
#'   This is a **supply-side** wiring only: the quantity supplied is placed on
#'   the litter-using streams and reported, but it is not bounded by what the
#'   housed herd could physically use. Bedding supplied for a group with no
#'   litter-using collected stream is warned about with its magnitude rather
#'   than zeroed in silence. Bounding it by demand needs per-head litter rates,
#'   and the citable sets disagree by more than an order of magnitude: EMEP/EEA
#'   Guidebook 2023, Ch. 3.B Table 3-7 gives 1500 kg straw per dairy cow-year
#'   over a 180-day housing period and 200 kg per finishing pig-year, and
#'   Rösemann et al. (Thünen Report 84) gives 5.0-8.0 kg per dairy place-day by
#'   housing system, while the only globally applied set (Smerald, Rahimi and
#'   Scheer 2023, *Sci. Data* 10:685, from Scarlat et al. 2010) uses 0.375 kg
#'   per cattle-day. That is whep#1005's point 2 and it stays open.
#'
#'   IPCC 2019 Refinement Vol.4 Ch.10 p. 10.96 offers an independent
#'   cross-check on the nitrogen: about 7 kg N per dairy cow-year, 4 for other
#'   cattle, 0.8 for market and 5.5 for breeding swine, doubled for deep
#'   bedding (Webb 2001; Döhler et al. 2002).
#'
#' @return A tibble with one row per
#'   `year x territory x sub_territory x livestock_category x mms_type`, plus
#'   `species_gen`, `loss_category`, `stream` (`"grazing"` or `"collected"`),
#'   `n_stream`, `c_stream`, `vs_stream`, `dm_bedding`, `n_bedding`,
#'   `c_bedding`, `method_mms` and `method_bedding`.
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
  opt <- utils::modifyList(
    list(mms_source = "regional_default", bedding = NULL),
    options
  )
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
    .attach_bedding(opt$bedding) |>
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
      "dm_bedding",
      "n_bedding",
      "c_bedding",
      "method_mms",
      "method_bedding"
    )
}

# Private helpers ----

# The manure-management systems that use litter. "Poultry Manure" is the
# deep-litter system: its own row in `manure_loss_fractions.csv` is sourced as
# IPCC 2019 V4 Ch10 Table 10.22 "Poultry manure with litter". Solid storage and
# daily spread are the bedded farmyard-manure systems. Liquid/slurry and
# anaerobic lagoons are excluded because litter is what makes a manure
# stackable, and pasture/range/paddock is excluded because grazing animals are
# not bedded (whep#1005).
.litter_mms_types <- function() {
  c("Solid Storage", "Daily Spread", "Poultry Manure")
}

.bedding_prototype <- function() {
  tibble::tibble(
    bedding_dm_t = double(),
    bedding_c_t = double(),
    bedding_n_t = double()
  )
}

# The join key. `sub_territory` is carried as a character literal here because
# the manure chain writes it as a bare `NA` (logical) at the national grain and
# as a cell id (character) at the subnational one; keying on the stringified
# value joins both without touching the column the caller sees.
.bedding_join_key <- function(rows) {
  dplyr::mutate(
    rows,
    sub_territory_key = as.character(.data$sub_territory)
  )
}

# Spread each group's bedding dry matter, carbon and nitrogen over its
# litter-using collected streams in proportion to their excreted nitrogen. With
# `bedding = NULL` the three columns are exact zeros, which is why nothing
# downstream moves until bedding is asked for.
.attach_bedding <- function(rows, bedding) {
  if (is.null(bedding)) {
    return(dplyr::mutate(
      rows,
      dm_bedding = 0,
      n_bedding = 0,
      c_bedding = 0,
      method_bedding = "none"
    ))
  }
  bedding <- bedding |>
    tibble::as_tibble() |>
    .check_bedding_cols() |>
    ensure_columns(.bedding_prototype(), extra = "keep") |>
    .bedding_join_key() |>
    dplyr::select(
      "year",
      "territory",
      "sub_territory_key",
      "bedding_dm_t",
      "bedding_c_t",
      "bedding_n_t"
    )
  key <- c("year", "territory", "sub_territory_key")
  shares <- rows |>
    .bedding_join_key() |>
    dplyr::mutate(
      litter_n = dplyr::if_else(
        .data$mms_type %in% .litter_mms_types() & .data$stream == "collected",
        .data$n_stream,
        0
      )
    ) |>
    dplyr::mutate(
      litter_n_total = sum(.data$litter_n),
      .by = dplyr::all_of(key)
    )
  .warn_bedding_unplaceable(shares, bedding, key)
  shares |>
    # `many-to-one`, so a bedding table carrying two rows for one
    # `(year, territory, sub_territory)` aborts rather than fanning the manure
    # streams out and placing the litter twice.
    dplyr::left_join(bedding, by = key, relationship = "many-to-one") |>
    dplyr::mutate(
      share = dplyr::if_else(
        .data$litter_n_total > 0,
        .data$litter_n / .data$litter_n_total,
        0
      ),
      dm_bedding = .data$share * dplyr::coalesce(.data$bedding_dm_t, 0),
      c_bedding = .data$share * dplyr::coalesce(.data$bedding_c_t, 0),
      n_bedding = .data$share * dplyr::coalesce(.data$bedding_n_t, 0),
      method_bedding = "litter_mms_n_share"
    ) |>
    dplyr::select(
      -"litter_n",
      -"litter_n_total",
      -"share",
      -"sub_territory_key",
      -"bedding_dm_t",
      -"bedding_c_t",
      -"bedding_n_t"
    )
}

.check_bedding_cols <- function(bedding) {
  req <- c("year", "territory", "sub_territory")
  miss <- req[!purrr::map_lgl(req, ~ rlang::has_name(bedding, .x))]
  if (length(miss) > 0) {
    cli::cli_abort(
      "{.arg bedding} is missing column{?s}: {.val {miss}}.",
      class = "whep_error_bedding_cols"
    )
  }
  bedding
}

# Bedding supplied for a group with no litter-using collected stream has
# nowhere to go: an all-grazing herd is bedded on paper only. Zeroing it
# silently would lose the carbon and nitrogen this whole path exists to trace,
# so it is named with its magnitude.
.warn_bedding_unplaceable <- function(shares, bedding, key) {
  placed <- shares |>
    dplyr::filter(.data$litter_n_total > 0) |>
    dplyr::select(dplyr::all_of(key)) |>
    dplyr::distinct()
  lost <- dplyr::anti_join(bedding, placed, by = key)
  lost <- dplyr::filter(lost, .data$bedding_dm_t > 0)
  if (nrow(lost) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "{nrow(lost)} bedding row{?s} reached no litter-using collected manure
     stream.",
    i = "{round(sum(lost$bedding_dm_t))} t of bedding dry matter and
      {round(sum(lost$bedding_n_t))} t N is not traced into any manure
      stream."
  ))
  invisible(NULL)
}

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
#' `applied_n` times the post-storage manure C:N (the solid/liquid/excreta value
#' for the stream's management system), so the applied C:N reflects storage, not
#' fresh excreta; the carbon and volatile-solids storage losses follow from that.
#' The grazing stream undergoes no storage and keeps its full carbon and volatile
#' solids (no storage C:N cap is applied to it).
#'
#' @param split A tibble from [split_manure_management()]. The `dm_bedding`,
#'   `n_bedding` and `c_bedding` columns are optional and default to zero, so a
#'   `split` produced before bedding existed behaves exactly as it did.
#' @param options A named list. `method` selects the loss method
#'   (`"ipcc_2019_tier2"`). `bedding_carbon` selects what happens to the
#'   bedding carbon in storage, and only matters once bedding is non-zero:
#'   * `"cap_at_stored_cn"` (default): the cap is applied to excreta plus
#'     bedding carbon together, `applied_c = min(c_stream + c_bedding,
#'     applied_n * post-storage C:N)`. This is the published behaviour and it
#'     is worth knowing what it implies: on WHEP's own coefficients the cap
#'     already **binds** for the solid systems, so today's applied C:N for
#'     `Solid Storage` and `Daily Spread` is already exactly the bedded
#'     farmyard-manure value (cattle 20.16 against excreta 19.07), reached by
#'     discarding 30% of the excreted carbon rather than by adding straw.
#'     Adding bedding under this rule therefore raises the applied carbon only
#'     through the bedding **nitrogen** it brings, and books the rest of the
#'     straw carbon as a storage loss.
#'   * `"additive"`: the cap is applied to the excreta carbon alone and the
#'     bedding carbon is added on top of it untouched,
#'     `applied_c = min(c_stream, applied_n * post-storage C:N) + c_bedding`.
#'     No storage loss is applied to bedding carbon, which makes this an
#'     explicit upper bound rather than an estimate -- it needs no
#'     bedding-specific loss coefficient, because there is no sourced one.
#'
#'   Whichever rule is chosen, bedding is **not** added to the volatile solids:
#'   the `Bo`/`MCF` methane pathway downstream is calibrated on excreta
#'   volatile solids, and extending it to litter is a separate decision
#'   (whep#1005).
#'
#' @return The input rows with `manure_type`, `applied_n`, `applied_c`,
#'   `applied_vs`, `n_volatilized`, `n_leached`, `n2o_direct_n`, `n2_n`,
#'   `n2o_indirect_n`, `c_lost`, `vs_destroyed`, `method_losses` and
#'   `method_bedding_carbon`.
#' @export
#' @examples
#' excretion <- tibble::tribble(
#'   ~year, ~territory, ~sub_territory, ~livestock_category,
#'   ~n_excretion, ~c_excretion, ~vs_excretion,
#'   2020L, "203", NA, "Cattle_milk", 100, 1900, 60
#' )
#' apply_management_losses(split_manure_management(excretion))
apply_management_losses <- function(split, options = list()) {
  opt <- utils::modifyList(
    list(method = "ipcc_2019_tier2", bedding_carbon = "cap_at_stored_cn"),
    options
  )
  if (!identical(opt$method, "ipcc_2019_tier2")) {
    cli::cli_abort("Unknown {.arg method} {.val {opt$method}}.")
  }
  bedding_carbon <- opt$bedding_carbon
  opt$bedding_carbon <- rlang::arg_match(
    bedding_carbon,
    c("cap_at_stored_cn", "additive")
  )
  .check_split_cols(split)
  split <- .fill_bedding_columns(split)

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

  # Bedding nitrogen enters the heap with the excreta and is stored with it, so
  # it carries the same management-loss fractions. `n_bedding` is 0 unless
  # bedding was supplied, which is why this is identical to the excreta-only
  # arithmetic by default (whep#1005).
  out <- out |>
    dplyr::mutate(n_managed = .data$n_stream + .data$n_bedding) |>
    dplyr::mutate(
      n_volatilized = dplyr::if_else(
        .data$stream == "grazing",
        0,
        .data$n_managed * .data$frac_gas_ms
      ),
      n_leached = dplyr::if_else(
        .data$stream == "grazing",
        0,
        .data$n_managed * .data$frac_leach_ms
      ),
      n2o_direct_n = dplyr::if_else(
        .data$stream == "grazing",
        0,
        .data$n_managed * .data$ef3
      ),
      n2_n = .data$n2o_direct_n * n2_ratio,
      n2o_indirect_n = .data$n_volatilized * ef4 + .data$n_leached * ef5,
      applied_n = dplyr::if_else(
        .data$stream == "grazing",
        .data$n_managed,
        pmax(
          0,
          .data$n_managed -
            .data$n_volatilized -
            .data$n_leached -
            .data$n2o_direct_n -
            .data$n2_n
        )
      )
    ) |>
    dplyr::left_join(.mms_manure_type(), by = "mms_type") |>
    dplyr::left_join(
      dplyr::transmute(
        .manure_cn_coefs(),
        cn_species = .data$species,
        manure_type = .data$manure_type,
        cn_post = .data$cn_ratio
      ),
      by = c("cn_species", "manure_type")
    )
  if (anyNA(out$cn_post)) {
    cli::cli_abort("Missing post-storage C:N for some (species, manure_type).")
  }

  # `c_managed` is the carbon actually in the heap; `c_excreta_applied` is what
  # the cap lets through from the excreta alone, and it is what the volatile
  # solids follow, so `applied_vs` stays an excreta quantity under either
  # bedding rule. With `c_bedding == 0` every line below reduces to the
  # excreta-only arithmetic exactly (whep#1005).
  out |>
    dplyr::mutate(
      c_managed = .data$c_stream + .data$c_bedding,
      c_excreta_applied = dplyr::if_else(
        .data$stream == "grazing",
        .data$c_stream,
        pmin(.data$c_stream, .data$applied_n * .data$cn_post)
      ),
      applied_c = .applied_manure_c(
        .data$stream,
        .data$c_managed,
        .data$c_excreta_applied,
        .data$c_bedding,
        .data$applied_n * .data$cn_post,
        opt$bedding_carbon
      ),
      c_lost = .data$c_managed - .data$applied_c,
      applied_vs = dplyr::if_else(
        .data$c_stream > 0,
        .data$vs_stream * .data$c_excreta_applied / .data$c_stream,
        .data$vs_stream
      ),
      vs_destroyed = .data$vs_stream - .data$applied_vs,
      method_losses = opt$method,
      method_bedding_carbon = opt$bedding_carbon
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
      "method_losses",
      "method_bedding_carbon"
    )
}

# Bedding is optional on the way in: a `split` built before bedding existed, or
# by a caller that assembles the contract by hand, carries none. Zero-fill it so
# every formula below is the excreta-only one it always was, rather than making
# the columns required and breaking those callers.
.fill_bedding_columns <- function(split) {
  ensure_columns(
    tibble::as_tibble(split),
    tibble::tibble(
      dm_bedding = double(),
      n_bedding = double(),
      c_bedding = double()
    ),
    defaults = list(dm_bedding = 0, n_bedding = 0, c_bedding = 0),
    extra = "keep"
  )
}

# The two bedding-carbon rules. Both reduce to `pmin(c_stream, applied_n *
# cn_post)` when there is no bedding, because `c_managed == c_stream` and
# `c_bedding == 0` then.
.applied_manure_c <- function(
  stream,
  c_managed,
  c_excreta_applied,
  c_bedding,
  cap,
  rule
) {
  if (identical(rule, "additive")) {
    return(dplyr::if_else(
      stream == "grazing",
      c_managed,
      c_excreta_applied + c_bedding
    ))
  }
  dplyr::if_else(stream == "grazing", c_managed, pmin(c_managed, cap))
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
