# Species mix of the excreta deposited by grazing animals.
#
# IPCC 2019 Refinement, Vol 4, Ch 11, Table 11.1 (Updated), p. 11.13, gives
# urine and dung deposited on pasture, range and paddock two direct N2O
# emission factors: EF3PRP,CPP "for cattle (dairy, non-dairy and buffalo),
# poultry and pigs" and EF3PRP,SO "for sheep and 'other animals'", where the
# other animals include "goats, horses, mules, donkeys, camels, reindeer, and
# camelids" (footnote 10, same page). The emission from a cell's excreta is
# therefore the N-weighted blend of the two, and the blend needs the species
# mix of the N deposited there.
#
# Every excreta row of the nitrogen inputs carries that mix as
# `excreta_cpp_share`, the share of its N deposited by CPP animals (the SO
# share is one minus it), and `method_excreta_species`, which records where
# the mix came from or why a row has none. This file only carries the mix;
# the loss methods apply the factors.
#
# The classes are the table inst/extdata/manure/ef3prp_species_class.csv,
# keyed on the manure engine's `livestock_category` and on the FAOSTAT
# option's `species_group` (inst/extdata/livestock_mapping.csv), each row
# citing the Table 11.1 category it follows. The engine is keyed on
# `livestock_category` and not on `species_gen`: the engine gives rabbits the
# swine `species_gen` as an energy and manure-management proxy, which would
# book rabbit excreta as pig excreta.

# The `method_excreta_species` vocabulary. The first two name the source of a
# real mix; the last two are the declared reasons a row carries none:
# * "fallback_label_no_species": the engine labelled manure "Excreta" only as
#   the fallback of a manure-type split that found no share for it
#   (.reattach_manure_type(), .apply_type_split()), so the row is not grazing
#   deposition and no species detail exists for it.
# * "no_excreta_n": the row carries no excreta N, so a share is undefined.
.excreta_species_methods <- function() {
  c(
    engine = "engine_livestock_category",
    faostat = "faostat_species_group",
    fallback = "fallback_label_no_species",
    no_n = "no_excreta_n"
  )
}

.excreta_species_classes <- function() {
  system.file(
    "extdata",
    "manure",
    "ef3prp_species_class.csv",
    package = "whep"
  ) |>
    utils::read.csv(stringsAsFactors = FALSE) |>
    tibble::as_tibble()
}

# TRUE where a species is cattle, poultry or pigs (CPP), FALSE where it is
# sheep or other animals (SO). A missing species is an absent input and a
# species the table does not name is an unrecognised vocabulary: neither has
# a defensible class, so both abort rather than default to one.
.excreta_is_cpp <- function(species, vocabulary) {
  classes <- .excreta_species_classes()
  classes <- classes[classes$vocabulary == vocabulary, ]
  if (is.null(species) || anyNA(species)) {
    .abort_excreta_species_absent(
      "{.field {vocabulary}} is missing on grazing excreta that must be
       priced by species."
    )
  }
  unmapped <- setdiff(unique(as.character(species)), classes$species)
  if (length(unmapped) > 0L) {
    cli::cli_abort(
      c(
        "No IPCC 2019 EF3PRP class for {.field {vocabulary}}
         {.val {unmapped}}.",
        i = "{cli::qty(length(unmapped))}Add {?it/them} to
             {.file inst/extdata/manure/ef3prp_species_class.csv}, citing
             the Table 11.1 category {?it follows/they follow}."
      ),
      class = "whep_excreta_species_unmapped"
    )
  }
  classes$ef3prp_class[match(as.character(species), classes$species)] == "CPP"
}

.abort_excreta_species_absent <- function(
  problem,
  call = rlang::caller_env(),
  .envir = parent.frame()
) {
  cli::cli_abort(
    c(
      problem,
      x = "Without the species mix the excreta N still sums correctly, so no
           mass check downstream can see that its emission factor has no
           basis.",
      i = "Carry {.field livestock_category} (manure engine) or
           {.field species_group} (FAOSTAT option) to the excreta rows."
    ),
    class = c("whep_excreta_species_absent", "whep_absent_input"),
    call = call,
    .envir = .envir
  )
}

# Per-cell grazing N and the part of it deposited by CPP animals, read off the
# apply_management_losses() rows before allocate_manure_to_land() pools
# species away. The grazing stream's applied_n is its excreted N (no storage
# loss, no bedding), and the allocation deposits exactly that sum on the
# cell's grassland, so the ratio is the N-weighted mix of that row.
.engine_grazing_mix <- function(losses) {
  grazing <- dplyr::filter(
    tibble::as_tibble(losses),
    .data$stream == "grazing"
  )
  if (nrow(grazing) > 0L && !rlang::has_name(grazing, "livestock_category")) {
    .abort_excreta_species_absent(
      "The manure-management rows carry no {.field livestock_category}."
    )
  }
  grazing |>
    dplyr::mutate(
      cpp_n = .data$applied_n *
        .excreta_is_cpp(.data$livestock_category, "livestock_category")
    ) |>
    dplyr::summarise(
      graze_n = sum(.data$applied_n),
      cpp_n = sum(.data$cpp_n),
      .by = c("year", "territory", "sub_territory")
    )
}

# Stamp the species mix on the engine's applied rows. A grazing-deposition row
# takes its cell's grazing mix; an "Excreta" row of any other stream is a
# fallback label (see .excreta_species_methods()) and is stamped, with no
# share; every other manure type carries neither column.
.attach_excreta_species <- function(applied, losses) {
  methods <- .excreta_species_methods()
  out <- applied |>
    dplyr::left_join(
      .engine_grazing_mix(losses),
      by = c("year", "territory", "sub_territory"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      .grazing = .data$manure_type == "Excreta" &
        .data$source_stream == "grazing",
      excreta_cpp_share = dplyr::if_else(
        .data$.grazing,
        .data$cpp_n / .data$graze_n,
        NA_real_
      ),
      method_excreta_species = dplyr::case_when(
        .data$.grazing ~ methods[["engine"]],
        .data$manure_type == "Excreta" ~ methods[["fallback"]],
        .default = NA_character_
      )
    )
  .check_grazing_mix(out)
  dplyr::select(out, -"graze_n", -"cpp_n", -".grazing")
}

# Where the absence is created: a grazing-deposition row whose cell has no
# species mix is a broken join, not a herd without species.
.check_grazing_mix <- function(out) {
  bad <- out$.grazing & !is.finite(out$excreta_cpp_share)
  if (!any(bad)) {
    return(invisible(out))
  }
  mass <- signif(sum(out$applied_n[bad]), 6)
  .abort_excreta_species_absent(
    "{sum(bad)} grazing excreta row{?s} found no species mix ({mass} t N).",
    call = rlang::caller_env()
  )
}

# The excreta columns of the nitrogen-input contract. A table with no excreta
# needs neither; one whose excreta rows carry no stamp is refused, because a
# missing mix and a mix of zero CPP N are the same number downstream.
.ni_excreta_species_cols <- function(applied) {
  if (!rlang::has_name(applied, "excreta_cpp_share")) {
    applied$excreta_cpp_share <- rep(NA_real_, nrow(applied))
  }
  if (!rlang::has_name(applied, "method_excreta_species")) {
    applied$method_excreta_species <- rep(NA_character_, nrow(applied))
  }
  unstamped <- applied$manure_type == "Excreta" &
    is.na(applied$method_excreta_species)
  if (any(unstamped)) {
    .abort_excreta_species_absent(
      "{sum(unstamped)} excreta row{?s} {?carries/carry} no
       {.field method_excreta_species}."
    )
  }
  applied
}

# N-weighted share over the rows of one output row. Defined only when every
# row carrying N has a share: one row of unknown species would otherwise be
# priced as though it had the mix of the rest.
.excreta_weighted_share <- function(share, n) {
  positive <- !is.na(n) & n > 0
  if (!any(positive) || anyNA(share[positive])) {
    return(NA_real_)
  }
  sum(share[positive] * n[positive]) / sum(n[positive])
}

# The stamp of one output row, consistent with .excreta_weighted_share(): the
# reason of the rows with N and no share when there are any, else the source
# of the mix. A row with excreta but no N is stamped "no_excreta_n".
.excreta_method_stamp <- function(method, share, n) {
  present <- !is.na(method)
  if (!any(present)) {
    return(NA_character_)
  }
  positive <- present & !is.na(n) & n > 0
  if (!any(positive)) {
    return(.excreta_species_methods()[["no_n"]])
  }
  unknown <- positive & is.na(share)
  pick <- if (any(unknown)) method[unknown] else method[positive]
  paste(sort(unique(pick)), collapse = "|")
}

# The FAOSTAT option's mix, from per-species N already summed to the output
# grain as `n_input_t` and its CPP part `cpp_n_t`.
.ni_faostat_species_mix <- function(rows) {
  methods <- .excreta_species_methods()
  rows |>
    dplyr::mutate(
      excreta_cpp_share = dplyr::if_else(
        .data$n_input_t > 0,
        .data$cpp_n_t / .data$n_input_t,
        NA_real_
      ),
      method_excreta_species = dplyr::if_else(
        .data$n_input_t > 0,
        methods[["faostat"]],
        methods[["no_n"]]
      )
    ) |>
    dplyr::select(-"cpp_n_t")
}

# CPP N of FAOSTAT per-species rows (`species_group`, `n_col`).
.ni_faostat_cpp_n <- function(rows, n_col) {
  if (nrow(rows) > 0L && !rlang::has_name(rows, "species_group")) {
    .abort_excreta_species_absent(
      "The FAOSTAT pasture manure rows carry no {.field species_group}."
    )
  }
  dplyr::mutate(
    rows,
    cpp_n_t = .data[[n_col]] *
      .excreta_is_cpp(.data$species_group, "species_group")
  )
}
