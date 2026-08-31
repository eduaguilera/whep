items_cbs <- here::here("inst", "extdata", "items_cbs.csv") |>
  readr::read_csv()

items_prod <- here::here("inst", "extdata", "items_prod.csv") |>
  readr::read_csv()

whep_polities_gpkg <- Sys.getenv(
  "WHEP_POLITIES_GPKG",
  unset = path.expand("~/whep-polities/data/final/polities_database.gpkg")
)

polities <- sf::st_read(whep_polities_gpkg, quiet = TRUE)
polities$iso3c <- polities$iso3_code
polities$has_geometry <- !sf::st_is_empty(polities)

polity_attrs <- polities |>
  sf::st_drop_geometry() |>
  dplyr::mutate(polity_prefix = sub("-.*", "", .data$polity_code)) |>
  dplyr::select(
    polity_prefix,
    polity_code,
    polity_name,
    polity_start_year = start_year,
    polity_end_year = end_year,
    polity_type,
    iso3_code,
    cow_code,
    continent,
    wiki_status,
    polygon_status,
    has_geometry
  )
known_polity_prefixes <- unique(polity_attrs$polity_prefix)

# A DEAD POLITY MUST NOT BE A RESOLUTION CANDIDATE.
#
# Upstream retires a polity when a finer split supersedes it -- `F248-1920-1991`
# was retired once `F248-1920-1947` and `F248-1947-1991` replaced it -- and marks
# that in `wiki_status`. We carried the column through and never filtered on it, so
# a retired polity stayed a candidate and `(area, year)` resolution had two answers
# for the same year. Which one `add_polity_code()` returned depended on row order.
#
# Upstream already draws this line and publishes it both ways: the manifest carries
# `counts = {total, live, dead}`, and whep-polities' own matcher reports "excluded
# from matching: 26 dead polities (retired/superseded)". We were the only consumer
# ignoring it.
#
# The filter belongs HERE and not on `polities`. The published `polities` table keeps
# every row, because looking up a retired code is legitimate -- a consumer holding
# historical output needs to resolve the code it already has. What must not happen is
# resolving TO one.
#
# It is LOAD-BEARING as of the #530 re-sync, having been latent before it. The old
# snapshot held 27 dead polities; this one holds 41, and 14 codes that were live in the
# old one are dead in this one -- `BLX-1921-1999`, `CAN-1886-1948`, `CAN-1948-2025`,
# `MOR-1956-1958`, `SER-2006-2008`, `RLAM-1850-2013`, the five `R*-1850-2021` buckets,
# `ROW-1850-2023`, `MNE-1913-1915` and `PER-1825-1909`. Every one of those has a live
# replacement it would otherwise compete with. Measured rather than assumed: this run
# reports excluding 41 dead polities and retaining 716.
#
# It does not fix ambiguity between two LIVE overlapping periods, which is a different
# class and no downstream filter can resolve it. That used to be a real gap here --
# Montenegro's `MNE-1913-1915`/`MNE-1913-1918` and Peru's `PER-1825-1909` against
# `PER-1825-1884`/`PER-1884-1909`, filed upstream as whep-polities issue 62 -- and
# upstream has since retired/superseded the duplicate in each pair, so this filter now
# removes them. The conflict detector added alongside this still guards the class, and
# test_polity_resolution_uniqueness.R now asserts it is empty rather than pinning it.
live_polity_attrs <- polity_attrs |>
  dplyr::filter(
    is.na(.data$wiki_status) |
      !.data$wiki_status %in% c("retired", "superseded")
  )
dropped_dead <- nrow(polity_attrs) - nrow(live_polity_attrs)
if (dropped_dead > 0L) {
  cli::cli_inform(
    "Excluded {dropped_dead} retired/superseded polities from resolution
     candidates; {nrow(live_polity_attrs)} remain."
  )
}

excel_na <- c("", "NA", "#N/A", "#DIV/0!", "#REF!")

# repair_table_labels(): shared with harmonization_tables.R, which reads the same
# vendored regions_full.csv to build regions_full and polities_cats.
source("data-raw/_labels.R")
# The source-label -> polity map published by whep-polities
# (data/final/label_alias_map.csv, gated there by write_label_alias_map.py
# --check). Embedded rather than resolved at runtime, for the same reason
# `polities` is: a package function cannot depend on a sibling checkout existing.
#
# This exists because `add_polity_code()` resolves NUMERIC area codes and nothing
# resolved a country LABEL. Datasets carrying labels therefore had no supported
# path to a polity: mueller_synthetic_n's `iso3c` column holds FAO-style legacy
# codes (BZE, ROM, ZAR) and lassaletta_grassland_share's `Country` holds name
# variants (Cape Verde, Swaziland), and both simply went unresolved. Building a
# lookup here instead of consuming the published one would make this package a
# second authority for label -> polity, which is exactly what misattributed
# FAOSTAT data in #387.
whep_label_alias_map <- Sys.getenv(
  "WHEP_POLITIES_LABEL_ALIAS_MAP",
  unset = path.expand("~/whep-polities/data/final/label_alias_map.csv")
)

# Fail with an explanation rather than readr's bare "does not exist". This file
# is published by whep-polities and arrives with lbm364dl/whep-polities#39,
# whereas polities_database.gpkg is already on that repo's main -- so this is the
# one upstream artifact a regeneration can be missing, and the raw error names a
# path without saying what provides it.
if (!file.exists(whep_label_alias_map)) {
  cli::cli_abort(c(
    "The published label alias map is missing.",
    x = "Looked for {.path {whep_label_alias_map}}.",
    i = paste(
      "It is published by whep-polities as",
      "{.path data/final/label_alias_map.csv} and gated there by",
      "{.code scripts/write_label_alias_map.py --check}."
    ),
    i = paste(
      "If that repository is checked out elsewhere, point",
      "{.envvar WHEP_POLITIES_LABEL_ALIAS_MAP} at the file."
    ),
    i = paste(
      "The committed data/polity_label_aliases.rda already carries the",
      "aliases, so only regeneration is affected, not use."
    )
  ))
}

polity_label_aliases <- readr::read_csv(
  whep_label_alias_map,
  show_col_types = FALSE,
  na = excel_na,
  col_types = readr::cols(
    source_label = readr::col_character(),
    source = readr::col_character(),
    year_start = readr::col_integer(),
    year_end = readr::col_integer(),
    polity_code = readr::col_character(),
    common_name = readr::col_character(),
    confidence = readr::col_character(),
    # How many source rows were actually observed for this label, 0 when the
    # label is merely mappable. Declared explicitly because this col_types list
    # is exhaustive by intent -- an upstream column that is not named here is a
    # column this script cannot see.
    observed_rows = readr::col_double()
  )
)

# Every published alias must name a polity the upstream database carries.
# Upstream gates the same invariant, so a failure here means the alias map and
# the GeoPackage were taken from different revisions rather than that the map is
# wrong. Checked against the freshly read `polities`, not against the committed
# data/polities.rda, because the two are regenerated together from this script.
unknown_alias_targets <- setdiff(
  polity_label_aliases$polity_code,
  polities$polity_code
)
if (length(unknown_alias_targets) > 0L) {
  cli::cli_abort(c(
    "The published label alias map targets polities this package cannot carry.",
    x = "Unknown: {.val {utils::head(unknown_alias_targets, 5)}}.",
    i = "Rebuild from the same whep-polities revision that produced the map."
  ))
}

cli::cli_inform(paste0(
  "Loaded {nrow(polity_label_aliases)} published label aliases over ",
  "{length(unique(polity_label_aliases$source_label))} labels."
))

regions_full_raw <- here::here(
  "inst",
  "extdata",
  "harmonization",
  "regions_full.csv"
) |>
  readr::read_csv(show_col_types = FALSE, na = excel_na) |>
  repair_table_labels()

regions_compact <- here::here("inst", "extdata", "regions.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

regions_for_crosswalk <- dplyr::bind_rows(
  regions_full_raw,
  regions_compact |>
    dplyr::anti_join(
      regions_full_raw |>
        dplyr::filter(!is.na(.data$code)) |>
        dplyr::transmute(area_code = as.integer(.data$code)),
      by = "area_code"
    ) |>
    dplyr::transmute(
      legacy_polity_prefix = .data$iso3c,
      polity_name = .data$area_name,
      code = as.integer(.data$area_code),
      iso3c = .data$iso3c,
      FAOSTAT_name = .data$area_name,
      name = .data$area_name,
      cbs = FALSE,
      fabio_code = as.integer(.data$area_code),
      region = .data$region
    )
)

# UPSTREAM PUBLISHES THE AREA-TO-POLITY MAP. WE USED TO RE-DERIVE IT.
#
# `whep-polities` ships `faostat_area_polity_map.csv` as part of its consumer
# contract: 281 rows, 228 distinct FAOSTAT area codes, every row `high`
# confidence, each carrying the year span it applies to and a `match_route`
# recording how it was decided (`iso-equal` 249, `registry` 18, `manual-route` 8,
# `manual-replace` 5, `manual-span` 1). This package referenced it nowhere and
# instead inferred the same mapping from the polity code STRING, deriving a
# prefix with `sub("-.*", "", polity_code)` and joining areas to that.
#
# A prefix answers a different question than a curated map, and the answers
# differ in ways that are all wrong in the same direction -- silently:
#
#   * FAOSTAT area 72 Djibouti resolved to NOTHING. Its live polities are
#     `FRS-1884-1977` and `FRS-1977-2025`, which the map names, but no prefix
#     derived from `DJI` reaches `FRS`. The mapping had been travelling through
#     `DJI-1886-2025`, which upstream marks dead, so filtering dead polities
#     exposed a mapping the prefix could never have reached.
#   * FAOSTAT area 15 resolved to `BLX-1921-1999` because prefix `BLX` reaches
#     both that and `BLX-1850-1999` and the tie-break picked the later start.
#     The map names `BLX-1850-1999` for 1961-1999.
#   * FAOSTAT area 181 could not reach `SRH-1953-1964` for 1961-1963, and area
#     206 "Sudan (former)" could not reach `SUD-1956-2011` at all, because those
#     periods carry a different prefix from the area's ISO3. Both fell back to a
#     nearest-period stand-in of a state that did not exist in those years.
#   * `sub("-.*", "")` collapses `MMR-LWR-1852-1885` to `MMR`, so a unit that sat
#     INSIDE Myanmar joined as though it were a period of Myanmar itself. Five
#     polities carry that four-part shape, three of them typed `subnational`, and
#     all five entered the crosswalk this way.
#
# So the map is the authority now, joined on `area_code` and year. Prefix
# inference survives in two places, both narrowed and both labelled in
# `mapping_source`: for an area the map does not cover at all, which is reported
# below because the fallback firing is the signal that upstream needs a mapping;
# and for a period of a mapped area lying outside every span the map declares,
# which is what keeps pre-FAOSTAT history resolvable.
#
# THE REST-OF-WORLD FOLD USED TO OUTRANK THE MAP, AND NO LONGER DOES.
# 31 map-covered areas carry `fabio_code == 999` (Syria, North Macedonia,
# Eswatini, New Caledonia, French Guiana, Palestine and 25 more). The fold sent
# every one of them down the prefix branch with the literal prefix `ROW`, which
# DELETED the map's answer for the area: 36 of the map's 281 rows reached no
# crosswalk row at all. `.unfold_rest_of_world()` then promoted the member's
# numeric `polity_area_code` and had no territorial identity left to promote
# with it, so all 62 folded areas published under `ROW-1850-2025` -- an
# aggregate with `continent` "World" and no geometry (#717).
#
# Both answers are emitted now, distinguished by `mapping_source`, and
# `.unfold_rest_of_world()` picks one according to its mode:
#
#   `fabio_row_fold`      the bucket's answer, `ROW-1850-2025` over the whole
#                         span. Used where the mode re-folds the area, and where
#                         upstream names no polity for it.
#   `fabio_row_promoted`  the map's own rows for the area, year-spanned, each
#                         naming the real polity. Used where the mode promotes.
#
# Emitting both is what keeps `whep.unfold_rest_of_world = "none"` able to
# reproduce a number published under the fold: the fold row is still there,
# untouched, and dropping the promoted rows restores exactly the old crosswalk.
whep_polities_faostat_map <- Sys.getenv(
  "WHEP_POLITIES_FAOSTAT_MAP",
  unset = path.expand("~/whep-polities/data/final/faostat_area_polity_map.csv")
)
if (!file.exists(whep_polities_faostat_map)) {
  cli::cli_abort(c(
    "Upstream FAOSTAT area map not found at
     {.path {whep_polities_faostat_map}}.",
    "i" = "Point {.envvar WHEP_POLITIES_FAOSTAT_MAP} at the published
           {.file data/final/faostat_area_polity_map.csv}.",
    "x" = "This aborts rather than falling back to prefix matching, because a
           silent fallback is the defect being removed."
  ))
}

# THE MAP'S YEAR SPANS ARE INCLUSIVE ON BOTH ENDS, and that is checked against
# the file rather than assumed: polity codes use an EXCLUSIVE `end_year`, so
# reading one as the other loses a year at every transition. Two independent
# reads of the same file agree. Five rows have `year_start == year_end` (areas 4,
# 29, 184 and 226 for 1961, area 248 for 1991) and each carries observed data, so
# an exclusive end would make them empty. And all 53 consecutive-span transitions
# satisfy `year_start == previous year_end + 1`, which an exclusive reading would
# turn into a one-year hole every time. Two used to break it -- areas 205 and 240
# repeated the boundary year because their `registry` rows copied the polity's own
# inclusive start and end -- and upstream fixed both in whep-polities PR 204, which
# corrected 16 `registry` spans in all (13 of them a `year_end` of 2025 pulled back
# to the last year FAOSTAT reports). Every one of the 15 areas they cover is
# Rest-of-World folded, so none of it reaches the join below and no crosswalk row
# changed.
faostat_area_map <- readr::read_csv(
  whep_polities_faostat_map,
  show_col_types = FALSE
) |>
  dplyr::transmute(
    area_code = as.integer(.data$area_code),
    map_year_start = as.integer(.data$year_start),
    map_year_end = as.integer(.data$year_end),
    polity_code = as.character(.data$polity_code),
    map_match_route = as.character(.data$match_route)
  )

backwards_spans <- faostat_area_map |>
  dplyr::filter(.data$map_year_end < .data$map_year_start)
if (nrow(backwards_spans) > 0L) {
  cli::cli_abort(c(
    "The upstream FAOSTAT map carries spans that end before they start.",
    "x" = "Offending area codes: {.val {backwards_spans$area_code}}.",
    "i" = "The spans are read as inclusive on both ends."
  ))
}

# NO AREA MAY SILENTLY LOSE ITS MAPPING -- that is exactly how Djibouti went
# missing. A map row naming a polity this package cannot resolve is a contract
# break, so it stops the build instead of quietly dropping the row.
unresolvable_map_codes <- setdiff(
  faostat_area_map$polity_code,
  live_polity_attrs$polity_code
)
if (length(unresolvable_map_codes) > 0L) {
  cli::cli_abort(c(
    "The upstream FAOSTAT map names polity codes that are not live polities.",
    "x" = "Unresolvable: {.val {unresolvable_map_codes}}.",
    "i" = "Either the polities snapshot predates the map, or upstream retired a
           polity the map still points at."
  ))
}

# Only dissolved-state aggregates whose FAOSTAT reporting does NOT overlap their
# successor states in time belong here (Czechoslovakia -> Czechia/Slovakia in
# 1993, USSR -> successors in 1992, Yugoslav SFR -> successors in 1992): the
# aggregate is the sole China-style overlap, so mapping it is lossless. All three
# are now covered by the upstream map, which decided them by `manual-replace`,
# so these prefixes only matter if upstream ever drops one.
#
# FAOSTAT area 351 "China" is deliberately NOT mapped: it is an aggregate of
# 41 (mainland) + 96 (Hong Kong) + 128 (Macao) + 214 (Taiwan) reported ALONGSIDE
# those components for every year (1961-2024, full overlap). Those components
# already map to their own polities (CHN/HKG/MAC/TWN), so mapping 351 to CHN as
# well double-counted China across every FAOSTAT domain. Left unmapped, 351 is
# dropped as a statistical aggregate (its iso3c and polity_code are NA).
manual_area_prefixes <- tibble::tribble(
  ~area_code, ~manual_polity_prefix, ~manual_note,
  51L, "F51", "FAOSTAT Czechoslovakia reporting area maps to WHEP Czechoslovakia polities.",
  228L, "F228", "FAOSTAT USSR reporting area maps to WHEP Russian Empire/USSR polities.",
  248L, "F248", "FAOSTAT Yugoslav SFR reporting area maps to WHEP Yugoslavia polities."
)

reporting_areas <- regions_for_crosswalk |>
  dplyr::transmute(
    area_code = as.integer(.data$code),
    area_name = dplyr::coalesce(
      .data$FAOSTAT_name,
      .data$name,
      .data$polity_name
    ),
    area_iso3c = .data$iso3c,
    # NOT a polity code and NOT a polity name. This is the vendored ISO3-LIKE
    # STEM from regions_full.csv (`legacy_polity_prefix` since #687) and the
    # legacy label that ships beside it, used a few lines below only as a
    # candidate PREFIX for polity inference. Every real answer this script
    # emits is `polity_code`/`polity_name`, resolved from the upstream map.
    # Until whep#711 the pair was published as `reporting_polity_code` and
    # `reporting_polity_name` -- the package's own names for a real periodized
    # polity -- so the crosswalk answered the identity question with a stem.
    legacy_polity_prefix = .data$legacy_polity_prefix,
    legacy_polity_name = .data$polity_name,
    cbs = .data$cbs,
    fabio_code = as.integer(.data$fabio_code),
    region = .data$region
  ) |>
  dplyr::left_join(manual_area_prefixes, by = "area_code") |>
  dplyr::mutate(
    area_iso3c_prefix = dplyr::if_else(
      .data$area_iso3c %in% known_polity_prefixes,
      .data$area_iso3c,
      NA_character_
    ),
    reporting_prefix = dplyr::if_else(
      .data$legacy_polity_prefix %in% known_polity_prefixes,
      .data$legacy_polity_prefix,
      NA_character_
    ),
    fabio_row_prefix = dplyr::if_else(
      !is.na(.data$fabio_code) & .data$fabio_code == 999L,
      "ROW",
      NA_character_
    ),
    mapping_prefix = dplyr::coalesce(
      .data$manual_polity_prefix,
      .data$fabio_row_prefix,
      .data$area_iso3c_prefix,
      .data$reporting_prefix,
      # Keep these last so unmatched reporting buckets remain visible.
      .data$legacy_polity_prefix,
      .data$area_iso3c
    ),
    area_in_map = .data$area_code %in% faostat_area_map$area_code
  )

# THE PREFIX MAY ONLY REACH A CANONICAL `PREFIX-start-end` CODE. This is the
# subnational leak, fixed at its root rather than by listing the offenders:
# `sub("-.*", "")` on `MMR-LWR-1852-1885` yields `MMR`, which is not that
# polity's family -- it is the family of the entity the unit sat inside. Five
# polities carry the four-part shape (`AZE-SSR-1920-1991`, `IDN-BLB-1949-1951`,
# `IDN-JVM-1949-1951`, `IDN-OTH-1949-1951`, `MMR-LWR-1852-1885`), three of them
# typed `subnational`, and all five entered the crosswalk this way. A prefix
# carries no information about them, so inference must not reach them; only an
# explicit upstream map row may.
canonical_polity_attrs <- live_polity_attrs |>
  dplyr::filter(grepl("^[^-]+-[0-9]{4}-[0-9]{4}$", .data$polity_code))
dropped_noncanonical <- nrow(live_polity_attrs) -
  nrow(canonical_polity_attrs)
if (dropped_noncanonical > 0L) {
  cli::cli_inform(
    "Excluded {dropped_noncanonical} polities whose code is not
     {.code PREFIX-start-end} from prefix inference; the upstream map is the only
     route to them."
  )
}

mapped_areas <- reporting_areas |>
  dplyr::filter(.data$area_in_map, is.na(.data$fabio_row_prefix)) |>
  dplyr::inner_join(
    faostat_area_map,
    by = "area_code",
    relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    live_polity_attrs |> dplyr::select(!"polity_prefix"),
    by = "polity_code"
  ) |>
  dplyr::mutate(mapping_source = "upstream_map")

# THE MAP'S ANSWER FOR A REST-OF-WORLD MEMBER, kept rather than discarded.
#
# Same join as `mapped_areas` on the areas that branch excludes, so the two
# together consume the map exactly once: 245 rows over 197 areas there, 36 rows
# over 31 areas here, and the map has 281 rows over 228 areas. That identity is
# asserted in `test_polity_faostat_map.R` -- it is the property that says the
# fold no longer deletes an upstream statement, and it fails the moment either
# branch starts shadowing the other.
#
# `polity_area_code` still resolves to 999 below, because these rows describe a
# member of the bucket; `.unfold_rest_of_world()` is what promotes it.
row_promoted_areas <- reporting_areas |>
  dplyr::filter(.data$area_in_map, !is.na(.data$fabio_row_prefix)) |>
  dplyr::inner_join(
    faostat_area_map,
    by = "area_code",
    relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    live_polity_attrs |> dplyr::select(!"polity_prefix"),
    by = "polity_code"
  ) |>
  dplyr::mutate(mapping_source = "fabio_row_promoted")

# The prefix branch for everything the map does not decide: the Rest-of-World
# fold, the six regional "Other" buckets, the China aggregate that must stay
# unmapped, and the rows carrying no reporting area at all.
prefix_areas <- reporting_areas |>
  dplyr::filter(!.data$area_in_map | !is.na(.data$fabio_row_prefix)) |>
  dplyr::left_join(
    canonical_polity_attrs,
    by = c("mapping_prefix" = "polity_prefix"),
    relationship = "many-to-many"
  ) |>
  dplyr::mutate(
    mapping_source = dplyr::if_else(
      is.na(.data$fabio_row_prefix),
      "prefix_fallback",
      "fabio_row_fold"
    )
  )

# PERIODS OUTSIDE THE MAP'S REACH STAY REACHABLE. The map spans the years FAOSTAT
# actually reports, which begin in 1961, so taking it as the ONLY route would
# delete every pre-1961 period from the crosswalk -- and those are load-bearing:
# `.resolve_hist_trade_polities()` resolves genuine historical trade sources with
# the back-cast floor switched OFF precisely because they are reported under
# their own year's borders. Dropping them would not drop the rows, it would
# re-attribute an 1890 Austria figure to `AUT-1919-2025` as an out-of-span
# stand-in. So a prefix-derived period of a mapped area is kept when it overlaps
# NO span the map declares for that area, and dropped when it does -- which is
# what kept `BLX-1921-1999` (1921-1999, overlapping the map's 1961-1999 span for
# area 15) out while keeping `AFG-1800-1893` in. Since the #530 re-sync upstream
# has retired `BLX-1921-1999` outright, so that particular row is now excluded one
# step earlier by the dead-polity filter; the rule still governs every live period
# of a mapped area, which is where it earns its place.
#
# The comparison is deliberately mixed-convention: `polity_end_year` is EXCLUSIVE
# so a period covers `start:(end - 1)`, while the map's `map_year_end` is
# INCLUSIVE. Treating them alike would drop a period that merely abuts a span.
#
# AND NOT FOR A STATISTICAL COMPOSITE. Where every polity the map names for an
# area is typed `aggregate`, the area is a composite reporting unit rather than a
# territory -- FAOSTAT area 15 Belgium-Luxembourg and area 151 Netherlands
# Antilles are the two -- and the constituent history that shares its prefix is
# not the same territory. Inferring a period outside its mapped span would let a
# 2023 area-151 figure land on `ANT-1816-1960`, a colonial entity that ended 63
# years earlier. `.add_polity_columns_dt()` already refuses to extend aggregate
# reporting areas; this keeps the crosswalk from handing it a way around that.
composite_areas <- faostat_area_map |>
  dplyr::left_join(
    live_polity_attrs |> dplyr::select("polity_code", "polity_type"),
    by = "polity_code"
  ) |>
  dplyr::summarise(
    all_aggregate = all(.data$polity_type == "aggregate"),
    .by = "area_code"
  ) |>
  dplyr::filter(.data$all_aggregate) |>
  dplyr::pull(.data$area_code)

prefix_candidates <- reporting_areas |>
  dplyr::filter(
    .data$area_in_map,
    is.na(.data$fabio_row_prefix),
    !.data$area_code %in% composite_areas
  ) |>
  dplyr::left_join(
    canonical_polity_attrs,
    by = c("mapping_prefix" = "polity_prefix"),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(!is.na(.data$polity_code))

# Subsumes the codes the map itself names for the area: a mapped period always
# overlaps its own span, every map span having been checked to lie inside its
# polity's validity.
shadowed_by_map <- prefix_candidates |>
  dplyr::distinct(
    .data$area_code,
    .data$polity_code,
    .data$polity_start_year,
    .data$polity_end_year
  ) |>
  dplyr::inner_join(
    faostat_area_map |>
      dplyr::select("area_code", "map_year_start", "map_year_end"),
    by = "area_code",
    relationship = "many-to-many"
  ) |>
  dplyr::filter(
    .data$polity_start_year <= .data$map_year_end,
    .data$polity_end_year - 1L >= .data$map_year_start
  ) |>
  dplyr::distinct(.data$area_code, .data$polity_code)

# AND A POLITY THE MAP AWARDED OUTSIDE THIS FOLD IS NOT AVAILABLE.
# `shadowed_by_map` joins `by = "area_code"`, so it only ever asks whether a
# candidate period overlaps a span of ITS OWN area. Nothing asked the other
# question -- whether the upstream map has already named that polity somewhere
# else -- and the two Ethiopia codes are exactly the pair that slips between
# them (#741). The map is unambiguous, one high-confidence row each:
#
#   62  1961-1992  ETH-1952-1993  Ethiopia PDR
#   238 1993-2024  ETH-1993-2025  Ethiopia
#
# Both areas carry the `ETH` prefix, so each is a prefix candidate for the
# other's polity, and each escapes the same-area overlap test on a boundary
# year: `ETH-1993-2025` starts in 1993, which is not `<= 1992`, so it is not
# shadowed for area 62; `ETH-1952-1993` ends (exclusively) in 1993, so
# `1992 >= 1993` is false and it is not shadowed for area 238. Neither test can
# see the other area at all. The result was area 62 holding the polity 238 owns
# and vice versa.
#
# THE EXCEPTION IS THE BUCKET THE OWNER FOLDS INTO, and that is the whole
# subtlety, because it is asymmetric where the defect looks symmetric.
# `.bucket_area_labels()` resolves a published row's `reporting_polity_code` by
# handing `.add_polity_columns_dt()` the `polity_area_code` -- the BUCKET code
# -- not the reporting area's own code. Area 62 folds into bucket 238, so
# bucket 238 sums Ethiopia PDR's 1961-1992 data, and `(238, ETH-1952-1993)` is
# the only period bucket 238 has before 1993: it is what attributes those years
# to the entity that actually reported them. Removing it on the symmetric "a
# different AREA owns it" reading is not a no-op -- measured over 1850-2025 it
# sends 176 (area, year) pairs to `ETH-1993-2025` as an out-of-span stand-in,
# which is a published value moving. The reverse row has no such role: nothing
# folds into bucket 62, area 238 is its own bucket, and area 62 stopped
# reporting in 1992, so `(62, ETH-1993-2025)` answers a question no consumer
# can ask correctly. One row goes, one stays.
#
# So a candidate survives only if its area IS the owner (the map's own row,
# which `shadowed_by_map` already covers) or its area is the bucket the owner
# folds into. The map is one row per polity -- 281 rows, 281 distinct codes
# over 228 areas -- so ownership is a statement about upstream rather than a
# tie-break invented here. Only three areas fold into a bucket that is not
# their own code (62 into 238, and 276/277 into 206); the ROW fold never
# reaches this branch because `fabio_row_prefix` routes it to `prefix_areas`.
#
# Prefix inference remains the only route to the periods the map does not
# reach: `ETH-1800-1889` through `ETH-1941-1952` are named by no map row, so
# both areas keep all seven, which is what `.resolve_hist_trade_polities()`
# needs.
#
# THE ROW THAT STAYS IS NOT A BUCKET-ONLY ROW, and #742 proposed to mark it as
# one. That issue reads `(238, ETH-1952-1993)` as right for the bucket and
# wrong for the area -- "reporting area 238 does not exist before 1993" -- and
# asks for a `key_role` column so the raw-reporting-area lookup stops matching
# it. Measured before designing anything, that is not what the row does. Area
# 238 is exactly where `.iso3_area_code_bridge()` sends `ETH` (whep#719), and
# `.resolve_hist_trade_polities()` resolves genuine historical sources under
# their own year's borders with the back-cast floor off. On the shipped
# historical-trade pins that lands 149 published rows on area 238 at 1961, a
# real period hit inside 1952-1993 -- Ethiopia including Eritrea, which is the
# territory the 1961 value covers. Taking the row out of the area lookup sends
# all 149 to `ETH-1993-2025` / `out_of_span`, and because the `area` label is
# attached from the BUCKET, which would keep the row, a single published row
# would then read `area = "Ethiopia (1952-1993)"` beside
# `polity_code = "ETH-1993-2025"` -- the two-vocabulary split of whep#584.
#
# So the row answers in both key spaces, no shipped row answers in only one,
# and a `key_role` column would today be derivable as
# `area_code %in% polity_area_code` and carry no information. The distinction
# earns a published column when a bucket-only row first exists, which is what
# whep#414 would need (bucket 206 wants `F206-2011-2025`, a polity upstream
# publishes but its per-area map cannot name, because area 206 stops reporting
# in 2011). test_polity_faostat_map.R pins both halves so the split cannot be
# implemented on one of them.
map_owners <- faostat_area_map |>
  dplyr::distinct(.data$polity_code, owner_area = .data$area_code) |>
  dplyr::left_join(
    reporting_areas |>
      dplyr::distinct(owner_area = .data$area_code, .data$fabio_code),
    by = "owner_area"
  ) |>
  dplyr::transmute(
    .data$polity_code,
    .data$owner_area,
    owner_bucket = dplyr::coalesce(.data$fabio_code, .data$owner_area)
  )

owned_elsewhere <- prefix_candidates |>
  dplyr::distinct(.data$area_code, .data$polity_code) |>
  dplyr::inner_join(
    map_owners,
    by = "polity_code",
    relationship = "many-to-many"
  ) |>
  dplyr::filter(
    .data$area_code != .data$owner_area,
    .data$area_code != .data$owner_bucket
  ) |>
  dplyr::distinct(.data$area_code, .data$polity_code)

outside_map_areas <- prefix_candidates |>
  dplyr::anti_join(shadowed_by_map, by = c("area_code", "polity_code")) |>
  dplyr::anti_join(owned_elsewhere, by = c("area_code", "polity_code")) |>
  dplyr::mutate(mapping_source = "prefix_outside_map")

# A BUCKET THAT SUMS TWO TERRITORIES NEEDS A POLITY THAT MEANS BOTH, and the
# per-area map cannot name one.
#
# `polity_area_code` is an aggregation bucket. Every branch above answers per
# REPORTING AREA, which is the only thing upstream's map is about, and that is
# enough while a bucket has one reporting member per year. FAOSTAT bucket 206 is
# the one place it is not: FAOSTAT reports area 206 "Sudan (former)" through
# 2011 and areas 276 Sudan / 277 South Sudan from 2012, WHEP sums the two
# successors back into bucket 206, and from 2012 the bucket's value covers both
# territories while its own code resolves to `SUD-1956-2011` -- a polity that
# ended at the secession, reported `out_of_span` (whep#414, whep#860).
#
# Upstream has since minted the entity that means the sum:
# `F206-2011-2025` "Sudan and South Sudan (combined reporting)", `aggregate`,
# with a constructed 2,505,813 km2 polygon, following the same `F<area>` naming
# it uses for `F237-1954-1975` (Vietnam) and `F249-1918-1990` (Yemen). It has no
# map row and correctly so: as a statement about the reporting AREA 206 it would
# be false, because that area stops reporting in 2011. It is a statement about
# the BUCKET, so this package makes it, which is whep#860's third question.
#
# DERIVED, NOT TYPED. The rule is measured off the two upstream tables:
#
#   1. a bucket-year is a genuine multi-territory sum when the map names MORE
#      THAN ONE polity across the areas reporting into that bucket that year;
#   2. it is dishonestly labelled when the bucket's own code resolves to no
#      `aggregate` polity in that year;
#   3. it is answerable when a LIVE `aggregate` polity named `F<bucket>` covers
#      those years.
#
# All three are checked below, so a row appears only while upstream keeps
# publishing the polity, and buckets 1-2 apply to are reported whether or not
# rule 3 finds an answer. Measured on this revision the rules select exactly one
# bucket. Rule 1 alone selects two -- 206 for 2012-2024 and FABIO's
# Rest-of-World 999 for 1850-2024 -- and rule 2 drops 999, whose own code
# already resolves to the aggregate `ROW-1850-2025`.
#
# THE SHAPE IS UNUSUAL AND DELIBERATE: this aggregate is live at the same time
# as its own members, because areas 276 and 277 report separately from 2012
# while the bucket keeps summing them. That is not a defect to design around --
# `BLX-1850-1999` coexists with `BEL`/`LUX` for 149 years -- and the owner's
# rule is that a source having data for the combined polygon is what makes the
# combined polygon a polity. It does mean bucket 206 is the first bucket whose
# own aggregate polity shares its key space with its members' polities, which is
# why `test_polity_folds.R` now pins three live polities on that bucket key
# rather than two.
bucket_members <- reporting_areas |>
  dplyr::filter(!is.na(.data$area_code)) |>
  dplyr::transmute(
    .data$area_code,
    bucket = dplyr::coalesce(.data$fabio_code, .data$area_code)
  )

# One row per (bucket, year, polity) the upstream map reports into the bucket.
# Spans are inclusive on both ends, as checked further up.
bucket_reported_years <- faostat_area_map |>
  dplyr::inner_join(bucket_members, by = "area_code") |>
  dplyr::mutate(
    year = purrr::map2(.data$map_year_start, .data$map_year_end, seq.int)
  ) |>
  tidyr::unnest("year")

multi_polity_bucket_years <- bucket_reported_years |>
  dplyr::summarise(
    n_polities = dplyr::n_distinct(.data$polity_code),
    .by = c("bucket", "year")
  ) |>
  dplyr::filter(.data$n_polities > 1L)

# Rule 2: what the bucket's OWN code resolves to in that year, read off the
# area-keyed rows built above on the same convention the resolver uses --
# `polity_end_year` exclusive, extended to the inclusive `map_year_end`. The
# resolver's other widening, one year at the open end of an unsucceeded period,
# is deliberately NOT applied: it would need the succession relation here, and
# the only thing it could change is the terminal year of an aggregate that rule
# 3 must ALSO find an `F<bucket>` polity for. `ROW-1850-2025` is the one live
# case and there is no `F999`, so the two readings select the same bucket-years.
bucket_own_label_years <- dplyr::bind_rows(
  mapped_areas,
  row_promoted_areas,
  prefix_areas,
  outside_map_areas
) |>
  dplyr::filter(!is.na(.data$area_code), !is.na(.data$polity_code)) |>
  dplyr::semi_join(
    dplyr::distinct(multi_polity_bucket_years, bucket = .data$bucket),
    by = c("area_code" = "bucket")
  ) |>
  dplyr::transmute(
    bucket = .data$area_code,
    .data$polity_type,
    from_year = .data$polity_start_year,
    to_year = pmax(
      .data$polity_end_year - 1L,
      dplyr::coalesce(.data$map_year_end, .data$polity_end_year - 1L)
    )
  ) |>
  # `seq.int()` counts DOWN when its bounds cross, which would turn an empty
  # period into a reversed range rather than into nothing.
  dplyr::filter(.data$to_year >= .data$from_year) |>
  dplyr::mutate(
    year = purrr::map2(.data$from_year, .data$to_year, seq.int)
  ) |>
  tidyr::unnest("year")

unlabelled_bucket_years <- multi_polity_bucket_years |>
  dplyr::anti_join(
    bucket_own_label_years |>
      dplyr::filter(.data$polity_type == "aggregate") |>
      dplyr::distinct(.data$bucket, .data$year),
    by = c("bucket", "year")
  )

# Rule 3: upstream's `F<area>` convention names the polity that means what a
# FAOSTAT reporting code covers, which is what makes this attributable rather
# than hand-typed. Only `aggregate` polities qualify, and only live ones, since
# `live_polity_attrs` is already filtered.
faostat_named_aggregates <- live_polity_attrs |>
  dplyr::filter(.data$polity_type == "aggregate") |>
  dplyr::mutate(
    bucket = as.integer(
      stringr::str_match(.data$polity_code, "^F([0-9]+)-")[, 2]
    )
  ) |>
  dplyr::filter(!is.na(.data$bucket)) |>
  dplyr::select(
    "bucket",
    "polity_code",
    "polity_start_year",
    "polity_end_year"
  )

bucket_aggregate_candidates <- unlabelled_bucket_years |>
  dplyr::inner_join(
    faostat_named_aggregates,
    by = "bucket",
    relationship = "many-to-many"
  ) |>
  dplyr::filter(
    .data$year >= .data$polity_start_year,
    .data$year < .data$polity_end_year
  )

# One row per (bucket, polity), carrying the first year the row answers for.
# `applies_from_year` is what stops the row claiming a year the bucket is NOT a
# multi-member sum: `F206-2011-2025` begins in 2011, the year `SUD-1956-2011`
# ends, but bucket 206 at 2011 is still area 206 alone and upstream's own map
# decides it. Without the floor the resolver would see two candidates for 2011
# and pick by `polity_start_year DESC` -- a tie-break that fires on NOTHING in
# the shipped crosswalk today (measured: zero `(area, year)` pairs match two
# rows, in all three Rest-of-World modes), so letting this row be the first to
# depend on it would be deciding a published label by row-order convention.
bucket_aggregate_rows <- bucket_aggregate_candidates |>
  dplyr::summarise(
    applies_from_year = min(.data$year),
    .by = c("bucket", "polity_code")
  ) |>
  dplyr::inner_join(
    live_polity_attrs |> dplyr::select(!"polity_prefix"),
    by = "polity_code"
  ) |>
  dplyr::inner_join(
    reporting_areas |>
      dplyr::select(
        "area_code",
        "area_name",
        "area_iso3c",
        "legacy_polity_prefix",
        "legacy_polity_name",
        "cbs",
        "fabio_code",
        "region"
      ),
    by = c("bucket" = "area_code")
  ) |>
  dplyr::transmute(
    area_code = .data$bucket,
    # TAKEN FROM THE BUCKET'S OWN AREA ROW, never invented: `area_name` is the
    # label `.add_land_bucket_label()` gives the bucket, and a second value for
    # one `area_code` re-splits the sum the bucket exists to produce (whep#563).
    .data$area_name,
    .data$area_iso3c,
    .data$legacy_polity_prefix,
    .data$legacy_polity_name,
    .data$cbs,
    .data$fabio_code,
    .data$region,
    .data$polity_code,
    .data$polity_name,
    .data$polity_start_year,
    .data$polity_end_year,
    .data$polity_type,
    .data$iso3_code,
    .data$cow_code,
    .data$continent,
    .data$wiki_status,
    .data$polygon_status,
    .data$has_geometry,
    .data$applies_from_year,
    mapping_source = "whep_bucket_aggregate"
  )

# THE BUCKETS RULES 1-2 SELECT BUT RULE 3 CANNOT ANSWER are the standing ask,
# exactly like `unnamed_row_areas` below: the list should only ever shrink, and
# it shrinks by upstream minting the polity, not by this script inventing one.
unanswered_buckets <- setdiff(
  unique(unlabelled_bucket_years$bucket),
  bucket_aggregate_rows$area_code
)
if (length(unanswered_buckets) > 0L) {
  cli::cli_inform(c(
    "!" = "{length(unanswered_buckets)} reporting bucket{?s} sum{?s/} more than
           one territory in some year and {?is/are} labelled by no aggregate
           polity.",
    "i" = "Buckets: {.val {unanswered_buckets}}. Upstream would name each one
           {.val {paste0('F', unanswered_buckets, '-<start>-<end>')}}."
  ))
}
if (nrow(bucket_aggregate_rows) > 0L) {
  cli::cli_inform(c(
    "Bucket-aggregate rows added for {nrow(bucket_aggregate_rows)}
     bucket{?s}:",
    "i" = "{paste0(bucket_aggregate_rows$area_code, ' -> ',
           bucket_aggregate_rows$polity_code, ' from ',
           bucket_aggregate_rows$applies_from_year, collapse = ', ')}."
  ))
}

polity_area_crosswalk <- dplyr::bind_rows(
  mapped_areas,
  row_promoted_areas,
  prefix_areas,
  outside_map_areas,
  bucket_aggregate_rows
) |>
  dplyr::mutate(
    polity_area_code = dplyr::if_else(
      !is.na(.data$fabio_code),
      .data$fabio_code,
      .data$area_code
    ),
    # A curatorial decision stays labelled as one whether upstream made it (a
    # `manual-*` route in the published map) or this package did.
    decided_by_hand = !is.na(.data$manual_polity_prefix) |
      (!is.na(.data$map_match_route) &
        startsWith(.data$map_match_route, "manual")),
    # NO REPORTING AREA IS TESTED FIRST, and that ordering is the whole point.
    # `not_a_reporting_area` used to sit below `matched`, so it could only fire
    # for a row that had neither an `area_code` NOR a `polity_code` -- and no
    # such row exists. The 20 rows the value was written for (Aland, Saint
    # Barthelemy, Guernsey, Jersey, the Isle of Man and Sint Maarten, which
    # `regions_full` carries without a FAOSTAT code, plus the six regional
    # aggregate polities)
    # all match a polity, so every one of them shipped as `matched` while
    # `not_a_reporting_area` shipped on nothing. They carry `NA` in BOTH
    # `area_code` and `polity_area_code`, so no consumer can join reported data
    # to them; saying so is the honest label.
    mapping_status = dplyr::case_when(
      is.na(.data$area_code) ~ "not_a_reporting_area",
      .data$decided_by_hand & !is.na(.data$polity_code) ~ "manual",
      !is.na(.data$polity_code) ~ "matched",
      TRUE ~ "unmapped"
    ),
    mapping_note = dplyr::case_when(
      !is.na(.data$manual_note) ~ .data$manual_note,
      .data$mapping_source == "whep_bucket_aggregate" ~
        "WHEP's own row for the aggregation bucket, not for the reporting area: from `applies_from_year` the bucket sums several reporting areas and this aggregate polity is what their union means. Upstream's per-area map cannot state it because the bucket's own area stopped reporting.",
      # Tested on `mapping_source`, not on `fabio_code == 999`, because both
      # answers for a Rest-of-World member now carry that code and only one of
      # them is the fold.
      .data$mapping_source ==
        "fabio_row_promoted" ~ "Upstream names this polity for the area; it applies when the area is promoted out of the FABIO Rest of World bucket.",
      !is.na(.data$fabio_code) &
        .data$fabio_code == 999L &
        .data$area_code != 999L ~
        "FABIO collapses this source area into the Rest of World reporting polity.",
      .data$mapping_status == "not_a_reporting_area" ~
        "No FAOSTAT/FABIO reporting area exists for this territory, so nothing can be joined to this row; it records which polity the territory belongs to.",
      .data$mapping_status == "unmapped" ~
        "No real WHEP polity is available yet; treat this as a statistical reporting area without a polygon.",
      .data$mapping_status == "manual" ~ paste0(
        "Upstream FAOSTAT area map decided this period by hand, route ",
        .data$map_match_route,
        "."
      ),
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(
    area_code,
    area_name,
    area_iso3c,
    legacy_polity_prefix,
    legacy_polity_name,
    cbs,
    fabio_code,
    region,
    polity_area_code,
    polity_code,
    polity_name,
    polity_start_year,
    polity_end_year,
    polity_type,
    iso3_code,
    cow_code,
    continent,
    wiki_status,
    polygon_status,
    has_geometry,
    mapping_source,
    map_year_start,
    map_year_end,
    map_match_route,
    applies_from_year,
    mapping_status,
    mapping_note
  ) |>
  dplyr::arrange(.data$area_code, .data$polity_start_year, .data$polity_code)

# THE FALLBACK FIRING IS A FINDING, NOT A DETAIL. Every area named here is one
# upstream has not mapped, so it is resolved by inference; the list should only
# ever shrink, and test_polity_faostat_map.R pins it so it cannot grow unnoticed.
fallback_areas <- polity_area_crosswalk |>
  dplyr::filter(
    .data$mapping_source == "prefix_fallback",
    !is.na(.data$area_code)
  ) |>
  dplyr::distinct(.data$area_code) |>
  dplyr::pull(.data$area_code) |>
  sort()
if (length(fallback_areas) > 0L) {
  cli::cli_inform(c(
    "!" = "{length(fallback_areas)} reporting area codes are absent from the
           upstream FAOSTAT map and fell back to prefix matching.",
    "i" = "Areas: {.val {fallback_areas}}."
  ))
}

# EVERY MAP ROW MUST REACH THE CROSSWALK, and the fold is what used to stop 36
# of them. Asserted here as well as in the tests, because this is the one place
# that holds the map and the crosswalk at the same time.
consumed_map_rows <- polity_area_crosswalk |>
  dplyr::filter(
    .data$mapping_source %in% c("upstream_map", "fabio_row_promoted")
  ) |>
  nrow()
if (consumed_map_rows != nrow(faostat_area_map)) {
  cli::cli_abort(c(
    "The crosswalk consumes {consumed_map_rows} of the upstream map's
     {nrow(faostat_area_map)} rows.",
    "x" = "A map row reaching no crosswalk row is an upstream statement this
           package silently discards, which is #717.",
    "i" = "Every map row must land in {.val upstream_map} or
           {.val fabio_row_promoted}, exactly once."
  ))
}

# THE REST-OF-WORLD MEMBERS UPSTREAM STILL DOES NOT NAME are the remaining ask,
# and the list should only ever shrink. `row_promotion_status()` reports the
# same population at run time, split by whether a polity exists for the
# territory at all.
unnamed_row_areas <- polity_area_crosswalk |>
  dplyr::filter(
    .data$mapping_source == "fabio_row_fold",
    !is.na(.data$area_code),
    .data$area_code != 999L,
    !.data$area_code %in% faostat_area_map$area_code
  ) |>
  dplyr::distinct(.data$area_code) |>
  dplyr::pull(.data$area_code) |>
  sort()
if (length(unnamed_row_areas) > 0L) {
  cli::cli_inform(c(
    "!" = "{length(unnamed_row_areas)} Rest-of-World member area codes are
           named by no upstream FAOSTAT map row, so they keep the bucket's
           polity even when promoted.",
    "i" = "Areas: {.val {unnamed_row_areas}}."
  ))
}

source_counts <- table(polity_area_crosswalk$mapping_source)
# `mapping_status` alone does not say how confident a row is -- `matched` covers
# a curated upstream hit and a prefix guess alike -- so the build reports the
# PAIR. It is `mapping_status` x `mapping_source` that identifies the branch,
# and printing it here is what makes a drift in either one visible at build time.
status_source <- polity_area_crosswalk |>
  dplyr::count(.data$mapping_status, .data$mapping_source) |>
  dplyr::mutate(
    cell = paste0(.data$mapping_status, "/", .data$mapping_source, " ", .data$n)
  )
cli::cli_inform(c(
  "Built {nrow(polity_area_crosswalk)} crosswalk rows from
   {nrow(faostat_area_map)} upstream map rows covering
   {dplyr::n_distinct(faostat_area_map$area_code)} areas.",
  "i" = "Rows by mapping source:
         {paste0(names(source_counts), ' ', as.integer(source_counts),
         collapse = ', ')}.",
  "i" = "Rows by status/source: {paste0(status_source$cell, collapse = ', ')}."
))

usethis::use_data(items_cbs, overwrite = TRUE)
usethis::use_data(items_prod, overwrite = TRUE)
usethis::use_data(polities, overwrite = TRUE, compress = "xz")
usethis::use_data(polity_area_crosswalk, overwrite = TRUE)
usethis::use_data(polity_label_aliases, overwrite = TRUE)
