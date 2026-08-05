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
# Inert for the COMMITTED snapshot and not for long. The committed `polities.rda` holds
# one `superseded` polity and it is not in the crosswalk, so regenerating against it
# changes no byte -- which is why this commit carries no `data/` diff. Run against the
# current upstream database it already excludes 27, and 22 of those are crosswalk
# candidates, so the filter becomes load-bearing the moment the snapshot is refreshed
# (#485). Measured rather than assumed: regenerating against the current upstream
# database reports excluding 27 dead polities and retaining 713.
#
# It does NOT fix every ambiguity. Two polities can be live and still overlap --
# Montenegro's MNE-1913-1915 and MNE-1913-1918 both cover 1913-1914 and are both
# marked draft upstream. No downstream filter can resolve that; it is filed upstream
# as whep-polities issue 62. The conflict detector added alongside this finds the
# class, and test_polity_resolution_uniqueness.R pins the known instance.
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
      polity_code = .data$iso3c,
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
# ONE DELIBERATE EXCEPTION: the FABIO Rest-of-World fold still outranks the map.
# 31 map-covered areas carry `fabio_code == 999` and therefore resolve to
# `ROW-1850-2023` today (Syria, North Macedonia, Eswatini, New Caledonia,
# French Guiana, Palestine and 25 more), even though the map names a real polity
# for each. Letting the map win there would move every Rest-of-World figure, and
# that fold is tracked separately as #419/#414. It is left standing here on
# purpose so this change stays confined to the mapping defect.
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
# an exclusive end would make them empty. And 51 of the 53 consecutive-span
# transitions satisfy `year_start == previous year_end + 1`, which an exclusive
# reading would turn into a one-year hole every time; the 2 exceptions (areas 205
# and 240) repeat the boundary year because their `registry` rows copy the
# polity's own inclusive start and end. Both exceptions are Rest-of-World folded,
# so neither reaches the join below.
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
    reporting_polity_code = .data$polity_code,
    reporting_polity_name = .data$polity_name,
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
      .data$reporting_polity_code %in% known_polity_prefixes,
      .data$reporting_polity_code,
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
      .data$reporting_polity_code,
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
# what keeps `BLX-1921-1999` (1921-1999, overlapping the map's 1961-1999 span for
# area 15) out while keeping `AFG-1800-1893` in.
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

outside_map_areas <- prefix_candidates |>
  dplyr::anti_join(shadowed_by_map, by = c("area_code", "polity_code")) |>
  dplyr::mutate(mapping_source = "prefix_outside_map")

polity_area_crosswalk <- dplyr::bind_rows(
  mapped_areas,
  prefix_areas,
  outside_map_areas
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
    mapping_status = dplyr::case_when(
      .data$decided_by_hand & !is.na(.data$polity_code) ~ "manual",
      !is.na(.data$polity_code) ~ "matched",
      is.na(.data$area_code) ~ "not_a_reporting_area",
      TRUE ~ "unmapped"
    ),
    mapping_note = dplyr::case_when(
      !is.na(.data$manual_note) ~ .data$manual_note,
      !is.na(.data$fabio_code) &
        .data$fabio_code == 999L &
        .data$area_code !=
          999L ~ "FABIO collapses this source area into the Rest of World reporting polity.",
      .data$mapping_status ==
        "unmapped" ~ "No real WHEP polity is available yet; treat this as a statistical reporting area without a polygon.",
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
    reporting_polity_code,
    reporting_polity_name,
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

shadowed_areas <- polity_area_crosswalk |>
  dplyr::filter(
    .data$mapping_source == "fabio_row_fold",
    .data$area_code %in% faostat_area_map$area_code
  ) |>
  dplyr::distinct(.data$area_code) |>
  dplyr::pull(.data$area_code) |>
  sort()
if (length(shadowed_areas) > 0L) {
  cli::cli_inform(c(
    "!" = "{length(shadowed_areas)} reporting area codes are named by the
           upstream FAOSTAT map but kept on the FABIO Rest-of-World fold.",
    "i" = "Areas: {.val {shadowed_areas}}.",
    "i" = "Lifting the fold is tracked separately and is out of scope here."
  ))
}

source_counts <- table(polity_area_crosswalk$mapping_source)
cli::cli_inform(c(
  "Built {nrow(polity_area_crosswalk)} crosswalk rows from
   {nrow(faostat_area_map)} upstream map rows covering
   {dplyr::n_distinct(faostat_area_map$area_code)} areas.",
  "i" = "Rows by mapping source:
         {paste0(names(source_counts), ' ', as.integer(source_counts),
         collapse = ', ')}."
))

usethis::use_data(items_cbs, overwrite = TRUE)
usethis::use_data(items_prod, overwrite = TRUE)
usethis::use_data(polities, overwrite = TRUE, compress = "xz")
usethis::use_data(polity_area_crosswalk, overwrite = TRUE)
usethis::use_data(polity_label_aliases, overwrite = TRUE)
