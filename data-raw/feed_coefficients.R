# Builds the feed coefficient package datasets from inst/extdata/feed CSVs.
# Source: afsetools Codes_coefs.xlsx (conv_bouwman, conv_krausmann, Cats, items_full)
# and Livestock_coefs.xlsx (max_intake), extracted to CSV via Python because R xlsx
# readers segfault on that external-reference workbook. See inst/extdata/feed/.

feed_dir <- here::here("inst", "extdata", "feed")
harm_dir <- here::here("inst", "extdata", "harmonization")

# 1. conv_bouwman: Bouwman et al. 2005 feed conversion (kg DM feed / kg product) and
#    per-feedtype composition, by species x feedtype x Bouwman region x anchor year.
conv_bouwman <- file.path(feed_dir, "conv_bouwman.csv") |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::transmute(
    item_bouwman,
    feed_type = feedtype,
    year,
    region_bouwman = region,
    conversion
  )

# 2. conv_krausmann: Krausmann et al. 2013 intake (kg DM / head / yr) for draft and
#    non-productive species that lack a product-based feed conversion.
conv_krausmann <- file.path(feed_dir, "conv_krausmann.csv") |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::transmute(
    item_cbs_code = item_code_cbs,
    species = item_cbs,
    conversion
  )

# 3. feed_taxonomy: item -> feed_group (crop class), feed_quality class, per-consumer
#    feedtype labels (granivores get a restricted set; only grazers eat fibrous roughage),
#    a priority rank, and a Zoot_fixed flag (intake equals demand regardless of supply).
items_lookup <- file.path(harm_dir, "items_full.csv") |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::distinct(item_cbs, item_cbs_code)

feed_cats <- file.path(feed_dir, "feed_cats.csv") |>
  readr::read_csv(show_col_types = FALSE)

quality_rank <- tibble::tribble(
  ~feed_quality, ~feed_quality_rank,
  "lactation", 1L,
  "high_quality", 1L,
  "low_quality", 2L,
  "residues", 3L,
  "grass", 4L
)

feed_taxonomy <- file.path(feed_dir, "feed_items.csv") |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::left_join(items_lookup, by = "item_cbs") |>
  dplyr::left_join(feed_cats, by = "Cat_1") |>
  # A few special feed types (Draught, Scavenging) have no feed quality class in
  # the Cats sheet; fall back to the lowercased feed group for them.
  dplyr::mutate(
    feed_quality = dplyr::coalesce(
      stringr::str_to_lower(Cat_feed),
      stringr::str_to_lower(Cat_1)
    )
  ) |>
  dplyr::left_join(quality_rank, by = "feed_quality") |>
  dplyr::transmute(
    item_cbs_code,
    item_cbs,
    feed_group = Cat_1,
    feed_quality,
    feed_quality_rank,
    granivore_feedtype = feedtype_graniv,
    grazer_feedtype = feedtype_grazers,
    zoot_fixed = feed_quality == "zoot_fixed"
  )

# 4. max_intake_share: per livestock-category diet-share caps (per item or per feed class).
max_intake_share <- file.path(feed_dir, "max_intake_share.csv") |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::transmute(
    livestock_category = Livestock_cat,
    var,
    var_value,
    max_intake_share
  )

usethis::use_data(
  conv_bouwman,
  conv_krausmann,
  feed_taxonomy,
  max_intake_share,
  overwrite = TRUE
)
