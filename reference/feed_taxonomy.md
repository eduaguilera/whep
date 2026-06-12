# Feed taxonomy.

Maps each feed item to its feed group, feed quality class, per consumer
feed type labels and an allocation priority. Granivores get a restricted
feed type set, so only grazers take fibrous roughage. Migrated from the
afsetools `Codes_coefs.xlsx` workbook.

## Usage

``` r
feed_taxonomy
```

## Format

A tibble with one row per feed item:

- item_cbs_code:

  FAOSTAT commodity balance item code.

- item_cbs:

  Item name.

- feed_group:

  Feed group (crop or material class).

- feed_quality:

  Feed quality class: lactation, high_quality, low_quality, residues,
  grass, zoot_fixed or non_feed.

- feed_quality_rank:

  Allocation priority rank (lower is allocated first): lactation and
  high_quality 1, low_quality 2, residues 3, grass 4.

- granivore_feedtype:

  Feed type the item provides to granivores.

- grazer_feedtype:

  Feed type the item provides to grazers.

- zoot_fixed:

  Whether intake equals demand regardless of supply (compound feed
  ingredients that are not substitutable).

## Source

afsetools `Codes_coefs.xlsx`.
