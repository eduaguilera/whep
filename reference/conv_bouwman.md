# Bouwman feed conversion ratios.

Feed conversion (kilograms dry matter feed per kilogram product) and per
feed type composition for the main production species, from Bouwman et
al. (2005). Migrated from the afsetools `Codes_coefs.xlsx` workbook.

## Usage

``` r
conv_bouwman
```

## Format

A tibble with one row per species, feed type, region and anchor year:

- item_bouwman:

  Bouwman species label (Beef cattle, Dairy cattle, Pigs, Poultry, Sheep
  and goats).

- feed_type:

  Feed type: animals, crops, grass, residues or scavenging.

- year:

  Anchor year (1970, 1995 or 2030).

- region_bouwman:

  Bouwman seventeen region label.

- conversion:

  Per feed type conversion factor (kg DM feed per kg product). Sum
  across feed types is the total feed conversion ratio; normalised
  across feed types it gives the default feed composition shares.

## Source

Bouwman et al. (2005), via afsetools `Codes_coefs.xlsx`.
