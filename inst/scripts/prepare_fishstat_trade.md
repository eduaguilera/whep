# Prepare classified FishStat source observations

This produces `fishstat_classified_raw_v1`, an **unconverted research
artifact**, not a replacement `fishstat-trade` pin. It addresses the source
classification and provenance parts of WHEP #1616 and supplies auditable
inputs for #1617. Both issues remain open for the standardized
product-to-account boundary. The CBS reader rejects this artifact, including
partial weight metadata. Legacy pins remain readable; their known
classification and product-weight limitations are not thereby repaired.

From the repository root, obtain the official release and build into a new
output directory:

```r
source("inst/scripts/download/download_fishstat.R")
download_fishstat("/path/to/raw-data")
```

```sh
Rscript inst/scripts/prepare_fishstat_trade.R \
  /path/to/raw-data/FI_Trade_2025.1.0 /path/to/new-output
```

The downloader obtains the exact FAO release below and extracts data plus
its classifications, unit definitions and country notes. Both scripts refuse
to overwrite an existing destination. The preparer checks the exact input
files against audited-byte fingerprints. These checks establish byte
identity, not scientific fitness or a publisher endorsement. The reference
checksums were measured from independently retrieved official files.

The output directory contains `fishstat-classified-raw.parquet` and
`manifest.json`. Every selected import/export `Q_tpw` observation is retained,
including held classifications and unresolved temporal coverage. Commodity
code, ISSCAAP group, source flag, measure, product form and original value
remain separate columns; missing quantities are not summed into zero.
Canonical polity identity comes from the loaded WHEP resolver tables, with
coverage status preserved. The reporter-to-area correspondence inherited from
PR #1628 remains provisional; unresolved territorial work stays in #1618 and
#458. A classified item code is a proposed category,
not proof that the row is eligible for a CBS account.

The manifest fingerprints source bytes, the bridge, actual loaded polity
objects, the package data payload, runtime code signatures and preparer code.
Ambient `WHEP_POLITIES_*` file hashes are labelled as build-input references;
their presence does not prove that the running resolver consumed those
files. Package version alone is insufficient to identify a local snapshot.
The artifact is read back and compared exactly before a manifest is written.
A directory left without a manifest after failure is not an accepted result.

## Classification versus accounting

The official classification supports correcting the old species bridge:
diadromous groups 21–25 belong with freshwater/diadromous fish; groups 31–34
and 38 are demersal; 76/77 are invertebrates; and 91–94 are aquatic plants.
The old bridge's labels for mammals 61–64 and miscellaneous products 81–83
were also wrong. Product form must be read separately: cod liver oil has
commodity code `411.11.5.2.19` while retaining cod-group ISSCAAP 32.

The explicit species correspondence is a mechanical correction. Scope and
standardization remain science decisions. Mammals, nonfood materials,
ambiguous oils, offal meals and explicitly unfit algae remain `held_scope`.
The Yearbook A.8 table includes coral/sponges whereas its food-scope prose
excludes miscellaneous products; this conflict is held rather than silently
resolved. The source's asterisk exclusion concerns capture/aquaculture
summary tables, not every trade account.

Meals retain their species category **and** their meal product form. This
neither assigns every meal to a single CBS destination nor supplies a
live-weight conversion. The historical FAO workflow uses commodity mappings,
extraction trees, country/period link tables and different FAOSTAT/FIAS
accounting treatments. Its historical `map_isscfc` lacks eight commodity codes
actually observed in the 2025 release (2,678 observations), so it is not a
complete current replacement. No historical extraction factor is adopted.

`Q_tpw` is net product weight. It is not live weight, even where the species
category is correct. The remaining work is to establish product-specific
conversion and account-scope rules, preserve their applicability and
uncertainty, compare against independently justified totals, and explicitly
validate the standardized consumer contract. Numerical closure alone cannot
prove source coverage or scientific correctness.

## Sources and measured scope

- [FAO FI_Trade_2025.1.0](https://www.fao.org/fishery/static/Data/FI_Trade_2025.1.0.zip):
  `CL_FI_COMMODITY_ISSCFC.csv`, `ISSCAAP.pdf`, `ISSCFC.pdf`, `FSJ_UNIT.csv`,
  `Trade_countries_notes.pdf` and `ReadMe.txt`. ZIP SHA256:
  `69afcd65bde6c860603808bf2c550541403049d0ea934e8b0161dd81b6ec72fb`.
- [FAO Yearbook 2021 (2024)](https://doi.org/10.4060/cc9523en): A.8, printed
  page 204, and the accompanying scope text.
- [FAO SWS methodology](https://sws-methodology.github.io/faoswsFisheriesSUAFBSdocumentation/):
  classification and standardization are separate stages.
- [Historical official implementation](https://github.com/SWS-Methodology/faoswsFisheryStandardization/tree/8b4e9c7ee78c039926f216724fd28fce0b1ffc7e):
  `Input_documents/map_isscfc.csv` and related country/period and extraction
  inputs. Historical implementation is not automatic authority for current
  coefficients or account scope.

A real dry run of the verified release preserves all 1,220,160 selected
observations and their exact product-weight sum. It classifies 1,188,614 rows
and holds 31,546; no observed commodity remains unmapped. With the packaged
1,329-polity cohort used for this review, 1,219,200 rows have adopted temporal
coverage and 960 are held as `polity_not_live`. Classification and territorial
coverage are separate statuses.

Across all source reporters, the revised classified categories move
10,941,399.53414301 import product tonnes and 11,224,406.270591002 export
product tonnes between categories in 2023. These are trade flows, not global
production or corrected live-weight balances. The source replay separately
reproduces all 99,031 frozen legacy pin rows exactly before classification,
which distinguishes the category comparison from changed reporter coverage.
No pin is published by this workflow.
