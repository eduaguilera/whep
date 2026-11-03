# Build a source/year reporting-support artifact

`build_reporting_support.R` writes a local native bundle and provenance
manifest, verifies production water/ice requirements, checks complete water-cell
coverage and inventory membership, then validates serialized readback. It has
no upload or registry-update path. It does **not** restore the legacy global
`polycell_support` pin by itself. Refs #1013, #1310, #1483 and #874.

Load the intended WHEP checkout with its sf dependencies and install `digest`
(the developer script uses it for SHA256 hashes). Source the script and call
`.rsa_build(config, output_dir)`. The output path must not exist as either a file or directory. Paths and required
attestations must be single nonempty strings; source-file labels must be unique.
`config`
is a list containing these paths:

- `inventory`: CSV with the `build_reporting_support()` inventory contract.
- `domain`: RDS of an sf domain with `year` and `domain_source`.
- `water`: RDS of a complete `lon`, `lat`, `water_frac` table for the domain.
- `ice`: RDS of the supplied source's sf ice layer.
- `provenance`: list containing nonempty `cohort_id`, `inventory_basis`,
  `domain_basis`, and named `source_files` paths. Add source revisions,
  layer methods and temporal limitations here too.

The script hashes input/source files; it does not establish the truth of a
provenance claim. Arbitrary caller-selected files and descriptive attestations
are not proof that an inventory or domain is complete, or that a cohort ID names
the loaded geometry. The manifest explicitly records this unverified status and
a separate hash of the loaded canonical geometry. Before running it, verify that the loaded package geometry
and identities match the intended upstream cohort. Include the upstream
manifest, embedded geometry data, source inventories, original physical-layer
files and derivation scripts among `source_files`. Keep private source paths in
the local manifest, not public documentation.

Supply a complete physical domain justified separately from the selected
observations. Do not derive it by summing whichever reporters survived a join.
It may include unreported territory, which remains in the audit and denominator.
Shared geometry ancestry is not independent evidence of historical borders.
Keep the domain fixed when comparing reporting selections. Report alternatives
with their explicit bases; do not remove canonical geometry or select identities
by numeric source-code availability to force a partition.

Water must contain explicit finite fractions for every domain-support cell;
zero is allowed when the source says dry. A sparse table with omitted cells is
not certified by this artifact driver. This is stricter than the legacy
polycell producer. Ice must pass the native production reader's existing
supplied-layer gate. Source acquisition and geographic/temporal completeness
still require appraisal; an input checksum is not that appraisal.

The output contains `reporting-support.rds` and `manifest.json`. The manifest
marks `published = FALSE` and `global_pin_restored = FALSE`. Read the bundle via
`read_reporting_support()`; use `role = "footprints"` for recoverable overlap
footprints, and the default partition for the selected source/year. Existing
`read_polycell_support(role = "overlap")` consumers are not automatically wired
to this artifact. Their migration and a full coherent pin rebuild remain
separate, explicitly tracked work.

Offline driver regressions (no pins or external source files):

```r
pkgload::load_all()
source("inst/scripts/verify_reporting_artifact.R")
```

These developer scripts are excluded from the built package; the regression
script is run explicitly in the source checkout. Package-native bundle
validation remains covered by `test_reporting_support.R`.


Acquire the physical inputs reproducibly before configuring their paths:

```r
source("inst/scripts/download/download_hydrology.R")
download_hydrology(dest_dir) # GLWD/GLWD_v2/ plus the DRT input.
source("inst/scripts/download/download_naturalearth.R")
download_naturalearth(dest_dir) # NaturalEarth/ne_10m_glaciated_areas/.
water <- whep::read_glwd_water(dir = dest_dir)
ice <- whep::read_glaciated_areas(file.path(dest_dir, "NaturalEarth"))
```

GLWD v2.0 is deposited at
[Figshare, version 1](https://doi.org/10.6084/m9.figshare.28519994.v1).
The downloader selects the combined-class archive
[file 54001814](https://ndownloader.figshare.com/files/54001814), whose
publisher-reported MD5 is `aea80ff46211b349ffbaa871442fd0ed`.
Keep the archive and verify its checksum, then retain the class and area-percent
rasters and their hashes with the derivation. A bounded derivation must crop
whole half-degree cells and retain the same class/fraction method, not sample
only the reported territories.

Natural Earth's [glaciated-areas source page](https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-glaciated-areas/)
links the official [10m physical archive](https://naciscdn.org/naturalearth/10m/physical/ne_10m_glaciated_areas.zip).
Preserve all shapefile companions, version metadata and checksums. The CDN URL
is not version-frozen, so a new download must be compared with the intended
artifact's manifest. This layer includes recently deglaciated areas and derives
from older mapping; its supplied presence does not establish year-specific
ice coverage. The source page and archive metadata can also differ in their
version labels. Record the actual files rather than inferring version from the
webpage alone.
