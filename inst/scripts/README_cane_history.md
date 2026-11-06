# Prepare a source-backed cane history

Run `prepare_cane_history.R` from a WHEP source checkout to create a local CSV
accepted by `build_primary_production(historical_data = ...)`. The script never
uploads a pin or changes the default production inputs.

After preparing and reviewing the artifact for its declared coverage, pass it
through the existing production argument with an explicit year range:

```r
history <- readr::read_csv("cane_history.csv", show_col_types = FALSE)
production <- whep::build_primary_production(
  start_year = 1940, end_year = 1965, historical_data = history
)
```

This real build needs WHEP's external inputs; it is not a package test. The
artifact carries the selected scenario only for its declared historical years.

```sh
Rscript inst/scripts/prepare_cane_history.R \
  observations.csv land.csv cane_history.csv source_anchored_land
```

The observation CSV contains one row per canonical reporting polity and year:
`year`, `reporting_polity_code`, `area_code`, `ha`, `tonnes`, `mass_basis`,
`source`, `method_cane`, `source_period`, and `source_footprint`. Declare
`mass_basis` as `fresh_cane`; raw sugar or sugar-equivalent quantities cannot
be inserted as cane. Keep original crop periods in `source_period`, and state
any period-to-calendar alignment in the source documentation. Distinguish
compiled government statistics from reconstructed source values in
`method_cane`. The land CSV contains the requested target and anchor years,
with `year`, `reporting_polity_code`, `area_code`, `land_area_ha`, and
`land_source`. Here `land_area_ha` means cropland over the declared
territorial footprint; the model assumes a constant source-crop share and yield
outside its anchors. Missing land rows or provenance are errors, not zero land.

Both inputs must use the same reviewed territorial authority and cohort as the
consumer. Numeric area codes are checked at ingestion; observations and land
are matched on canonical polity and year. Matching identifiers does not prove
that a reconstructed source footprint exactly equals a sovereign polygon.
Record partition approximations and any use of present-day land ratios for a
retrospective footprint. Do not relabel whole Pakistan as West Pakistan.

Three explicit methods are available:

- `source_anchored_land` interpolates area and production separately between
  supplied anchors. Outside those anchors it fixes their yield and scales
  their area by the supplied land ratio. This performed better than the tested
  one-sided trend alternatives in bounded India/Pakistan holdouts; it remains
  provisional and is not a validated Myanmar default.
- `source_anchored_constant` uses the same interior interpolation, but holds
  both area and production constant outside the observed interval. It is a
  sensitivity alternative.
- `source_only` rejects every uncovered target year. It is a completeness
  diagnostic and never silently switches to either estimator.

All supplied observations remain unchanged. Output rows retain `method_cane`,
anchor years and extrapolation distance. Source labels distinguish modelled
rows from the original observations; those labels survive the existing
historical-data consumer even though its output omits the extra preparation
metadata. The script verifies canonical identity, year, item, unit and value
through ingestion before writing the CSV and its `.manifest.json`. Existing
outputs or manifests are rejected. The manifest records input/output/code
hashes, methods, territorial coverage and maximum extrapolation distance.

Use the artifact for its declared domain. A build covering additional earlier
years does not validate those years; prepare their target land rows explicitly
if they are to use this estimator. For the bounded #1580 review, the artifact
covers 1940–1960 and the consumer comparison runs 1940–1965, retaining the
current FAOSTAT anchors from 1961 onward. Publication or default adoption of a
curated artifact is a separate reviewed operation.

## Evidence and limits

Kurosaki (2011), *Compilation of Agricultural Production Data in Areas
Currently in India, Pakistan, and Bangladesh from 1901/02 to 2001/02*, Hi-Stat
Discussion Paper 169, Tables 8–11 and pp. 7–10, supplies area/production pairs
and explicitly describes territorial reconstruction assumptions:
<https://gcoe.ier.hit-u.ac.jp/research/discussion/2008/gd10-169.html>.

The provisional methods were compared using rolling withheld blocks of
compiled-government India/West Pakistan records. Reconstructed target rows
were excluded from scoring, although earlier reconstructed anchors could
train. One-year interior interpolation had median production errors of
5.54%/10.11%; five-year gaps had medians of 10.28%/12.59% and maximum errors of
45.9%/47.5%. Source-anchored land backcasts over five-year blocks had medians of
15.0%/21.5% and aggregate biases of +9.2%/+20.1%. Overlapping folds are not
independent samples. These results do not validate Myanmar, long extrapolation
or full global history. Short unconstrained production trends performed worse
and sometimes produced negative values; no arbitrary cap was used to hide
that failure.

The Myanmar 1959/60 example is FAO-reported sugarcane republished in UN ECAFE's
*Economic Survey of Asia and the Far East 1963* (1964), Table 10, pp. 193/195:
<https://repository.unescap.org/items/18579e4a-ba45-4056-9a42-b345ee9bfea1>.
It shares FAO lineage and is not independent corroboration. Its unmarked
country row alone does not establish comprehensive measurement coverage.
Retain that qualification rather than inferring a sugar-to-cane factor.

The early-period column is not a clean validation target. ECAFE reproduces
15 kha and 1.076 Mt under 1948/49–1952/53, with a warning about the actual
averaging periods. More specifically, the earlier FAO Table 20 scan assigns
its 1,076 kt production entry to 1952/53 alone (footnote 10), while the 15 kha
area has no matching annual-period annotation. Neither a five-year observed
production mean nor a like-period yield follows. The extract also leaves the
asterisk meaning and edition metadata unverified.

As sensitivity calculations only, comparing the land-scaled scenario's
1948–1952/1949–1953 means with those entries gives production 16.8–17.4% low
and area 44.5–45.6% high; constant quantities give production 0.93% low and
area 73.3% high. Those comparisons cannot establish a Myanmar estimator
ranking because the source periods are not matched. Do not turn the summary
into annual observations or infer an unqualified default. A production-only
entry cannot become a paired source anchor by inventing its annual area.
