# Read SPAM harvested area, production and yield by crop and technology.

Reads the SPAM (Spatial Production Allocation Model) global gridded crop
tables for the irrigated ("I") and all-rainfed ("R") technologies, per
~5-arcmin pixel and crop, and joins harvested area, production and yield
into one row per (pixel, crop, technology). This is the per-crop,
per-country "Level" input to the irrigated:rainfed regime yield ratio of
[`build_regime_yield_ratio()`](https://eduaguilera.github.io/whep/reference/build_regime_yield_ratio.md);
it does not itself compute the ratio.

`vintage = "2010"` (SPAM2010 v2.0, doi:10.7910/DVN/PRFF8V) is fetched
automatically: the three needed Global_CSV zips (harvested area,
production, yield) are downloaded from Harvard Dataverse on first use,
verified against their published MD5s, and cached under
`rappdirs::user_cache_dir("whep")`; `dir` or the `WHEP_SPAM_DIR`
environment variable overrides the cache with a directory of your own
(holding the zip files, or their already-extracted `_TI`/`_TR` members).

`vintage = "2020"` (SPAM2020 v2.0, doi:10.7910/DVN/SWPENT) is **never**
downloaded: Harvard Dataverse gates every file of that dataset behind a
mandatory guestbook requiring an email and institution, and the API's
documented way past it needs a logged-in Dataverse account submitting
that guestbook response. `dir`/`WHEP_SPAM_DIR` therefore must point at a
directory you have already populated by hand – fill the guestbook once
in a browser at the dataset's Dataverse page, download the three
Global_CSV zips, and pass their directory. The zips are verified against
the MD5s Dataverse publishes on the record (not re-checked against a
live download, since none is attempted); the column layout is read
through the exact same parser as SPAM2010, and aborts clearly, naming
what it found, if the expected identifying or crop columns are not there
– SPAM2020's layout has not been verified against SPAM2010's, since the
guestbook blocks even its own 6 KB ReadMe.

## Usage

``` r
read_spam_yields(vintage = c("2010", "2020"), dir = NULL, example = FALSE)
```

## Source

Yu, Q. et al. (2020). A cultivated planet in 2010 – Part 2: The global
gridded agricultural-production maps. Earth System Science Data 12,
3545-3572.
[doi:10.5194/essd-12-3545-2020](https://doi.org/10.5194/essd-12-3545-2020)
. Data: SPAM2010 v2.0,
[doi:10.7910/DVN/PRFF8V](https://doi.org/10.7910/DVN/PRFF8V) (CC-BY
4.0). SPAM2020 v2.0:
[doi:10.7910/DVN/SWPENT](https://doi.org/10.7910/DVN/SWPENT) (CC-BY 4.0;
gated behind a Harvard Dataverse guestbook, see Description).

## Arguments

- vintage:

  Which SPAM release to read: `"2010"` (default, fetched automatically)
  or `"2020"` (never fetched; needs `dir`/`WHEP_SPAM_DIR`, see
  Description).

- dir:

  Optional path to a directory holding the SPAM Global_CSV zip files (or
  their extracted `_TI`/`_TR` members), overriding `WHEP_SPAM_DIR`. A
  `vintage`-named subdirectory is read from it (e.g.
  `file.path(dir, "2010")`), matching what
  `inst/scripts/download/download_spam.R` writes.

- example:

  If `TRUE`, return a small fixture instead of reading SPAM data.
  Defaults to `FALSE`.

## Value

A tibble with `cell5m` (SPAM's pixel id), `lon`, `lat`, `iso3`,
`name_cntr`, `name_adm1`, `name_adm2`, `alloc_key` (SPAM's own
admin-coded pixel key), `spam_crop` (SPAM's short crop code, e.g.
`"whea"`), `technology` (`"I"` irrigated or `"R"` all-rainfed),
`harvested_area_ha`, `production_t`, `yield_kg_ha` (as SPAM publishes
it, not converted), `vintage` and `method_spam_source` (`"cache"`:
fetched and MD5-verified on demand; `"user_supplied"`: read from
`dir`/`WHEP_SPAM_DIR`, MD5-verified when a zip was found there,
unverified when only already-extracted CSVs were). A provenance record
(DOI, vintage, origin) is attached; read it back with
[`get_provenance()`](https://eduaguilera.github.io/whep/reference/get_provenance.md).

## Examples

``` r
read_spam_yields(example = TRUE)
#> # A tibble: 12 × 15
#>     cell5m     lon   lat iso3  name_cntr name_adm1 name_adm2 alloc_key spam_crop
#>      <int>   <dbl> <dbl> <chr> <chr>     <chr>     <chr>         <int> <chr>    
#>  1 1652909  42.5    58.1 RUS   Russian … Kostroms… Administ…   3832670 ocer     
#>  2 1652909  42.5    58.1 RUS   Russian … Kostroms… Administ…   3832670 ocer     
#>  3 2737530  67.5    37.2 AFG   Afghanis… Balkh     Kaldar      6342971 vege     
#>  4 2737530  67.5    37.2 AFG   Afghanis… Balkh     Kaldar      6342971 vege     
#>  5 2968820 -98.3    32.7 USA   United S… Texas     Palo Pin…   6880981 ocer     
#>  6 2968820 -98.3    32.7 USA   United S… Texas     Palo Pin…   6880981 ocer     
#>  7 3342453  77.8    25.5 IND   India     Madhya P… Shivpuri    7743094 opul     
#>  8 3342453  77.8    25.5 IND   India     Madhya P… Shivpuri    7743094 opul     
#>  9 3851276  -0.292  15.7 MLI   Mali      Gao       Gao         8922157 rice     
#> 10 3851276  -0.292  15.7 MLI   Mali      Gao       Gao         8922157 rice     
#> 11 3967747 -14.4    13.5 GMB   Gambia    Upper Ri… Sandu       9191988 sorg     
#> 12 3967747 -14.4    13.5 GMB   Gambia    Upper Ri… Sandu       9191988 sorg     
#> # ℹ 6 more variables: technology <chr>, harvested_area_ha <dbl>,
#> #   production_t <dbl>, yield_kg_ha <dbl>, vintage <chr>,
#> #   method_spam_source <chr>
```
