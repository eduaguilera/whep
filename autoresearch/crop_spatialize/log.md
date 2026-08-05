# Optimization Log — run_crop_spatialize

## Known baseline (from prepare_spatialize autoresearch)

| Section | year_range | Approx time | Notes |
|---------|------------|-------------|-------|
| s10_crop_spatialize | 1980–2010 (31 years) | ~54s | Already uses mclapply (2 workers), data.table per-year allocation |

## Experiments

| Run | total_s | Status | Description |
|-----|---------|--------|-------------|
<!-- Start with algorithmic/structural changes; worker-count increase requires RAM estimation first -->
| baseline | 193.44 | — | 11 years (2000-2010), 2 mclapply workers; profile: 30.8% [.data.table, 18.6% vctrs::vec_locate_matches, 13.4% bmerge. Per-year alloc ~4-7s; "individual crops done" 162s vs expected ~54s → ~108s post-mclapply overhead |
| 1 | 166.35 | kept | Move CFT aggregation inside .run_spatialize_year with data.table; parent receives ~591K CFT rows/year instead of 1.5M crop rows; eliminates dplyr join+summarise on 17.5M rows in parent; −27s (14.0%); RAM chunk: 2 GB → 619 MB |
| 2 | 122.66 | kept | Write yields/nitrogen per-year parquets directly from worker (nanoparquet); parent no longer bind_rows(17.5M yields) + bind_rows(85M nitrogen) + two chunk writes; workers also run concurrently so I/O overlaps; −44s (26.4% vs run 1, −36.6% vs baseline) |
| 3 | 122.06 | reverted | Filter zero-ha rows from crops_yr before yields/N (rainfed+irrigated>0); −1.9s (1.5%) — upstream joins already filter heavily, almost no zeros exist; marginal gain, reverted |
| 4 | 119.27 | reverted | data.table inner_join for yields+nitrogen initial join; −2.8% — remaining vctrs is in downstream left_joins on spatial_yield_idx/item_ratios, not initial join; marginal, reverted |
| 5 | 115.86 | kept | arrow::write_parquet instead of nanoparquet in forked workers; Arrow C++ backend ~40% faster (5.18s vs 8.86s/3yrs profile); −6.8s (−5.5% vs run 2); profile: nanoparquet→parquet___arrow___FileWriter__WriteTable 11.1%, Table__from_dots 3.8% |
| 6 | 118.33 | reverted | data.table rewrite of spatial index left_joins in .spatialize_yields_year + .spatialize_nitrogen_year; vctrs dropped 12.4%→2.1% but bmerge exploded 8.9%→24.9% — sorting 1.5M+ rows by composite (lon,lat,item) keys costs more than vctrs hash join; +2.5s regression vs run 5 |
| 7 | 232.67 | reverted | match()+paste() instead of dplyr::left_join for spatial index lookups; paste() exploded to 49.7% of serial time — 49s on 85M+ N rows after cartesian join; +94% regression, clear dead end |
| 8 | 115.55 | kept | n() instead of n_distinct(paste(lon,lat)) in summary_tbl; CFT rows already unique per (lon,lat,year,cft_name) so paste+distinct is wasted work on 6.5M rows; enables dplyr::collect() removal in Arrow combine (streaming direct to write_parquet); cumulative −40.3% vs baseline |

## Notes on dead ends and constraints

- **N join data.table rewrite** (from prepare_spatialize run 6): 1.5s gain,
  0.4% of total — marginal; do not retry alone.
- **`.spatialize_year` data.table vectorisation**: already done; all 175 crops
  processed in a single cartesian pass per year.
- **Spatial index joins**: cannot be optimised with data.table binary merge
  (sorting 1.5M rows by composite gridded keys costs more than vctrs hash join,
  Run 6) nor with match()+paste() (paste on 85M N rows is catastrophic,
  Run 7). dplyr/vctrs hash join is the best available approach for this
  data shape.
