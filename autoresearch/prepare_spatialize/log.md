# Optimization Log — prepare_spatialize_all.R

## Known baselines (pre-autoresearch)

| Section | Approx time | Notes |
|---------|-------------|-------|
| s5_mirca_irrigation | ~25s | Already optimized (was 260s) |
| s9a_hydrology | ~140s | GLWD bottleneck, irreducible without pre-caching |

## Experiments

| Run | total_s | Status | Section | Description |
|-----|---------|--------|---------|-------------|
| 1   | 353.64  | kept   | s4      | Parallelize 11-year LUH2 loop with furrr::future_map (move carea_rast load inside worker); s4 23.1s→10.9s |
| 2   | 350.81  | reverted | s6   | furrr::future_map2 for 151 EarthStat yield rasters — zero gain; bottleneck is post-read dplyr joins, not I/O (s3 pre-warms OS file cache) |
| 3   | 382.43  | baseline | —    | New baseline with 31-year range (1980–2010); s2 grew to 35.6s |
| 4   | 357.43  | kept     | s2   | Parallelize .luh2_country_totals year loop with furrr (extract .luh2_year_country_totals worker); s2 35.6s→19.4s |
| 5   | 370.73  | reverted | s3   | furrr::future_map2 for 151 EarthStat harvest-fraction rasters — zero gain; EarthStat tasks (~0.085s each) too fast, furrr startup overhead dominates regardless of cache state |
| 6   | 357.03  | reverted | s10  | data.table rewrite of nitrogen join pipeline — only 1.5s gain (0.4%) at the cost of 40 extra lines; marginal with added complexity |
| 7   | 336.09  | kept     | s4+s2 | Combined LUH2 pass: `.read_luh2_year_combined` does one terra::aggregate per type instead of two; country totals derived from already-aggregated 0.5° grid via dplyr join; s4 now returns `luh2_totals` for s2; s2 LUH2 time 19.4s→0.2s, s4 grew from ~10.9s to 22.9s (now covers both); net −21s (6.0%) |
| 8   | 328.17  | reverted | s9b  | parallel::mclapply for texture(modal)+pH(mean) aggregations — 10.6s gain on s9b (28.3→17.7s) but only 2.4% of total (below 5% threshold); terra SpatRasters cannot cross fork boundary so df returned in worker instead |

## Notes on remaining opportunities

- **EarthStat furrr** (s3, s3b, s6): dead end — furrr startup (~2-3s) exceeds benefit for ~0.085s tasks; marked as won't-do
- **s9a GLWD hydrology**: irreducible without pre-caching aggregated raster next to source data (one-time setup)
- **s9b HWSD soil**: fork parallelism (mclapply) saves 10.6s on section but only 2.4% of total — marginal. Not worth complexity vs. threshold.
- **s10 N join**: marginal even with data.table (22.8M rows but most time is in parquet write, not join)
- **Merge s2+s4 LUH2 passes**: DONE in run 7
- **s10 crop_spatialize**: 54s (16%); investigate `run_crop_spatialize` for parallelism opportunities
- **s9b HWSD soil**: 28.3s; terra-bound, cannot reduce without changing outputs
- **s8 livestock**: 11.5s; only 2 years processed for gridded_pasture due to year_range cap in LUH2 call
