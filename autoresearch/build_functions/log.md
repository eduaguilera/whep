# Autoresearch Experiment Log

## Baseline

> Run `Rscript autoresearch/build_functions/benchmark.R` before starting
> experiments and record the baseline metric here.

| Run | total_s | Status | Description |
|-----|---------|--------|-------------|
| 0   | 71.20   | baseline | Initial state before any changes |
| 1   | 67.58   | reverted | Added .copy=FALSE to fill_linear internal callers — only 2/13 callers pass data.tables, rest pass tibbles (as.data.table already copies). Marginal gain masked by I/O variance (~10s swings). Complexity not justified. |
| 2   | 64.66   | kept | Eliminate double tibble→data.table conversion in fill_linear. Previously, dup-check created a temp data.table then main path created another. Now reuse the same one. ~8.6% gain, minimal diff. |
| 3   | 70.84   | reverted | Removed unnecessary copies in build_cbs.R (fao_trade, feed_dt, fbs_new). Marginal gain — these datasets are small. copy% unchanged at 2.38s. High I/O variance in this run. |
| 4   | 65.12   | kept | Vectorized .flag_cf_and_spikes in clean_faostat.R: replaced per-group by= with global rleidv+tabulate+cummax. Flagging step: 1.72s→0.25s (85% faster). Correctness confirmed by matching flag counts. |
| 5   | 53.35   | kept | Replace zoo::na.locf with data.table::nafill in fill_proxy_growth forward/backward fill. Avoids per-group R dispatch for LOCF/NOCB. Extension steps: 8.1s→5.1s (37% faster). |
| 6   | 51.80   | kept | Vectorize ave() calls in fill_proxy_growth: segmented cumprod via data.table by=, segmented cummin via cumsum-with-resets. Extension: 5.1s→4.4s. rev drops out of top 15 profile. |
| 7   | 53.78   | kept | Preserve sort order across consecutive fill_linear calls via .whep_sorted_by attribute. Skips redundant setkeyv when data is already sorted by the same columns. Small gain, minimal diff. |
| 8   | 54.26   | kept | Replace ifelse() with data.table::fifelse() in fill_linear and fill_proxy_growth hot paths. ifelse drops from top 15 profile. Marginal gain, mechanical change. |
| 9   | 37.80   | kept | Replace dplyr::slice_min in .dedup_production with data.table setorderv + .I[1L] per group. Eliminates vctrs ranking overhead (vec_rank, slice_rank_idx, nth drop from profile). ~30% faster than previous. |
| 10  | 36.04   | kept | Cache .processed_raw result in .cbs_reclassify_processing — was called twice with same args. Eliminates one redundant copy+merge+computation. Small gain, simplifies code. |
| 11  | —       | reverted | Replaced dplyr::summarise+first() with data.table in .format_cbs_output. Caused row-order mismatch in integration test. Row-order preservation tricky. |
| 12  | 38.68   | kept | Extract .extract_source_lookup helper using data.table [, by=] to replace 3 dplyr::distinct(.keep_all=TRUE) calls. Deduplicates code, marginal perf gain. |
| 13  | 36.03   | kept | Replace tidyr::pivot_wider with data.table::dcast in .calculate_raw_yields and .select_best_source. Reverted two other dcast replacements that broke correctness (dcast aggregates on duplicates). |
| 14  | 30.05   | kept | Replace dplyr::summarise+first() with data.table[, by=] in .format_cbs_output. Eliminates nth from profile (was 2.1%). Runs on full 400k+ CBS result — major speedup. |
| 15  | 33.96   | kept | Convert .cbs_final_balance: replace dplyr left_join+mutate+filter chains with data.table merge+`:=`+subsetting. Also extract .extract_source_lookup for 4th call site. |
| 16  | 297.47  | kept | Add .copy=FALSE to fill_linear for 3 data.table callers. Convert .cbs_redistribute_notprocessed from dplyr to data.table (full range: 21.7s→16.5s, vctrs overhead: 24.3s→16.2s). |
| 17  | 283.12  | kept | Convert .select_best_source mutate chain to data.table: fcoalesce+fcase+`:=` replace case_when+if_else. Full range: 304→283s (7% total). vctrs: 24.3→15.1s. |
| 18  | —       | reverted | Tried dcast with fun.aggregate=sum for .cbs_impute_trade pivot_wider — changed CBS output (24559 vs 28410 flags). pivot_wider takes first value on duplicates, sum changes semantics. |
| 19  | 94.41   | kept | Add sort=FALSE to all 50 merge() calls across build_cbs.R, build_production.R, read_raw_inputs.R. Eliminates redundant post-merge sorting. forderv: 67.7s→59.2s, setkeyv drops from top 15. ~4.5% total gain, mechanical change. |
| 20  | —       | reverted | Tried replacing setorderv with setkeyv in gapfilling/CBS hot paths so by= ops use existing key. No measurable gain — setkeyv still calls forderv, by= doesn't efficiently use prefix keys. |
| 21  | 92.87   | kept | Replace 3 merge() calls in .add_global_destiny_shares and .cbs_redistribute_notprocessed with data.table update-joins (dt[lookup, col := i.col, on=...]). Eliminates full-table copy overhead. Destiny assembly: 4.6→3.5s. Also added caller analysis to benchmark.R. |
| 22  | 76.89   | kept | Replace dcast+melt in .cbs_redistribute_notprocessed with long-format aggregation — computes domestic_supply as sum of components without pivoting. Also narrow unique() in .format_cbs_output to dedup by key+value (5 cols) instead of all columns (7 cols). dcast forderv: 4.5→3.9s, frankv: 4.1→3.6s. Year range changed to 1920-1965 (new baseline ~74s). |
| 23  | —       | reverted | Tried replacing unique() with [, by=] aggregation in .extract_source_lookup. Same forderv cost — both sort by the same columns. No net gain. |
| 24  | —       | reverted | Tried replacing pivot_wider+pivot_longer in .cbs_impute_trade with dcast+melt. vctrs: 3.84→1.98s (saved 1.86s), but dcast forderv increased by 1.6s (dedup unique + dcast). Net neutral (~74.39s). |
| 25  | ~72     | kept | Cache .extract_source_lookup: extract once in .fix_cbs, pass to 4 downstream functions via src_lookup param. Eliminates 3 of 4 redundant unique() calls. unique forderv: 7.7→5.2s (saved 2.5s). Simplifies pipeline architecture. |
| 26  | 73.98   | kept | Replace full-source dcast in .select_best_source with targeted dcast on 3 primary sources only + separate by= for other_mean. Eliminates frankv over dynamic source columns. dcast+frankv: 8.2→7.0s. Simpler code (no dynamic column handling). |
| 27  | —       | reverted | Tried replacing .calculate_raw_yields dcast with merge self-join. The dcast operates on a tiny dataset (~50K rows); production dcast at line 1960 (300K+ rows) dominates. No measurable gain. |
