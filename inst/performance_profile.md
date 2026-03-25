# Performance Profile

Benchmark: 2 years (2000–2001), single-threaded, on a RAM-constrained machine.

## Summary

| Step | Time | Peak memory |
|---|---|---|
| `build_primary_production` | ~34 s | ~268 MB |
| `build_commodity_balances` | ~35 s | ~449 MB |
| **Total** | **~68 s** | **~449 MB** |

Output sizes are tiny (prod 4.6 MB, cbs 3.6 MB). Almost all memory is
intermediate allocations.

Memory scales ~165 MB per additional year. Extrapolating to the full
1850–2021 range (172 years) would require ~28 GB peak — impractical
without further work.

---

## build_primary_production — Step timings

| Step | Time | Notes |
|---|---|---|
| Reading CBS production | 8.3 s | 4 parquet reads via Arrow + transform + polity aggregation |
| Computing yields | 6.8 s | Gap-filling with `fill_linear`, many grouped operations |
| Adding historical yields | 4.1 s | `fill_linear` + `fill_proxy_growth` over many groups |
| Building fodder dataset | 2.6 s | 2 parquet reads + dplyr joins |
| Flagging spikes | 2.3 s | `setorderv` + grouped `shift` over ~88K rows |
| Building livestock stocks | 1.5 s | 1 parquet read + transforms |
| Reading FAO crops/livestock | 1.6 s | 1 parquet read + column rename + polity aggregation |
| Flagging carry-forwards | 1.7 s | `setkeyv` + grouped reverse-cumsum |
| Combining primary raw | 1.4 s | `rbindlist` + dplyr joins |
| Reading land areas | 0.7 s | 1 parquet read |
| Reading international yields | 0.8 s | 1 parquet read |
| Extending historical series | 0.6 s | Linear extrapolation |
| All other steps | < 0.5 s | |

### Hottest functions (self time)

| Function | Self time | % | What it does |
|---|---|---|---|
| `forderv` | 5.0 s | 11% | data.table internal sorting for group-by and key operations |
| `.Call` | 4.9 s | 11% | Native C calls (Arrow reader, data.table internals) |
| `[.data.table` | 3.4 s | 8% | Subsetting and grouped aggregation |
| `Table__from_ExecPlanReader` | 3.1 s | 7% | Arrow parquet deserialization |
| `copy` | 2.4 s | 5% | Defensive `data.table::copy()` calls |
| `FUN` | 1.7 s | 4% | Per-group summary functions (`vapply`/`apply`) |
| `.first_non_missing` | 1.3 s | 3% | Custom aggregator used in yield gap-filling |
| `bmerge` | 1.1 s | 2% | data.table merge/join |

### Improvement opportunities

- **Reading CBS production (8.3 s)**: Reads 4 FAOSTAT parquet files,
  transforms each, and aggregates to polities. Already mitigated by
  passing CB extracts to the CBS build (saving the duplicate read
  there). Could improve further by re-partitioning parquet files by
  year so Arrow reads smaller row groups.

- **Computing yields (6.8 s)**: Dominated by `fill_linear` gap-filling
  called per item×area group. The vectorized `nafill` path is already
  used, but the number of groups (~thousands) makes the grouped
  operations expensive. Could benefit from reducing the number of
  group-by passes.

- **Adding historical yields (4.1 s)**: Same pattern — grouped
  `fill_linear` and `fill_proxy_growth`. Many small groups each
  requiring sort + fill.

---

## build_commodity_balances — Step timings

| Step | Time | Notes |
|---|---|---|
| Reading CBS inputs | 12.0 s | Reads trade, cbs-new, historical trade, GDP, residues, land (CB extracts reused from production) |
| Flagging spikes | 4.9 s | Grouped spike detection over ~206K rows |
| Filling destiny gaps | 4.3 s | Destiny share computation + interpolation + assembly |
| Flagging carry-forwards | 2.7 s | Grouped carry-forward detection |
| Combining CBS sources | 2.0 s | dplyr joins + source selection logic |
| Redistributing non-processed | 1.5 s | Grouped operations |
| Final balancing | 1.4 s | Clamp DS, fix exports, default destiny |
| Imputing trade & domestic supply | 1.0 s | GDP-based imputation |
| Second round processed products | 0.9 s | |
| Reclassifying processing | 0.9 s | |
| All other steps | < 0.5 s | |

### Hottest functions (self time)

| Function | Self time | % | What it does |
|---|---|---|---|
| `forderv` | 10.3 s | 19% | data.table sorting — dominates CBS |
| `.Call` | 7.2 s | 14% | Native C calls |
| `bmerge` | 3.7 s | 7% | data.table joins (many merge steps in CBS pipeline) |
| `[.data.table` | 3.5 s | 7% | Grouped aggregation |
| `vec_slice` | 1.8 s | 3% | vctrs slicing (dplyr internals) |
| `vctrs::vec_locate_matches` | 1.6 s | 3% | dplyr join matching |
| `copy` | 1.5 s | 3% | Defensive copies |
| `Table__from_ExecPlanReader` | 1.5 s | 3% | Arrow parquet reads |

### Improvement opportunities

- **`forderv` sorting (10.3 s, 19%)**: The single biggest CBS
  bottleneck. Called implicitly by every `setkeyv`, `setorderv`,
  and grouped `[.data.table` operation. The CBS pipeline has many
  group-by steps that each re-sort. Could reduce by ensuring data
  stays sorted across consecutive operations that use the same key,
  or by combining multiple grouped operations into fewer passes.

- **Reading CBS inputs (12.0 s)**: Even with CB extracts reused from
  production, still reads 6 additional datasets (trade, cbs-new,
  historical trade ×2, GDP, crop residues, land). The trade parquet
  (201 MB on disk, 35 row groups of 500K rows) is the largest.
  Re-partitioning by year would help.

- **QC flagging (7.6 s combined)**: Spike and carry-forward detection
  each iterate over all groups. The sort is done once (carry-forward
  sets the key, spikes skip if key matches), but the grouped
  operations themselves are expensive at ~206K rows across thousands
  of groups.

- **`bmerge` joins (3.7 s)**: Many merge steps in the CBS pipeline
  (combine sources, add trade, impute, etc.). Each merge sorts
  and probes. Could reduce by pre-sorting merge keys or combining
  sequential merges.

---

## Memory profile

| Years | Peak memory | Output size |
|---|---|---|
| 1 (2000) | 289 MB | 8 MB |
| 2 (2000–2001) | 449 MB | 8 MB |
| 3 (2000–2002) | 592 MB | 13 MB |
| 5 (2000–2004) | 921 MB | 24 MB |

### Where memory goes

Arrow year filtering via predicate pushdown is effective — `faostat-trade`
goes from 1.9 GB (unfiltered) to 38 MB (1 year). However, Arrow must
load entire row groups (500K rows each spanning all years), creating
temporary spikes:

| Dataset | Disk size | 1-year filtered | Unfiltered |
|---|---|---|---|
| faostat-trade | 201 MB | 37.6 MB | 1934 MB |
| faostat-fbs-old | 33.5 MB | 20.3 MB | 1011 MB |
| faostat-fbs-new | 45.3 MB | 0 MB (no data for 2000) | 503 MB |
| faostat-production | 35.0 MB | 8.1 MB | — |

During reading, Arrow temporarily allocates memory for full row groups
before filtering, then releases it. This creates memory spikes of
~100–200 MB per large dataset read even for 1-year queries.

### Improvement opportunities

- **Re-partition parquet files by year**: Writing parquet files with
  one row group per year (or small year ranges) would let Arrow skip
  irrelevant groups entirely, reducing both memory spikes and read time.

- **Stream inputs instead of reading all at once**:
  `.cbs_read_inputs` loads ~12 datasets into a list simultaneously.
  Processing datasets incrementally and freeing each after use would
  reduce peak memory.

---

## How to reproduce

```r
# Adjust years to fit available RAM (~165 MB per year)
source("inst/scripts/profile_performance.R")
```
