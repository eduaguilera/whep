# Performance Profile

Benchmarked on a 128 GB machine, single-threaded.

## Summary

| Range | Years | Production | CBS | Total | Peak memory |
|---|---|---|---|---|---|
| Small (2000–2001) | 2 | 13.2 s | 12.7 s | **25.8 s** | 469 MB |
| Medium (2000–2004) | 5 | 15.8 s | 17.2 s | **32.9 s** | 1.0 GB |
| Large (1961–2021) | 61 | 79.1 s | 102.4 s | **181.5 s** | 11.4 GB |

Memory scales ~185 MB per additional year (104 MB base).
Extrapolating to the full 1850–2021 range (172 years): ~31 GB peak.

Output sizes are tiny (prod 4.6 MB, CBS 3.6 MB for 2 years).
Almost all memory is intermediate allocations.

---

## build_primary_production — Step timings (2-year run)

| Step | Time | Notes |
|---|---|---|
| Reading CBS production | 3.9 s | 4 parquet reads via Arrow + transform + polity aggregation |
| Computing yields | 1.2 s | Gap-filling with `fill_linear`, vectorised character aggregation |
| Building fodder dataset | 1.5 s | 2 parquet reads + dplyr joins |
| Adding historical yields | 1.2 s | `fill_proxy_growth` over many groups |
| Reading FAO crops/livestock | 0.9 s | 1 parquet read + column rename + polity aggregation |
| Building livestock stocks | 1.0 s | 1 parquet read + transforms |
| Flagging carry-forwards + spikes | 0.8 s | Fused single-pass QC detection |
| Reading land areas | 0.8 s | 1 parquet read |
| Reading international yields | 0.7 s | 1 parquet read |
| Combining primary raw | 0.4 s | `rbindlist` + dplyr joins |
| Extending historical series | 0.3 s | Linear extrapolation |
| All other steps | < 0.2 s | |

### Step timings scale (61-year run)

| Step | Time | Notes |
|---|---|---|
| Reading CBS production | 20.6 s | 4 parquet reads — data volume scales with years |
| Computing yields | 15.6 s | Gap-filling dominates at scale |
| Combining primary raw | 11.2 s | Large merge with 2.4M rows |
| Adding historical yields | 6.3 s | `fill_proxy_growth` + joins |
| Building livestock stocks | 5.6 s | |
| Reading FAO crops/livestock | 4.6 s | |
| Building fodder | 3.9 s | |
| Assembling production | 2.1 s | |
| Reading land + yields | 4.2 s | |
| QC flagging | 1.6 s | Fused carry-forward + spike pass |
| All other steps | < 2 s | |

### Hottest functions (self time, 61-year run)

| Function | Self time | % | What it does |
|---|---|---|---|
| `forderv` | 34.0 s | 28% | data.table internal sorting for group-by and key ops |
| `copy` | 13.1 s | 11% | Defensive `data.table::copy()` in `fill_linear` |
| `Table__from_ExecPlanReader` | 12.6 s | 10% | Arrow parquet deserialization |
| `[.data.table` | 10.4 s | 9% | Subsetting and grouped aggregation |
| `.Call` | 10.3 s | 9% | Native C calls (Arrow, data.table internals) |
| `bmerge` | 4.0 s | 3% | data.table merge/join |
| `setkeyv` | 2.5 s | 2% | Explicit key setting (sort + key assignment) |

### Improvement opportunities

- **`forderv` sorting (34 s, 28%)**: Dominates production. Called
  implicitly by every `setkeyv`, `setorderv`, and grouped
  `[.data.table` operation. Already mitigated by using `setkeyv`
  (which preserves keys for subsequent operations) and fusing QC
  passes. Further improvement requires reducing the number of
  group-by passes or pre-sorting data before entering pipe chains.

- **`copy` (13 s, 11%)**: `fill_linear` defensively copies its
  input to avoid mutation. Each of the ~15 internal calls copies
  the full data.table. Could eliminate by adding an internal
  `.in_place` flag for callers that own the data.

- **Arrow reads (12.6 s, 10%)**: 4 FAOSTAT parquet files (fbs-new,
  fbs-old, cbs-old-animal, cbs-old-crops). Already mitigated by
  passing CB extracts to the CBS build. Could improve further by
  re-partitioning parquet files by year.

---

## build_commodity_balances — Step timings (2-year run)

| Step | Time | Notes |
|---|---|---|
| Reading CBS inputs | 6.8 s | Trade, cbs-new, historical trade, GDP, residues, land (CB extracts reused) |
| Flagging carry-forwards + spikes | 1.8 s | Fused single-pass QC detection |
| Filling destiny gaps | 1.1 s | Destiny share computation + interpolation + assembly |
| Combining CBS sources | 0.5 s | dplyr joins + source selection |
| Reclassifying processing | 0.4 s | |
| Final balancing | 0.3 s | Clamp DS, fix exports, default destiny |
| Redistributing non-processed | 0.3 s | Grouped operations |
| Imputing trade & domestic supply | 0.3 s | GDP-based imputation |
| All other steps | < 0.3 s | |

### Step timings scale (61-year run)

| Step | Time | Notes |
|---|---|---|
| Filling destiny gaps | 21.7 s | Interpolation + assembly dominate at scale |
| Reading CBS inputs | 18.2 s | Trade parquet (201 MB) is the largest |
| Redistributing non-processed | 9.2 s | Many grouped operations |
| Reclassifying processing | 7.3 s | |
| Final balancing | 7.1 s | |
| Combining CBS sources | 5.2 s | |
| Imputing trade & domestic supply | 4.9 s | |
| Extending CBS historical series | 4.3 s | |
| QC flagging | 3.6 s | Fused carry-forward + spike pass |
| All other steps | < 3 s | |

### Hottest functions (self time, 61-year run)

| Function | Self time | % | What it does |
|---|---|---|---|
| `forderv` | 51.4 s | 30% | data.table sorting — dominates CBS |
| `bmerge` | 15.5 s | 9% | data.table joins (many merge steps) |
| `.Call` | 12.1 s | 7% | Native C calls |
| `vctrs::vec_locate_matches` | 11.3 s | 7% | dplyr join matching |
| `[.data.table` | 10.0 s | 6% | Grouped aggregation |
| `setkeyv` | 9.6 s | 6% | Explicit key setting |
| `copy` | 8.4 s | 5% | Defensive copies |
| `Table__from_ExecPlanReader` | 7.1 s | 4% | Arrow parquet reads |
| `vec_slice` | 5.5 s | 3% | vctrs slicing (dplyr internals) |

### Improvement opportunities

- **`forderv` sorting (51.4 s, 30%)**: The single biggest CBS
  bottleneck. The CBS pipeline has many group-by steps that each
  re-sort. Already mitigated by fusing QC into one pass and using
  `setkeyv` instead of `setorderv` to preserve keys. Further
  improvement requires ensuring data stays sorted across consecutive
  operations or combining multiple grouped operations into fewer
  passes.

- **`bmerge` + `vctrs::vec_locate_matches` (26.8 s, 16%)**: Many
  merge/join steps in the CBS pipeline (combine sources, add trade,
  impute, destiny shares, etc.). Could reduce by pre-sorting merge
  keys or replacing dplyr joins with data.table joins where vctrs
  overhead is significant.

- **Reading CBS inputs (18.2 s)**: Even with CB extracts reused
  from production, still reads 6 additional datasets. The trade
  parquet (201 MB on disk, 35 row groups of 500K rows) is the
  largest. Re-partitioning by year would help.

---

## Memory profile

| Years | Range | Peak memory |
|---|---|---|
| 2 | 2000–2001 | 469 MB |
| 5 | 2000–2004 | 1.0 GB |
| 61 | 1961–2021 | 11.4 GB |

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

### Improvement opportunities

- **Re-partition parquet files by year**: Writing parquet files with
  one row group per year (or small year ranges) would let Arrow skip
  irrelevant groups entirely, reducing both memory spikes and read time.

- **Stream inputs instead of reading all at once**:
  `.cbs_read_inputs` loads ~12 datasets into a list simultaneously.
  Processing datasets incrementally and freeing each after use would
  reduce peak memory.

---

## Recent optimisations

1. **Fuse QC flags into single pass**: Combined `.flag_carry_forward`
   and `.flag_spikes` into `.flag_cf_and_spikes`, halving grouped
   dispatch overhead (one sort + one `by=` body instead of two).

2. **Vectorise character aggregation**: Replaced per-group
   `.first_non_missing()` calls with C-level `unique()` operations.
   At 61 years, "Computing yields" dropped from 32.4 s to 15.6 s
   (−52%).

3. **Skip redundant sorts in `fill_linear`**: Use `setkeyv` instead
   of `setorderv` and check key before sorting, so consecutive
   `fill_linear` calls with the same grouping skip re-sorting.

4. **Reuse CB extracts from production** (prior commit): CBS build
   skips 4 redundant parquet reads by reusing extracts attached
   to the production output.

---

## How to reproduce

```r
# Run all ranges (small, medium, large):
source("inst/scripts/profile_performance.R")

# Run a specific range:
# Rscript inst/scripts/profile_performance.R small
# Rscript inst/scripts/profile_performance.R medium
# Rscript inst/scripts/profile_performance.R large
```
