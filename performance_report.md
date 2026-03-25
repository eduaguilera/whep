# Performance Report: `get_bilateral_trade()` Optimizations

**Branch:** `lbm364dl/improve-bilateral-trade`
**Date:** 2026-03-25
**Dataset:** 10.3M raw bilateral trade rows, 187 countries, 3987 year-item groups
**Machine:** 32 cores (16 used), Linux

## Overall Result

| | Time | Speedup |
|---|---|---|
| **Original** | 22.7s | - |
| **Optimized** | 7.1s | **3.2x** |
| Data load (unchanged) | 4.7s | - |

End-to-end `get_bilateral_trade()` went from ~27s to ~12s.

## Step-by-Step Breakdown

| Step | Original | Optimized | Speedup | Key change |
|---|---|---|---|---|
| **clean** | 6.5s | 2.0s | **3.2x** | Base R vectorized ops, numeric key `%in%` |
| **nest** | 1.8s | 1.2s | **1.6x** | Skip factor conversion on BTD |
| **process** | 14.4s | 3.9s | **3.7x** | `parallel::mclapply`, BLAS `tcrossprod`, `match()` |

## Optimizations Applied

### 1. IPF column scaling: `t(t(m)*v)` to `tcrossprod(ones, v)`

**File:** `R/bilateral_trade.R:357`

The inner loop of the iterative proportional fitting algorithm scaled columns using `t(t(m) * v)`, which creates two transposed copies of the matrix. Replaced with `m * tcrossprod(ones, v)` which uses BLAS-optimized matrix multiplication and avoids transpositions. Benchmarked 6 alternatives; `tcrossprod` was 2.7x faster than `t(t(m)*v)` on 187x187 matrices.

### 2. Parallel processing: sequential `purrr::map2` to `parallel::mclapply`

**File:** `R/bilateral_trade.R:124`

The 3987 year-item groups are independent. Replaced `purrr::map2` (sequential) with `parallel::mclapply` using half the available cores. Scales well -- on 16 cores, gives ~3.7x speedup on the process step alone.

### 3. Flow direction preference: `dplyr::anti_join` to numeric key `%in%`

**File:** `R/bilateral_trade.R:199-206`

The original used `dplyr::anti_join` on 4 columns (10.3M rows) to prefer export data over import data when both exist. Replaced with a single numeric key: `from + to*1e3 + year*1e6 + item*1e10` (all values fit within double precision 2^53). A simple `%in%` on this key is much faster than the hash-join machinery of `anti_join`.

### 4. Cleaning: `dplyr::mutate/ifelse` to base R vectorized assignment

**File:** `R/bilateral_trade.R:178-194`

Replaced `dplyr::mutate(from_code = ifelse(...), to_code = ifelse(...), across(...))` with direct vector assignment. Replaced `add_item_cbs_code()` (which does a `dplyr::left_join`) with `match()` on the lookup table. On 10.3M rows, this avoids significant dplyr overhead.

### 5. Matrix indexing: factor `as.character` to integer `match()`

**File:** `R/bilateral_trade.R:322-323`

`.build_trade_matrix` originally converted factor `from_code`/`to_code` to character for dimname-based indexing. The factor conversion itself on 6.7M BTD rows cost ~0.4s. Replaced with `match(btd$from_code, code_int)` for integer-based positional indexing, which is faster per-group and eliminates the upfront factor conversion entirely.

### 6. Estimate computation: `outer()` to `tcrossprod()`

**File:** `R/bilateral_trade.R:334-335`

`outer(exports, imports)` creates the full outer product then multiplies by a scalar. `tcrossprod(exports, imports * scale)` folds the scalar into the second argument, using BLAS for the rank-1 matrix multiplication. Marginal per-call improvement, but called 3987 times.

### 7. Row sum functions: `rowSums()` to `.rowSums()`

**File:** `R/bilateral_trade.R:218, 234`

`.rowSums(m, nr, nc)` skips the S4 method dispatch and input validation of `rowSums()`. Same for `.colSums()`. Small but consistent improvement across thousands of calls.

## Alternatives Investigated and Rejected

### data.table

Thoroughly benchmarked for every step:

- **Nesting:** `tidyr::nest` is optimized for list-column patterns; data.table's `split`/`lapply` approach was slower.
- **Cleaning:** Same speed as base R vectorized ops. No gain from data.table's in-place modification since the input is a tibble.
- **`fread` on CSV:** The bilateral trade file is 121 MB as parquet vs 885 MB as CSV. `nanoparquet::read_parquet()` reads in ~2.5s; `fread` on CSV takes ~4-5s. Parquet wins.
- **Matrix operations:** data.table is irrelevant here -- the core computation is pure matrix algebra and IPF.

### Other rejected alternatives

- **`rep(v, each=nr)` for column scaling:** Allocating a large repeated vector was 30% slower than `tcrossprod`.
- **All 32 cores instead of 16:** Negligible improvement due to memory contention and the overhead-to-work ratio on 187x187 matrices.

## Correctness Verification

All 3987 output matrices match between original and optimized implementations with a maximum absolute difference of 1.5e-08 (floating point precision). 78 unit tests pass.
