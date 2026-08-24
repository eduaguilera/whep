# Resident-memory floor left by the production/CBS chain (internal archetype).
#
# Building primary production and the wide CBS leaves the process far larger
# than its live data, and `gc()` does not bring it back: #777 measured 20.6 GB
# resident for 1.42 GB live at 1901-2022. This script re-derives that
# decomposition on demand, because "the process is 20 GB" has three different
# causes with three different remedies:
#
#   live      - objects still reachable (the build cache, the returned tables).
#               `gc()`'s own accounting. The only part that is WHEP's bug.
#   arena     - blocks glibc has freed but not returned to the OS. Visible as
#               mallinfo2()$fordblks, and reclaimable with malloc_trim(0)
#               without touching a single live object.
#   allocator - pages held by an allocator that is not glibc. arrow's default
#               pool is mimalloc, which keeps its segments after
#               `bytes_allocated` falls back to zero; the pool's `max_memory`
#               is the size of that residue.
#
# Only the first is a leak. Reporting the other two as one number is what makes
# a floor look like a retention bug.
#
# Usage:
#   Rscript validation/memory_floor.R
# Config via env: VAL_MEM_YEAR_MIN, VAL_MEM_YEAR_MAX (a window, not the full
# span -- the full span needs ~28 GB and this is meant to be runnable on a
# busy machine). VAL_MEM_TRIM=0 skips the malloc_trim step.
#
# Reading the output: `floor_gb` is resident minus live after three full
# collections, and `reclaimable_gb` is how much of it one malloc_trim(0) call
# hands back. A large `reclaimable_gb` means the process is not holding data,
# it is holding address space.

suppressPackageStartupMessages({
  devtools::load_all(".")
})

year_min <- as.integer(Sys.getenv("VAL_MEM_YEAR_MIN", "2005"))
year_max <- as.integer(Sys.getenv("VAL_MEM_YEAR_MAX", "2015"))
do_trim <- Sys.getenv("VAL_MEM_TRIM", "1") != "0"

# glibc's malloc_trim()/mallinfo2() are not reachable from R, so the numbers
# that separate "freed but not returned" from "still live" need a three-call
# shim. No compiler, or not glibc (macOS, Windows): those columns come back NA
# and the live/resident pair still works.
.mem_shim <- function() {
  if (Sys.info()[["sysname"]] != "Linux" || !nzchar(Sys.which("gcc"))) {
    return(NULL)
  }
  dir <- tempfile("whep_mem_shim")
  dir.create(dir)
  src <- file.path(dir, "shim.c")
  writeLines(
    c(
      "#include <malloc.h>",
      "void whep_trim(int *res) { *res = malloc_trim(0); }",
      "void whep_info(double *out) {",
      "  struct mallinfo2 mi = mallinfo2();",
      "  out[0] = (double) mi.arena;",
      "  out[1] = (double) mi.hblkhd;",
      "  out[2] = (double) mi.uordblks;",
      "  out[3] = (double) mi.fordblks;",
      "}"
    ),
    src
  )
  # R CMD SHLIB writes its .o and .so into the working directory, so build
  # from inside the temporary one rather than dropping objects in the repo.
  previous <- setwd(dir)
  on.exit(setwd(previous), add = TRUE)
  built <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "SHLIB", "shim.c"),
    stdout = FALSE,
    stderr = FALSE
  ))
  so <- file.path(dir, paste0("shim", .Platform$dynlib.ext))
  if (built != 0L || !file.exists(so)) {
    return(NULL)
  }
  dyn.load(so)
  TRUE
}

.has_shim <- !is.null(.mem_shim())

.proc_gb <- function(field) {
  if (!file.exists("/proc/self/status")) {
    return(NA_real_)
  }
  status <- readLines("/proc/self/status")
  line <- grep(paste0("^", field, ":"), status, value = TRUE)
  if (length(line) == 0L) {
    return(NA_real_)
  }
  as.numeric(gsub("[^0-9]", "", line)) / 1024^2
}

.malloc_fields <- c("arena", "mmapped", "in_use", "freed_kept")

.malloc_gb <- function() {
  if (!.has_shim) {
    return(stats::setNames(rep(NA_real_, 4), .malloc_fields))
  }
  out <- .C("whep_info", out = double(4))$out
  stats::setNames(out / 1024^3, .malloc_fields)
}

.malloc_trim <- function() {
  if (!.has_shim) {
    return(NA_integer_)
  }
  .C("whep_trim", res = integer(1))$res
}

.arrow_pool_gb <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    return(c(allocated = NA_real_, max = NA_real_))
  }
  pool <- arrow::default_memory_pool()
  c(
    allocated = pool$bytes_allocated / 1024^3,
    max = pool$max_memory / 1024^3
  )
}

checkpoints <- list()

.checkpoint <- function(label) {
  collected <- gc(verbose = FALSE, full = FALSE)
  malloc <- .malloc_gb()
  arrow_pool <- .arrow_pool_gb()
  row <- tibble::tibble(
    label = label,
    resident_gb = .proc_gb("VmRSS"),
    peak_gb = .proc_gb("VmHWM"),
    live_gb = sum(collected[, 2]) / 1024,
    glibc_arena_gb = malloc[["arena"]],
    glibc_mmapped_gb = malloc[["mmapped"]],
    glibc_in_use_gb = malloc[["in_use"]],
    glibc_freed_kept_gb = malloc[["freed_kept"]],
    arrow_allocated_gb = arrow_pool[["allocated"]],
    arrow_max_gb = arrow_pool[["max"]]
  )
  checkpoints[[length(checkpoints) + 1L]] <<- row
  cat(sprintf(
    "%-28s resident %6.2f peak %6.2f live %6.2f glibc-freed-kept %6.2f\n",
    label,
    row$resident_gb,
    row$peak_gb,
    row$live_gb,
    row$glibc_freed_kept_gb
  ))
  invisible(row)
}

.collect_hard <- function(times = 3L) {
  for (i in seq_len(times)) {
    gc(full = TRUE)
  }
  invisible(NULL)
}

.checkpoint("00 session start")

years <- year_min:year_max
started <- proc.time()[["elapsed"]]
production <- whep::get_primary_production(years = years)
.checkpoint("01 primary production")
cbs <- whep::get_wide_cbs(years = years)
.checkpoint("02 wide cbs")
build_seconds <- proc.time()[["elapsed"]] - started

.collect_hard()
after_gc <- .checkpoint("03 after 3x gc(full)")

if (do_trim) {
  trim_started <- proc.time()[["elapsed"]]
  .malloc_trim()
  trim_seconds <- proc.time()[["elapsed"]] - trim_started
  after_trim <- .checkpoint("04 after malloc_trim(0)")
} else {
  trim_seconds <- NA_real_
  after_trim <- after_gc
}

# The cache is the one part of the residue that is genuinely live. An explicit
# `years =` request caches TWO primary-production builds -- the window asked
# for and the +/-5 context window the CBS build needs -- so both show up here.
cache_gb <- vapply(
  ls(whep:::.build_cache),
  function(key) {
    as.numeric(utils::object.size(whep:::.build_cache[[key]])) / 1024^3
  },
  numeric(1)
)

rm(production, cbs)
whep::whep_clear_cache()
.collect_hard()
.checkpoint("05 cache cleared")
if (do_trim) {
  .malloc_trim()
  .checkpoint("06 after malloc_trim(0)")
}

floor_gb <- after_gc$resident_gb - after_gc$live_gb
reclaimable_gb <- after_gc$resident_gb - after_trim$resident_gb

cat(sprintf(
  paste0(
    "\nMETRIC years=%d-%d build_seconds=%.0f peak_gb=%.2f live_gb=%.2f",
    " floor_gb=%.2f reclaimable_gb=%.2f trim_seconds=%.3f",
    " arrow_max_gb=%.2f cache_gb=%.2f cache_slots=%d shim=%s\n"
  ),
  year_min,
  year_max,
  build_seconds,
  after_gc$peak_gb,
  after_gc$live_gb,
  floor_gb,
  reclaimable_gb,
  trim_seconds,
  after_gc$arrow_max_gb,
  sum(cache_gb),
  length(cache_gb),
  .has_shim
))

cat("\nCHECKPOINTS_JSON_START\n")
cat(jsonlite::toJSON(
  dplyr::bind_rows(checkpoints),
  dataframe = "rows",
  auto_unbox = TRUE,
  digits = 3,
  pretty = TRUE
))
cat("\nCHECKPOINTS_JSON_END\n")

cat("\nCACHE_JSON_START\n")
cat(jsonlite::toJSON(
  as.list(round(cache_gb, 3)),
  auto_unbox = TRUE,
  pretty = TRUE
))
cat("\nCACHE_JSON_END\n")
