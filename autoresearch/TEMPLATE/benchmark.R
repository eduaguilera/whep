#
# Autoresearch benchmark harness template.
#
# Profiles <FEATURE> under Rprof and reports:
#   - METRIC total_s=<seconds>                  (the optimization score)
#   - PROFILE_START/END                         (top 15 self-time entries)
#   - CALLERS_START/END                         (top callers of hotspots)
#
# Usage:
#   Rscript autoresearch/<FOLDER>/benchmark.R
#
# Customize:
#   1. The "Config" block — inputs/year range/size that keeps runs fast
#      enough to iterate (~20-60s) but representative of real workloads.
#   2. The "Run" block — call the function(s) under optimization.
#   3. The `targets` vector — which functions to drill into for caller
#      analysis (defaults assume a tidyverse/data.table pipeline).

# Load the package in dev mode. If running inside renv with all deps
# installed, `library(<PACKAGE>)` also works.
devtools::load_all(".")

# ── Config ──────────────────────────────────────────────────────────────────
# TODO: set inputs that exercise the hot path without taking too long.
# start_year <- 1900L
# end_year <- 1965L

# ── Run ─────────────────────────────────────────────────────────────────────

gc(reset = TRUE)
prof_file <- tempfile("prof_", fileext = ".out")

Rprof(prof_file, interval = 0.02)
t0 <- proc.time()

# TODO: call the function(s) you are optimizing.
# result <- my_function(...)

elapsed <- (proc.time() - t0)[["elapsed"]]
Rprof(NULL)

# ── Report ──────────────────────────────────────────────────────────────────

cat(sprintf("METRIC total_s=%.2f\n", elapsed))

cat("PROFILE_START\n")
sp <- summaryRprof(prof_file)$by.self
sp$pct <- round(100 * sp$self.time / sum(sp$self.time), 1)
top <- utils::head(sp, 15)
for (i in seq_len(nrow(top))) {
  cat(sprintf(
    "  %5.1f%% %6.2fs  %s\n",
    top$pct[i],
    top$self.time[i],
    rownames(top)[i]
  ))
}
cat("PROFILE_END\n")

# ── Caller analysis ────────────────────────────────────────────────────────
# Parse the raw Rprof output to find what calls the hotspot functions.
# Adjust `targets` to match whatever dominates the PROFILE output.

cat("CALLERS_START\n")

lines <- readLines(prof_file)
# Skip header line (contains the sample interval)
lines <- lines[-1]

targets <- c("forderv", "bmerge", "copy")

for (target in targets) {
  caller_counts <- list()
  for (line in lines) {
    fns <- strsplit(line, " ")[[1]]
    # Rprof stacks are bottom-to-top: first element = leaf, last = root
    idx <- which(fns == paste0('"', target, '"'))
    if (length(idx) == 0) {
      next
    }
    for (j in idx) {
      # Get the caller (next element = parent in call stack)
      if (j < length(fns)) {
        caller <- fns[j + 1]
        # Get grandparent for more context
        if (j + 1 < length(fns)) {
          gp <- fns[j + 2]
          key <- paste0(caller, " <- ", gp)
        } else {
          key <- caller
        }
      } else {
        key <- '"<top>"'
      }
      caller_counts[[key]] <- (caller_counts[[key]] %||% 0L) + 1L
    }
  }
  if (length(caller_counts) == 0) {
    next
  }

  counts <- unlist(caller_counts)
  counts <- sort(counts, decreasing = TRUE)
  total_samples <- sum(counts)
  interval <- 0.02
  cat(sprintf(
    "\n  === %s (%.1fs total) ===\n",
    target,
    total_samples * interval
  ))

  top_callers <- utils::head(counts, 10)
  for (i in seq_along(top_callers)) {
    pct <- round(100 * top_callers[i] / total_samples, 1)
    cat(sprintf(
      "    %5.1f%% %5.1fs  %s\n",
      pct,
      top_callers[i] * interval,
      names(top_callers)[i]
    ))
  }
}

cat("\nCALLERS_END\n")
