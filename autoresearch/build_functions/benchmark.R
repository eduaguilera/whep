#
# Autoresearch benchmark harness. Profiles the full pipeline
# (production + CBS) as a single unit under Rprof.
#
# Usage:
#   Rscript autoresearch/build_functions/benchmark.R
#
# Output format (grep-friendly):
#   METRIC total_s=<seconds>
#   PROFILE_START
#   ... top 15 self-time entries ...
#   PROFILE_END
#   CALLERS_START
#   ... top callers of forderv, bmerge, copy ...
#   CALLERS_END

# Load the package in dev mode. If running inside renv with all deps
# installed, `library(whep)` also works.
devtools::load_all(".")

# ── Config ──────────────────────────────────────────────────────────────────
# Use a small year range for fast iteration (~25s per run).
start_year <- 1850L
end_year <- 2023L

# ── Run ─────────────────────────────────────────────────────────────────────

gc(reset = TRUE)
prof_file <- tempfile("prof_pipeline_", fileext = ".out")

Rprof(prof_file, interval = 0.02)
t0 <- proc.time()

prod <- build_primary_production(
  start_year = start_year,
  end_year = end_year
)
cbs <- build_commodity_balances(
  prod,
  start_year = start_year,
  end_year = end_year
)

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
# Parse the raw Rprof output to find what calls forderv, bmerge, copy.

cat("CALLERS_START\n")

lines <- readLines(prof_file)
# Skip header line
lines <- lines[-1]

# For each target function, find its callers (the function that called it)
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
