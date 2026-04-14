#
# Autoresearch benchmark harness. Profiles the full pipeline
# (production + CBS) as a single unit under Rprof.
#
# Usage:
#   Rscript inst/autoresearch/benchmark.R
#
# Output format (grep-friendly):
#   METRIC total_s=<seconds>
#   PROFILE_START
#   ... top 15 self-time entries ...
#   PROFILE_END

# Load the package in dev mode. If running inside renv with all deps
# installed, `library(whep)` also works.
devtools::load_all(".")

# ── Config ──────────────────────────────────────────────────────────────────
# Use a small year range for fast iteration (~25s per run).
start_year <- 1900L
end_year <- 1965L

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
