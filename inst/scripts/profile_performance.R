library(whep)

# Year ranges to benchmark.
# Adjust or comment out larger ranges to fit available RAM
# (~165 MB per year).
ranges <- list(
  small = c(2000, 2001), # 2 years  — quick smoke test
  medium = c(2000, 2004), # 5 years  — moderate workload
  large = c(1961, 2021) # 61 years — full FAOSTAT era
)

# Parse optional CLI argument: Rscript profile_performance.R small
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  sel <- args[1]
  if (!sel %in% names(ranges)) {
    stop(
      "Unknown range '",
      sel,
      "'. Choose: ",
      paste(names(ranges), collapse = ", ")
    )
  }
  ranges <- ranges[sel]
}

# ── Helpers ──────────────────────────────────────────────────────────────────

.profile_one <- function(label, start_year, end_year) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(sprintf(
    "  %s  (%d–%d, %d years)\n",
    label,
    start_year,
    end_year,
    end_year - start_year + 1L
  ))
  cat(strrep("=", 70), "\n")

  gc(reset = TRUE)

  # -- Production --
  prof_prod <- tempfile("prof_prod_", fileext = ".out")
  Rprof(prof_prod, interval = 0.02)
  t0 <- proc.time()
  prod <- build_primary_production(
    start_year = start_year,
    end_year = end_year
  )
  t_prod <- (proc.time() - t0)[["elapsed"]]
  Rprof(NULL)
  mem_prod <- gc()
  peak_prod <- mem_prod[2, 6]

  cat(sprintf(
    "\n  build_primary_production: %.1f s, %.0f MB peak\n",
    t_prod,
    peak_prod
  ))
  cat("  Top self-time:\n")
  sp <- summaryRprof(prof_prod)$by.self
  print(head(sp, 10))

  # -- CBS --
  gc(reset = TRUE)
  prof_cbs <- tempfile("prof_cbs_", fileext = ".out")
  Rprof(prof_cbs, interval = 0.02)
  t0 <- proc.time()
  cbs <- build_commodity_balances(
    prod,
    start_year = start_year,
    end_year = end_year
  )
  t_cbs <- (proc.time() - t0)[["elapsed"]]
  Rprof(NULL)
  mem_cbs <- gc()
  peak_cbs <- mem_cbs[2, 6]

  cat(sprintf(
    "\n  build_commodity_balances: %.1f s, %.0f MB peak\n",
    t_cbs,
    peak_cbs
  ))
  cat("  Top self-time:\n")
  sp <- summaryRprof(prof_cbs)$by.self
  print(head(sp, 10))

  cat(sprintf(
    "\n  TOTAL: %.1f s | peak memory: %.0f MB\n",
    t_prod + t_cbs,
    max(peak_prod, peak_cbs)
  ))

  data.frame(
    range = label,
    years = end_year - start_year + 1L,
    prod_s = round(t_prod, 1),
    cbs_s = round(t_cbs, 1),
    total_s = round(t_prod + t_cbs, 1),
    peak_mb = round(max(peak_prod, peak_cbs), 0),
    prod_rows = nrow(prod),
    cbs_rows = nrow(cbs)
  )
}

# ── Run ──────────────────────────────────────────────────────────────────────

results <- lapply(names(ranges), function(nm) {
  r <- ranges[[nm]]
  .profile_one(nm, r[1], r[2])
})
results <- do.call(rbind, results)

cat("\n")
cat(strrep("=", 70), "\n")
cat("  SUMMARY\n")
cat(strrep("=", 70), "\n")
print(results, row.names = FALSE)

# Memory scaling estimate
if (nrow(results) >= 2) {
  fit <- lm(peak_mb ~ years, data = results)
  cat(sprintf(
    "\n  Memory scaling: ~%.0f MB base + ~%.0f MB/year\n",
    coef(fit)[1],
    coef(fit)[2]
  ))
  est_full <- predict(fit, newdata = data.frame(years = 172))
  cat(sprintf(
    "  Extrapolated full range (1850-2021, 172 yr): ~%.1f GB\n",
    est_full / 1024
  ))
}
