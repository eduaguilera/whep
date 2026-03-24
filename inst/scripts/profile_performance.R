library(whep)

start_year <- 1958
end_year <- 1965

# ── Profiling with Rprof ──────────────────────────────────────────────────────

# Profile build_primary_production
prof_file_prod <- tempfile("prof_prod_", fileext = ".out")
Rprof(prof_file_prod, interval = 0.02)
t0 <- proc.time()
prod <- build_primary_production(start_year = start_year, end_year = end_year)
t_prod <- proc.time() - t0
Rprof(NULL)

cat("\n=== build_primary_production elapsed ===\n")
print(t_prod)
cat("\n=== Top functions (by self time) ===\n")
prof_prod <- summaryRprof(prof_file_prod)
print(head(prof_prod$by.self, 30))
cat("\n=== Top functions (by total time) ===\n")
print(head(prof_prod$by.total, 30))

# Profile build_commodity_balances
prof_file_cbs <- tempfile("prof_cbs_", fileext = ".out")
Rprof(prof_file_cbs, interval = 0.02)
t0 <- proc.time()
cbs <- build_commodity_balances(
  prod,
  start_year = start_year,
  end_year = end_year
)
t_cbs <- proc.time() - t0
Rprof(NULL)

cat("\n=== build_commodity_balances elapsed ===\n")
print(t_cbs)
cat("\n=== Top functions (by self time) ===\n")
prof_cbs <- summaryRprof(prof_file_cbs)
print(head(prof_cbs$by.self, 30))
cat("\n=== Top functions (by total time) ===\n")
print(head(prof_cbs$by.total, 30))

cat("\n=== TOTAL elapsed ===\n")
print(t_prod + t_cbs)
