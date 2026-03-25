library(whep)

start_year <- 2000
end_year <- 2001

# ── Profiling with Rprof ──────────────────────────────────────────────────────

gc(reset = TRUE)

# Profile build_primary_production
prof_file_prod <- tempfile("prof_prod_", fileext = ".out")
Rprof(prof_file_prod, interval = 0.02)
t0 <- proc.time()
prod <- build_primary_production(start_year = start_year, end_year = end_year)
t_prod <- proc.time() - t0
Rprof(NULL)
mem_prod <- gc()

cat("\n=== build_primary_production elapsed ===\n")
print(t_prod)
cat("Peak memory:", mem_prod[2, 6], "MB\n")
cat("\n=== Top functions (by self time) ===\n")
prof_prod <- summaryRprof(prof_file_prod)
print(head(prof_prod$by.self, 20))

# Profile build_commodity_balances
gc(reset = TRUE)
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
mem_cbs <- gc()

cat("\n=== build_commodity_balances elapsed ===\n")
print(t_cbs)
cat("Peak memory:", mem_cbs[2, 6], "MB\n")
cat("\n=== Top functions (by self time) ===\n")
prof_cbs <- summaryRprof(prof_file_cbs)
print(head(prof_cbs$by.self, 20))

cat("\n=== TOTAL elapsed ===\n")
print(t_prod + t_cbs)
