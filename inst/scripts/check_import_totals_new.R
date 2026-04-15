library(devtools)
library(dplyr)

load_all(quiet = TRUE)

env_new <- new.env(parent = globalenv())
sys.source("R/n_prov_destiny.R", envir = env_new)

res_new <- env_new$create_n_prov_destiny()
nat_new <- env_new$create_n_nat_destiny()

cat("res_new imports total:", sum(filter(res_new, Origin == "Outside")$MgN, na.rm = TRUE), "\n")
cat("res_new import rows:", nrow(filter(res_new, Origin == "Outside")), "\n")
cat("nat_new imports total:", sum(filter(nat_new, Origin == "Outside")$MgN, na.rm = TRUE), "\n")
cat("nat_new import rows:", nrow(filter(nat_new, Origin == "Outside")), "\n")
