# Simplest
devtools::check()

# For CRAN release
devtools::check(remote = TRUE, manual = TRUE, env_vars = c(NOT_CRAN = FALSE))
