# Temporary R_LIBS setup for devtools::check() with renv
Sys.setenv(R_LIBS = renv::paths$library())

# Run the check
devtools::check()

# Restore the original R_LIBS
Sys.unsetenv("R_LIBS")
