# CRAN submission comments — whep 0.4.0

## This is an update, not a new submission

whep 0.1.0, 0.2.0 and 0.3.0 are already on CRAN; 0.4.0 is the fourth
release. The package name, maintainer and license are unchanged.

## What changed since 0.3.0

`NEWS.md` carries the full, per-change list. In summary, 0.4.0 grows the
package rather than breaking it: the namespace goes from 30 to 192 exported
functions with **nothing removed and no signature retired**, so no 0.3.0 code
stops working. The new entry points cover the gridded soil carbon, soil
nitrogen and water balances, the FABIO-style footprint and its stressor
extensions, and the polity crosswalk that resolves historical territories.

Several changes move published numbers; each is described in `NEWS.md` with
its before/after magnitude. All examples run during `R CMD check` — the
package uses no `\dontrun{}` and no `\donttest{}`.

## Test environments

* local: Ubuntu 24.04.4 LTS, R 4.5.2 — `R CMD check --as-cran`
* GitHub Actions (`.github/workflows/R-CMD-check.yaml`), on every push:
  * ubuntu-latest, R-devel
  * ubuntu-latest, R-release
  * ubuntu-latest, R-oldrel-1
  * macos-latest, R-release
  * windows-latest, R-release
* win-builder, R-release and R-devel — TO RUN, see the checklist below
* R-hub — TO RUN, see the checklist below

## R CMD check results

TO FILL IN at submission, from the run in step 2 of the checklist below.
Nothing in this section has been measured on the release version yet, because
the version has not been bumped yet: the last dev-checkout run gave 0 errors,
0 warnings and 2 notes, both explained in step 2.

## Possible NOTEs, and why they are expected

* **Size of tarball (~5.9 MB).** The harmonisation crosswalks and the polity
  geometry table ship as package data because they are what makes the
  pipeline reproducible without network access. They are already stored
  `xz`-compressed (`LazyDataCompression: xz`).
* **`Imports` includes many non-default packages.** The package spans several
  independent pipelines (FAOSTAT ingestion, Parquet and NetCDF I/O, sparse
  linear algebra, ODE integration, spatial aggregation). Every import is used
  unconditionally by an exported function, so moving one to `Suggests` would
  leave that entry point unusable rather than degraded.

## Reverse dependencies

whep has no reverse dependencies on CRAN. Checked with

``` r
tools::package_dependencies(
  "whep",
  db = tools::CRAN_package_db(),
  reverse = TRUE,
  which = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
)
#> $whep
#> character(0)
```

so `revdepcheck::revdep_check()` has nothing to check and no downstream
package can be broken by this update.

<!--
MAINTAINER CHECKLIST — this file is .Rbuildignore'd and never ships. Run every
item immediately before submitting, replace the "TO RUN" / "TO FILL IN" markers
above with what you actually see, then delete this comment block.

1. `usethis::use_version("minor")`, so DESCRIPTION reads 0.4.0 and the NEWS
   heading reads `# whep 0.4.0`. Until then `--as-cran` NOTEs "Version
   contains large components (0.3.0.9000)".
2. `rcmdcheck::rcmdcheck(args = "--as-cran")` locally, and write the counts
   you get into "R CMD check results" above.
   As of this file's last update, a clean-export run of
   `--as-cran --no-tests --ignore-vignettes` on a dev checkout gave
   0 errors | 0 warnings | 2 notes:
     - "Version contains large components (0.3.0.9000)" — goes away at
       step 1; and an invalid DOI, see step 8.
     - "unable to verify current time" — local only; the CI workflow sets
       `_R_CHECK_SYSTEM_CLOCK_=0`.
3. `devtools::test()` — 100% green.
4. `devtools::check_win_devel()` and `devtools::check_win_release()`.
5. `rhub::rhub_check()`.
6. `revdepcheck::revdep_check()` — expected empty (see above), but
   re-confirm against the CRAN database of the day.
7. `spelling::spell_check_package()` — clean against `inst/WORDLIST`.
8. `urlchecker::url_check()`, and confirm every `\doi{}` resolves. The GLEAM
   supplement DOI in `R/livestock_coefs.R` is a 404 today and `--as-cran`
   NOTEs it; it needs the correct source before submission (whep#607).
9. Tarball size against the accepted 0.3.0 tarball (whep#183).
-->
