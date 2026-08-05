# Contributing to whep

Thanks for considering a contribution. `whep` processes
agro-environmental data for the [Who Has Eaten the
Planet](https://github.com/eduaguilera/whep) project, funded by the
European Research Council.

This is a research codebase, so parts of it are unavoidably technical:
nitrogen and carbon balances, gridded land-use modelling, multi-regional
input–output accounting. **You do not need to understand any of that to
make a useful contribution.** A good share of the open work is ordinary
R engineering — dead code, argument validation, missing tests,
documentation — and we label it as such so you can find it.

- [Finding something to work on](#finding-something-to-work-on)
- [Do I need the data?](#do-i-need-the-data)
- [Setting up](#setting-up)
- [The six CI checks, and how to run them
  locally](#the-six-ci-checks-and-how-to-run-them-locally)
- [Code style](#code-style)
- [Documentation](#documentation)
- [Tests](#tests)
- [Changing packaged data](#changing-packaged-data)
- [Citations and references](#citations-and-references)
- [Opening a pull request](#opening-a-pull-request)
- [Reporting a bug](#reporting-a-bug)
- [Conduct](#conduct)

## Finding something to work on

Start with [**good first
issue**](https://github.com/eduaguilera/whep/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).
Those issues carry a comment aimed specifically at a first-time
contributor: whether the task needs data or network access, the exact
`file:line` to look at, what “done” looks like, and which test file to
touch.

[**help
wanted**](https://github.com/eduaguilera/whep/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22)
is the wider pool of issues nobody is actively working on.

[**no-data-needed**](https://github.com/eduaguilera/whep/issues?q=is%3Aissue+is%3Aopen+label%3Ano-data-needed)
is the one to reach for if you cannot get the project’s input data — see
[the next section](#do-i-need-the-data). It means the issue can be
reproduced, fixed *and verified* with nothing but a clone of the
repository.

Two more labels tell you what kind of review to expect, and they are
worth understanding before you pick something:

| Label | What it means |
|----|----|
| `mechanical` | No methodological decision. The change is objectively correct, and green CI plus a test is close to sufficient to merge. |
| `needs-expert` | Embeds a scientific or methodological decision. Will not be merged on green CI alone; a domain expert has to weigh in. |

If you are new here, prefer `mechanical`. A `needs-expert` issue may be
small in diff terms and still need weeks of discussion about what the
right number is.

Comment on an issue before starting substantial work, so two people
don’t write the same patch. For a one-line fix, just open the PR.

## Do I need the data?

Usually not — and this is the question that trips people up, so it is
worth being explicit.

**The test suite is entirely offline.** No credentials, no downloads, no
pinned datasets. A dedicated `offline-tests` CI job enforces this by
running the suite with an empty cache behind a dead proxy, so any test
that reaches the network fails deterministically. You can therefore
clone, install dependencies, and run
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
with no access to anything.

The **full data pipeline** is a different matter. Running a real build
needs inputs that are not in the repository:

- **Pinned inputs** are fetched over the network from the project’s pins
  board, registered in `inst/extdata/whep_inputs.csv`.
- **Multi-GB rasters** (CRU climate, LPJmL output, HYDE, LUH2, HWSD, and
  others) live on local disk and are located through environment
  variables — `WHEP_CRU_DIR`, `WHEP_LUH2_DIR`, `WHEP_HWSD_DIR`, and
  friends. The readers abort with an instruction when a variable is
  unset; never hardcode a path.

So: if an issue is about a pipeline *result* being wrong, you probably
need the data and should say so on the issue. If it is about dead code,
a crash, argument validation, documentation, or a missing test, you
almost certainly do not.

You do not have to work this out yourself. Issues carrying
[**`no-data-needed`**](https://github.com/eduaguilera/whep/issues?q=is%3Aissue+is%3Aopen+label%3Ano-data-needed)
have been checked: the fix and its verification need only the repository
— package data under `data/` and `inst/extdata/`, hand-built
[`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html)
fixtures, and arguments you inject yourself all count as available. If
you find an issue with that label that turns out to need data after all,
please say so on the issue; the label is only worth having if it is
reliable.

## Setting up

You need **R \>= 4.1.0** (the codebase uses the native `|>` pipe
throughout).

``` r

install.packages("devtools")
devtools::install_deps(dependencies = TRUE)
devtools::load_all()
```

You also need the [`air`](https://github.com/posit-dev/air) formatter
binary on your `PATH` — see the formatting check below. It is not an R
package; download a release for your platform.

## The six CI checks, and how to run them locally

Six workflows run on every pull request, including one from a fork. Five
of them gate the merge; `test-coverage` is informational. Each is
reproducible on your machine, and doing that first is much faster than
pushing and waiting.

### 1. `format-suggest` — code formatting

``` bash
air format .
```

**This is mandatory and it is not optional or approximate.** CI runs the
`air` binary and posts a failing suggestion for any diff, so
hand-matching the style does not work. Run the binary. Note that it
formats *every* `.R` file in the repo — `R/`, `tests/`, `data-raw/` —
not only the ones you edited, so commit only your own hunks if it
reformats something unrelated.

### 2. `lint` — `lintr`

``` r

lintr::lint_package(
  linters = lintr::linters_with_defaults(
    object_usage_linter = NULL,
    line_length_linter = NULL,
    indentation_linter = NULL,
    commas_linter = NULL
  )
)
```

Those four linters are disabled because they conflict with `air`’s
output. `inst/scripts` and `inst/analysis` are excluded via `.lintr`.

Note that `line_length_linter` being off does **not** license long lines
— the 80-character maximum is still a house rule (see below), it just is
not machine enforced.

### 3. `R-CMD-check` — five platforms

macOS release, Windows release, and Ubuntu on R-devel, release and
oldrel-1. **No errors, no warnings, no notes.** This job also runs the
test suite, so a failing test turns up here as a hard ERROR.

To check without waiting for the tests:

``` r

rcmdcheck::rcmdcheck(
  build_args = "--no-build-vignettes",
  args = c("--no-tests", "--ignore-vignettes"),
  error_on = "error"
)
```

Two things account for most avoidable failures here:

- Every variable used in non-standard evaluation must be declared in
  [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  in `R/utils.R`.
- Every `stats::` and `utils::` function needs its explicit namespace
  prefix ([`stats::median()`](https://rdrr.io/r/stats/median.html), not
  [`median()`](https://rdrr.io/r/stats/median.html)).

### 4. The test suite

Run it directly while you work, rather than through the full check:

``` r

devtools::test()                          # everything
devtools::test(filter = "footprint")      # one file
```

The whole suite must be green — 100%, no skips added to get there.

### 5. `offline-tests` — no test may touch the network

Reproduce the CI condition exactly:

``` bash
XDG_CACHE_HOME=$(mktemp -d) http_proxy=http://127.0.0.1:9 \
  https_proxy=http://127.0.0.1:9 Rscript -e 'devtools::test()'
```

If this job fails on its own while the ordinary test job passes, you
have added a test that quietly fetches something. The fix is to give it
an offline fixture, **not** to skip the test and not to relax the job.

### 6. `pkgdown` — the documentation site

Every documented topic — functions *and* datasets with roxygen docs —
must appear in `_pkgdown.yml` under `reference:`. Adding an exported
function without listing it there fails the build. Check with:

``` bash
comm -23 \
  <(ls man/*.Rd | sed 's|man/||;s|\.Rd||' | grep -v whep-package | sort) \
  <(grep '^  - ' _pkgdown.yml | sed 's/^  - //' | sort)
```

Empty output means you are fine.

### And one that does not gate

`test-coverage` runs the suite under `covr` and reports to Codecov.
Treat a coverage drop as a prompt to add a test, not as a blocker.

One note on `format-suggest`: it is deliberately configured to work on
pull requests from forks, which is the normal path for an outside
contribution. If it posts suggestions on your PR, run `air format .` and
push again.

### The short version, before you push

``` bash
air format .
```

``` r

devtools::document()
rcmdcheck::rcmdcheck(
  build_args = "--no-build-vignettes",
  args = c("--no-tests", "--ignore-vignettes"),
  error_on = "error"
)
devtools::test()
```

## Code style

The baseline is the [tidyverse style
guide](https://style.tidyverse.org/), with `air` as the arbiter of
formatting. On top of that, this repo has conventions worth knowing
before you write anything — they come up in review constantly:

**Layout**

- Maximum line width is **80 characters**.
- Exported functions come **first** in a file; private helpers, prefixed
  with `.`, come at the end.
- Functions should be short — 25 lines is the target. Split large ones
  into named helpers rather than adding comments to a long body.
- No functions defined inside functions. Everything at top level.
- No more than 5 arguments in a signature; group related ones into a
  named list.
- Function and variable names stay under 30 characters.

**R idiom**

- Always use the native pipe `|>`. Write functions so they read as piped
  expressions.
- Always namespace imported functions:
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  never bare [`filter()`](https://rdrr.io/r/stats/filter.html). Do not
  use `@importFrom`.
- Avoid `for` loops — reach for vectorised operations, `purrr`, or
  `dplyr`/`tidyr`. (Exception: a `data.table` helper iterating a small
  fixed set of column names.)
- Use `stringr` rather than base R for string work.
- Use the `.by` argument for grouping.
- Escaped regex characters need double-escaping in R strings: `"\\."`,
  not `"\."`.
- When a function takes column names as arguments, expect them unquoted
  and use `{{ }}` inside.

**Data structures**

- This is a tidy-data project. Exported functions accept and return
  **`tibble`s**. Private helpers may use `data.table` internally for
  speed but must convert back before returning. Never a bare
  `data.frame`.
- Use
  [`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html)
  for small inline tables.
- Column names are `snake_case` and must be self-explanatory. Write
  `ne_maintenance`, `volatile_solids`, `methane_potential` — not `NEm`,
  `VS`, `Bo`.
- Do not carry redundant name/code column pairs (`area` alongside
  `area_code`) through intermediate steps. Integer codes are enough as
  join keys internally; human-readable names get joined on at the final
  output stage.

**Errors and validation**

- Validate arguments with `rlang` predicates —
  [`rlang::has_name()`](https://rlang.r-lib.org/reference/has_name.html),
  not `%in% names()`.
- Signal with `cli`:
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html),
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html).
  Never bare [`stop()`](https://rdrr.io/r/base/stop.html) or
  [`warning()`](https://rdrr.io/r/base/warning.html).

**Multi-method functions**

An estimation function with more than one defensible method exposes a
`method =` or `tier =` argument, defaulting to the most rigorous
available. Simpler methods stay selectable for sensitivity analysis.
They are alternatives, never silent fallbacks: the chosen method is
recorded in an output column, and a coarser one is used only when
explicitly asked for.

## Documentation

Use roxygen2, and document exported functions only — private `.` helpers
need no roxygen block.

- First line is the title, with no `@title` tag: short, verb in the
  imperative. Then `@description`, `@param` for each argument,
  `@return`, `@export`, `@examples`.
- One space after `#'`; indent continuation lines by two.
- End every documentation sentence with a full stop.

**Examples must run.** Never use `\dontrun{}` or `\donttest{}` —
everything runs during `R CMD check`. For a function that depends on
remote data or is slow, use the `example = FALSE` pattern: add an
`example` argument that returns a small hardcoded
[`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html)
fixture from `R/toy_examples.R`, and let `@examples` call
`my_function(example = TRUE)`. See
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
for the reference implementation. For anything that runs fast on small
input, just write a self-contained inline example.

Run
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
after any roxygen change, and commit the regenerated `man/` files.

## Tests

Tests use [testthat](https://testthat.r-lib.org/) (edition 3), one test
file per R script: `R/thing.R` is tested by
`tests/testthat/test_thing.R`.

- Reach exported objects through
  [`whep::name`](https://rdrr.io/r/base/name.html), not bare names. For
  dynamic access in a loop, `getExportedValue("whep", nm)`. Never `:::`
  or
  [`getFromNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html)
  for something exported.
- Build fixtures with
  [`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html);
  assert with `testthat` expectations or `pointblank`.
- Factor repeated fixture setup into helpers.
- `tests/testthat/test_gapfilling.R` is a good model to imitate.
- Every test must pass offline. See check 5 above.

## Changing packaged data

The `.rda` files under `data/` are build products. When you edit a CSV
in `inst/extdata/harmonization/`, regenerate them:

1.  Edit the CSV.
2.  `Rscript data-raw/harmonization_tables.R` — rebuilds the `.rda`
    files.
3.  `Rscript data-raw/table_mappings.R` — only if `regions.csv` or
    `items_*.csv` changed.
4.  `Rscript data-raw/whep_inputs.R` — only if `whep_inputs.csv`
    changed.

Commit both the CSV and the regenerated `.rda`.

## Citations and references

This one matters more here than in most repositories, and there is no
flexibility in it:

> **Never guess or invent a reference title, author, year, or DOI.**
> Verify every bibliographic detail against the actual source — the
> paper, the PDF, a DOI lookup — before you write it down.

If you cannot verify a citation, say so explicitly in the PR rather than
supplying a plausible-looking one. A wrong DOI in a science package is
worse than an absent one, because it looks checked.

## Opening a pull request

The project follows [this
workflow](https://lbm364dl.github.io/follow-the-workflow/); it is worth
a read.

- Branch from `main`. Name branches `yourname/short-topic` or
  `type/short-topic` — e.g. `docs/contributing-guidelines`,
  `chore/remove-renv`.
- Keep a PR to one concern. A focused diff gets reviewed; a sprawling
  one waits.
- Reference the issue it addresses.
- Describe what you **measured**, not only what you changed. This
  codebase’s issues are written with numbers in them — a before/after
  count, a total that now balances, the test that fails without your fix
  — and PRs are read the same way. “Fixes the drop” is much weaker than
  “recovers 1.66 Tg that the filter discarded; total now closes against
  the polity table to 1e-10”.
- If you touched a `needs-expert` area, say plainly which choice you
  made and why, so the expert reviewing it knows where to look.
- Run all six checks locally first.

Do not be discouraged if a small PR draws methodological discussion. It
usually means you found something real that nobody had decided yet.

## Reporting a bug

Please include:

- What you ran, as a copy-pasteable snippet, and what happened.
- The version of `whep` (or the commit), your R version, and your
  platform.
- Whether it reproduces on a clean `main`.
- For anything numeric: the measured magnitude. “Global total is 3.7%
  low” is actionable; “the numbers look off” cannot be triaged.

If a value is wrong rather than the code being broken, say which
quantity, for which year and area, and against what reference you
compared it.

## Conduct

Be decent to each other. Assume the person on the other side of the
review is acting in good faith and knows something you don’t. Critique
the code and the method, not the person.

Report unacceptable behaviour to the maintainers, who may remove
comments, commits or contributors as needed.
