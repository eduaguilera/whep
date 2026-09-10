# rOpenSci pre-submission enquiry — draft

**Status: DRAFT. Nothing has been sent to rOpenSci.** Submitting is the
maintainer's call. This file exists so the enquiry can be reviewed and edited
before it goes out, and so the scope argument is on the record either way.

Refs #593. Gates #46 (readiness tracker) and, indirectly, #190 (CRAN 0.4.0).

This file lives under `.github/`, which `.Rbuildignore` excludes wholesale via
`^\.github$`, so it never enters the package build. Do not move it to
`vignettes/` — that directory *is* built, and this is not a vignette.

---

## 1. How the enquiry is sent

Verified 2026-09-10.

- Mechanism: open an issue on
  <https://github.com/ropensci/software-review/issues> using the template
  **"Submit a presubmission inquiry"**
  (`.github/ISSUE_TEMPLATE/B-submit-a-presubmission-inquiry.md` in
  `ropensci/software-review`, default branch `main`). The template applies the
  label `0/presubmission` automatically.
- The policy invites it explicitly. From
  <https://devguide.ropensci.org/softwarereview_policies.html> §5.2:

  > If you are unsure whether your package fits into one of the general or
  > statistical categories, please open an issue as a pre-submission inquiry

- Section 4 below is written to that template's headings, so it can be pasted
  in with only the author handles filled.
- The template now carries a **Use of Generative AI** section, added after
  rOpenSci's preliminary AI policy of 2026-02-26
  (<https://ropensci.org/blog/2026/02/26/ropensci-ai-policy/>). It is answered
  honestly in §4; see §3.4 for why that matters here more than for most
  submissions.

## 2. Measured facts

Every number below was measured on 2026-09-10 against `main` at `8004b218`,
not carried over from an earlier issue. Several figures in #46 and in
`CLAUDE.md` are stale and are corrected here.

| Fact | Measured | How |
| --- | --- | --- |
| Version | `0.3.0.9000` (0.1.0/0.2.0/0.3.0 already on CRAN) | `DESCRIPTION` |
| Licence | MIT + file LICENSE | `DESCRIPTION` |
| `R/` scripts | 166 | `ls R/*.R \| wc -l` |
| `R/` lines | 101,855 | `cat R/*.R \| wc -l` |
| Exported objects | 261 | `grep -c 'export(' NAMESPACE` |
| Documented topics | 365 `man/*.Rd` | `ls man/*.Rd \| wc -l` |
| Shipped datasets | 56 `.rda` | `ls data/*.rda \| wc -l` |
| Exports with no `@examples` | **0 of 261** | alias-to-`\examples` map over `man/` |
| `\dontrun` / `\donttest` | **none** anywhere | `grep -rl` over `R/` and `man/` |
| `Imports` | 24 | `read.dcf("DESCRIPTION")` |
| `Suggests` | 22 | same |
| `Depends` | R (>= 4.1.0) | same |
| Test files | 184 | `ls tests/testthat/test*.R \| wc -l` |
| Test result (CI, main) | **FAIL 0 / WARN 207 / SKIP 33 / PASS 10830** | `gh run view 34340341932 --log` |
| Line coverage | **83%** | Codecov badge for `main` |
| Vignettes | 3 (`constant_territory`, `footprint-analysis`, `trade-sources-coverage`) | `ls vignettes/` |

Corrections to figures quoted elsewhere in the tracker:

- `CLAUDE.md` says "~140 scripts in `R/`, ~70k lines, 290 documented topics".
  Measured: **166 scripts, 101,855 lines, 365 topics**.
- #46 says coverage is **73.76%** and that **10 of 191** exports lack
  examples. Measured: **83%** coverage, **0 of 261** exports lack examples.
  #594 and #189 are both closed and both gates now pass.
- #46 says the suite is `FAIL 0 | PASS 5648`. Measured on CI: **PASS 10830**.
- Of #46's five failing gates, four are closed and measurably fixed (#592
  metadata via merged PR #75, #594 coverage, #189 examples, #595 README
  statement of need, #31 spell check — `tests/spelling.R` and `inst/WORDLIST`
  both present). **#593 is the only one still open.**
- #46 says `R CMD check` is **0 errors, 0 warnings, 0 notes**. Measured on the
  latest `main` run: **1 NOTE on all four platforms**, from the spell check
  #31 added. Details and the two offending words are in §2.2.

### 2.1 CI surfaces that actually exist

Seven GitHub Actions workflows, all green on `main` at `8004b218`:
`R-CMD-check`, `offline-tests`, `test-coverage`, `lint`, `pkgdown`,
`format-main`, `agent-instructions`.

- `R-CMD-check` runs on **four** platforms per PR (macOS release, Windows
  release, Ubuntu release, Ubuntu oldrel-1) plus a **weekly** Ubuntu R-devel
  leg that is `continue-on-error: true` and deliberately off the PR path.
- It **does** pass `--as-cran`: the workflow overrides only `build_args`, and
  `r-lib/actions/check-r-package@v2` defaults `args` to
  `c("--no-manual", "--as-cran")` with `error-on: "warning"`. Verified against
  the action's own `action.yaml`.
- There is a **second, independent check surface**: r-universe
  (<https://eduaguilera.r-universe.dev>). Six binaries at `8004b218`, five
  `check: OK` and one `check: WARNING` (macOS oldrel, R 4.5.3). **That one is
  not ours.** Read out of the r-universe build log
  (`gh run view 34366096456 --repo r-universe/eduaguilera --log` — the
  package's own HTML page returns HTTP 403 to automated fetches, but the
  build log does not):

  ```
  * checking package dependencies ... WARNING
  Cannot process vignettes
  Package suggested but not available for checking: 'knitr'
  VignetteBuilder package required for checking but not installed: 'knitr'
  ```

  `knitr` failed to install on that runner ("Installing from remotes: knitr"),
  so the vignette builder was absent, which also produced that leg's single
  NOTE ("Package has 'vignettes' subdirectory but apparently no vignettes").
  Both trace to one missing dependency on the build host, not to the package.
  Nothing to fix, and worth recording so it is not re-investigated.
- Six `skip_on_ci()` calls remain in the suite. `CLAUDE.md` warns these do not
  fire on r-universe, so those six run for real there. They did not cause the
  WARNING above, but they are a class of test that only this second surface
  exercises, which is a reason to keep watching it.

### 2.2 The check is not clean today — two separate things

**One ERROR that CI cannot see.** `rcmdcheck --as-cran` **ERRORs on `main` on
any host with more than four cores** (#1039; fix in open PR #1047): the
bilateral-trade code requests 16 forked workers against `R CMD check`'s
two-core limit. GitHub's runners have four cores, so CI is blind to it by luck
of the core count rather than by design. This is a genuine CRAN blocker for
#190. It should be merged before either an rOpenSci submission or a CRAN
submission, and it is worth saying in the enquiry that we know about it.

**One NOTE that CI does see, on every platform.** #46 states "`R CMD check` —
**0 errors, 0 warnings, 0 notes**". That is stale. Measured on the `main` run
`34340342167` (2026-09-09), all four platforms report:

```
Status: 1 NOTE
```

and the NOTE is the spell check that #31 introduced, failing on two words
absent from `inst/WORDLIST`:

```
* checking tests ...
  Running 'spelling.R'
  Comparing 'spelling.Rout' to 'spelling.Rout.save' ...
< Potential spelling errors:
<   WORD          FOUND IN
<   unanchored    get_bilateral_trade.Rd:30
<   unexercised   polity_area_crosswalk.Rd:165
```

This is a two-word fix to `inst/WORDLIST` (or a
`spelling::update_wordlist()`), and it is deliberately **not** bundled into
the pull request that added this file — it belongs with #31, not with a docs
change. But it means neither #46's "0/0/0" claim nor #190's "0 errors / 0
warnings / **0 notes**" definition of done is currently met, and the gap is
cheap to close. Note also that a NOTE does not fail our CI: the workflow uses
`error-on: "warning"`, so notes pass silently. That is why a stale 0/0/0 claim
could survive in the tracker for a month.

### 2.3 The data-availability picture (this is better than #593 assumed)

#593 and the brief for this work both describe the inputs as "pins on a
private board". **That is not what the code does.** Measured:

- `inst/extdata/whep_inputs.csv` lists **73** pinned inputs, all on a single
  board: a **public, unauthenticated** WebDAV share on CSIC's Nextcloud
  (`https://saco.csic.es/public.php/dav/...`), read through
  `pins::board_url()`. The board's `_pins.yaml` answers **HTTP 200 with no
  credentials** (checked 2026-09-10). Anyone can fetch it.
- **Two** third-party datasets are downloaded on demand from a DOI and
  verified against the published MD5, then cached under
  `rappdirs::user_cache_dir("whep")`: the LUH2 `states.nc` (Zenodo record
  15556812, CC-BY-4.0) and the critical-nitrogen archive (Zenodo record
  6395016, CC-BY-4.0). Both licences are cited at the point of use in
  `R/luh2_landuse.R` and `R/critical_n.R`.
- **17** `WHEP_*` environment variables appear in `R/`. Most of them point at
  multi-GB third-party archives held on local disk (CRU, HYDE, HWSD, HaNi,
  LUH2, LPJmL, wind, Natural Earth, WPP, the gridded land surfaces), and for
  those the reader **aborts with an instruction** when the variable is unset
  rather than falling back to something else. A few are only *overrides* for
  a WHEP-built pin (`WHEP_POLITY_FRACTION_PATH` became one in #694), so 17
  counts variables, not hard prerequisites.
- **15** scripts under `inst/scripts/download/` fetch those archives from
  their official sources, so each is reproducibly obtainable rather than a
  local accident.
- **The test suite never reaches the network and never reads a `WHEP_*`
  path**, and that is enforced rather than asserted: the `offline-tests` job
  runs the whole suite with `http_proxy`/`https_proxy` pointed at a **dead
  port** (`http://127.0.0.1:9`), an **empty** `XDG_CACHE_HOME` so the pin
  cache starts cold, and `CI: "false"` so the six `skip_on_ci()` tests run
  there too and cannot hide from it.

So the fair statement to rOpenSci is not "our data is private". It is: **a
reviewer can install the package, run every example, run the full test suite
and build the site with nothing but a clone; they cannot re-run the full
pipeline end to end without tens of GB of third-party downloads and about a
dozen environment variables.** That is the reviewability constraint worth
disclosing, and it is a much weaker objection than inaccessible data would be.

The durability question is separate and real: an institutional Nextcloud
public link is not an archive with a DOI. #457 already prefers verified
on-demand DOI downloads over pins for exactly this reason. A reviewer may well
raise it.

## 3. Honest scope assessment

### 3.1 rOpenSci's criteria, quoted at source

All quotes from <https://devguide.ropensci.org/softwarereview_policies.html>,
read 2026-09-10.

The framing sentence (§5.2):

> rOpenSci aims to support packages that enable reproducible research and
> manage the data lifecycle for scientists. Packages submitted to rOpenSci
> should fit into one or more of the categories outlined below.

The categories that `whep` could plausibly claim (§5.2.1, verbatim):

> **data munging**: Packages for processing data from formats above. This area
> does not include broad data manipulation tools such as reshape2 or tidyr, or
> tools for extracting data from R code itself. Rather, it focuses on tools
> for handling data in specific scientific formats generated from scientific
> workflows or exported from scientific instruments.

> **geospatial data**: We accept packages focused on accessing geospatial
> data, manipulating geospatial data, and converting between geospatial data
> formats.

> **data retrieval**: Packages for accessing and downloading data from online
> sources with scientific applications. […] retrieval packages should be
> focused on data sources or topics, rather than services, and should do more
> than just download data.

> **data validation and testing**: Tools that enable automated validation and
> checking of data quality and completeness as part of scientific workflows.

Two exclusions that bear directly on `whep` (§5.2 and §5.2.1, verbatim):

> For instance, data visualization packages are no longer in scope.

> Statistical/ML libraries for modelling or prediction are typically not
> included in this category

And the generality expectation (§5.2.2, verbatim):

> Packages should be general in the sense that they should solve a problem as
> broadly as possible while maintaining a coherent user interface and code
> base.

The statistical track is a **separate** system with its own categories
(<https://stats-devguide.ropensci.org/overview.html>), which are: Bayesian and
Monte Carlo Routines; Regression and Supervised Learning; Dimensionality
Reduction, Clustering and Unsupervised Learning; EDA and Summary Statistics;
Time Series Analyses; Machine Learning; Spatial Analyses; Probability
Distributions. From that page:

> Any software which fits in to one or more of these categories may be deemed
> in-scope, and submitted for review, while software which cannot be described
> by any of these categories will generally be deemed out of scope.

That track is live: rOpenSci announced new statistical editors in its
February 2026 news digest
(<https://ropensci.org/blog/2026/02/26/news-february-2026/>).

On being on CRAN already (§5.1.1, verbatim):

> We strongly suggest submitting your package for review before publishing on
> CRAN or submitting a software paper describing the package to a journal.
> Review feedback may result in major improvements and updates to your
> package, including renaming and breaking changes to functions. We do not
> consider previous publication on CRAN or in other venues sufficient reason
> to not adopt reviewer or editor recommendations.

> Do not submit your package for review while it or an associated manuscript
> is also under review at another venue, as this may result in conflicting
> requests for changes.

And on rejection timing (§5.1, verbatim):

> Rejections are usually done early (before the review process begins, see the
> aims and scope section)

That last sentence is the whole argument for #593 going first: a scope
rejection costs one issue, and it arrives before any reviewer is recruited.

### 3.2 What `whep` actually is, measured

A line-count classification of all 166 files in `R/` into three buckets. The
split is a judgement call, so the rule is stated rather than asserted: bucket
3 is a fixed list of 29 infrastructure files (`utils.R`, `table_schema.R`,
`toy_examples.R`, the dataset docs, the pins plumbing, and so on); of the
rest, anything whose filename matches a modelling or indicator subsystem
(`io_model`, `footprint`, `_extension`, `carbon_balance`, `soc`, `n_balance`,
`nitrogen`, `n2o`, `nh3`, `leach`, `water_balance`, `bnf`, `critical_n`,
`crop_npp`, `manner_model`, `livestock`, `manure`, `feed`, `enteric`,
`methane`, `excret`, `decomposition`, `lmdi`, `typolog`, `grafs`,
`circularity`, `nourishment`, `protein_`, `energy_co2`, …) is bucket 1, and
everything remaining is bucket 2. Reasonable people would move a handful of
files; the conclusion does not depend on which:

| Bucket | Files | Lines | Share |
| --- | --- | --- | --- |
| Domain modelling and derived indicators | 95 | 55,657 | **54.6%** |
| Data access, harmonisation, geospatial | 42 | 33,786 | 33.2% |
| Package infrastructure, docs, fixtures | 29 | 12,412 | 12.2% |

Bucket 2 is the part that fits rOpenSci's categories: FAOSTAT/LUH2/LPJmL/CRU/
HYDE/HWSD readers, the item and area harmonisation tables, the commodity
balance sheets, the trade matrices, the gap-filling, the year-aware polity
model, and the 0.5°-grid spatialisation. That is squarely "handling data in
specific scientific formats generated from scientific workflows", and the
gridded half is squarely geospatial.

Bucket 1 is the part that does not: five soil-carbon models behind
`calculate_soc_dynamics()`, a gridded nitrogen balance with selectable NH3 /
N2O / leaching methods, a Leontief MRIO engine (`build_io_model()`,
`build_footprint()`) with per-stressor extensions, livestock enteric-methane
and manure-emission models, LMDI decomposition, and the nourishment /
protein-quality / circularity indicator families.

Two further measured details that cut against fit:

- **24 of 261 exports (9.2%) are `plot_*` functions**, 5,885 lines across five
  files. `whep` is not a visualisation package, but data visualisation is
  explicitly out of scope, and roughly a tenth of the public API is exactly
  that.
- **`whep` is no longer a data-retrieval package in any meaningful sense.**
  `get_faostat_data()` and its scraper were **removed** in #999 (commit
  `8004b218`, i.e. the current tip). FAOSTAT now arrives as curated snapshots
  through our own pins board. Only the two Zenodo readers download anything
  from a third party. So "data retrieval" should probably *not* be ticked on
  the template, even though it would have been tickable a month ago.

### 3.3 The argument against fit, stated properly

1. **The majority of the code is in no listed category.** 54.6% of the lines
   are process-based modelling and derived indicators. The general categories
   do not include domain modelling, and §5.2.1 says outright that
   "Statistical/ML libraries for modelling or prediction are typically not
   included".
2. **The statistical track does not rescue it either.** `whep`'s models are
   *mechanistic and deterministic* — ODE-integrated soil carbon pools, IPCC
   emission factors, a Leontief inverse — not statistical estimation. Of the
   eight statistical categories, only "Spatial Analyses" is even arguable, and
   that is a poor description of a gridded mass balance. There is no inference,
   no likelihood, no uncertainty propagation to write `srr` standards against.
   So the honest reading is that `whep` falls **between** the two tracks rather
   than into either.
3. **Generality.** §5.2.2 asks for packages that "solve a problem as broadly as
   possible while maintaining a coherent user interface and code base". `whep`
   is the code of one ERC project and its `Description` says so. Some exported
   surfaces are visibly project-internal rather than general — the public API
   includes `create_alfredos_typologies()`,
   `create_typologies_of_josette()` and `create_typologies_grafs_spain()`,
   i.e. **exported functions named after individual people**, which the
   project's own `CLAUDE.md` naming rule forbids for new files and which a
   reviewer will read as an internal artefact escaping into the API. A
   reviewer could reasonably read the whole package as one project's pipeline
   released as a package, not as a general tool — and 261 exports across a
   102k-line code base is a lot of surface for a reviewer to find coherent.
4. **Reviewability.** A reviewer can run the tests and the examples offline,
   but cannot reproduce a published number without tens of GB of external
   archives. Review would therefore be a review of the code and the API, not
   of the results. That is normal for large scientific packages, but it is
   worth naming rather than discovering in round two.
5. **Size and reviewer load.** 102k lines, 261 exports, 365 topics, 24
   imports. rOpenSci reviews are volunteer work with a nominal effort
   expectation measured in hours. Even a scope-positive editor may struggle to
   recruit two reviewers.
6. **We are already on CRAN**, which §5.1.1 says buys nothing procedurally and
   explicitly warns may cost us breaking changes for existing users.

### 3.4 The generative-AI disclosure, and why it is load-bearing here

Measured on `main`: **1,040 of 3,359 commits (31%)** carry a
`Co-Authored-By: Claude` trailer; since 2026-06-01 it is **861 of 1,677
(51%)**. The repository ships an agent instruction file (`CLAUDE.md`), an
`agent-instructions` workflow, and its own conventions for fleets of agents
working in parallel worktrees.

The trailer count is an imperfect proxy in both directions — some
agent-assisted work may lack the trailer, and a co-authored commit is not
necessarily wholly generated — but the order of magnitude is not in doubt and
must be disclosed. rOpenSci's policy does not forbid it
(<https://ropensci.org/blog/2026/02/26/ropensci-ai-policy/>):

> Our initial policy updates are not intended to restrict use of generative AI
> tools.

but it does say authors will be asked

> to describe their use, and to affirm that all generated material has been
> carefully reviewed by the authors

and warns that

> extensive AI use could increase the time to find reviewers.

For `whep` that last sentence is a practical risk on the same order as the
scope risk, and the enquiry is the right place to raise it rather than the
submission. Note also that the affirmation is about *maintainer* review of
generated material — that is a claim only the maintainers can make, and it
should be checked internally before anyone ticks that box.

### 3.5 The argument for fit

- The data-harmonisation third of the package is a strong, unusual fit for
  *data munging*: reconciling FAOSTAT's several vintages, LUH2, HYDE, CRU,
  HWSD and LPJmL onto one item/area/year schema is precisely "handling data in
  specific scientific formats generated from scientific workflows", and almost
  nothing on CRAN does it. The nearest package, `FAOSTAT` (CRAN 2.4.2,
  2026-05-07), is a FAOSTAT/WDI download client and does not overlap with
  `whep`'s harmonisation, balances, or gridding — so §5.2.3 package
  overlap is not an obstacle.
- The gridded work (0.5° cell × polity land, water and nutrient surfaces,
  LUH2/HYDE/HWSD raster ingestion) is a clean *geospatial data* fit.
- The year-aware polity model — resolving historical territories back to 1850
  so a series is attributed to the state that existed at the time — is a
  genuinely reusable contribution beyond this project, and beyond agriculture.
- Engineering readiness is real and measured, not aspirational: `--as-cran`
  with 0 errors and 0 warnings on four platforms (1 NOTE — see §2.2 — and
  #1039 off a 4-core host), 83% coverage, 10,830 passing
  tests, an offline-enforced suite, 261 of 261 exports with runnable examples,
  no `\dontrun`, `CITATION.cff`, `codemeta.json`, contributing guide, code of
  conduct, MIT licence, pkgdown site, and a second check surface on
  r-universe.
- The package could be *scoped* for review rather than rejected: an editor
  could review the data-access and harmonisation surface and treat the
  modelling as out-of-review-scope internals. Asking whether that is possible
  is the single most useful question in the enquiry.

### 3.6 Our own read

**Scope fit is genuinely uncertain and, on the measured composition, more
likely negative than positive for the package as a whole.** The most probable
good outcome is not "yes, in scope" but "in scope if you submit a narrower
package". A plausible editor response is to suggest splitting the
harmonisation layer into its own package and submitting that — which would be
a substantial piece of work, and is a decision the team should have an opinion
about *before* asking, because it may well be the answer.

## 4. The draft enquiry — text to paste

Fill in the author handles and confirm the AI paragraph with the maintainers
before sending.

---

Submitting Author Name: Catalin Covaci
Submitting Author Github Handle: @lbm364dl
Other Package Authors Github handles: @eduaguilera
Repository: https://github.com/eduaguilera/whep
Submission type: Pre-submission
Language: en

---

- Paste the full DESCRIPTION file inside a code block below:

```
<paste the current DESCRIPTION here verbatim at the time of sending>
```

## Scope

- Please indicate which category or categories from our package fit policies
  or statistical package categories this package falls under:

    **Data Lifecycle Packages**

	- [ ] data retrieval
	- [ ] data extraction
	- [x] data munging
	- [ ] data deposition
    - [ ] data validation and testing
	- [ ] workflow automation
	- [ ] version control
	- [ ] citation management and bibliometrics
	- [ ] scientific software wrappers
	- [ ] field and lab reproducibility tools
	- [ ] database software bindings
	- [x] geospatial data
	- [ ] translation

     **Statistical Packages**

	- [ ] Bayesian and Monte Carlo Routines
	- [ ] Dimensionality Reduction, Clustering, and Unsupervised Learning
	- [ ] Machine Learning
	- [ ] Regression and Supervised Learning
	- [ ] Exploratory Data Analysis (EDA) and Summary Statistics
	- [ ] Spatial Analyses
	- [ ] Time Series Analyses
	- [ ] Probability Distributions

- Explain how and why the package falls under these categories (briefly, 1-2
  sentences). Please note any areas you are unsure of:

We are writing a pre-submission enquiry rather than a submission because we
think roughly a third of this package is in scope and over half of it is not,
and we would rather hear that from an editor now than after doing the
remaining readiness work.

`whep` harmonises the public record of world agriculture — production, feed,
trade, processing and consumption — from FAOSTAT's several vintages plus LUH2,
HYDE, CRU, HWSD and LPJmL output into one tidy item/area/year schema reaching
back to 1850, including a year-aware model of historical polities so a series
is attributed to the territory that existed at the time. It also produces
0.5°-gridded land, water and nutrient surfaces from those sources. That part
we believe is **data munging** ("handling data in specific scientific formats
generated from scientific workflows") and **geospatial data**.

**We want to be direct that this is not most of the package.** We classified
all 166 files in `R/` by line count: 33% is the data access and harmonisation
work described above, 12% is package infrastructure, and **55% is domain
modelling and derived indicators** — five soil-carbon models behind one
`method=`-selectable entry point, a gridded nitrogen balance with selectable
NH3/N2O/leaching methods, a Leontief multi-regional input-output engine with
per-stressor extensions, livestock enteric-methane and manure-emission models,
LMDI decomposition, and nourishment/protein-quality indicator families.

Domain modelling is not one of the general categories, and §5.2.1 says
statistical/ML modelling libraries are typically excluded. We also do not
think the statistical track fits: our models are mechanistic and deterministic
(ODE-integrated carbon pools, IPCC emission factors, a Leontief inverse), with
no inference, likelihood or uncertainty propagation, so of the eight
statistical categories only "Spatial Analyses" is even arguable and it is a
poor description of a gridded mass balance. Our honest reading is that the
package falls between the two tracks.

So our questions are:

1. Does the modelling majority put the package out of scope outright?
2. If not, could a review be **scoped** to the data-access and harmonisation
   surface, treating the modelling as out-of-review-scope internals — and is
   that something rOpenSci does?
3. If neither, would you expect us to split the harmonisation layer out as its
   own package and submit that instead? We would like to know if that is the
   likely answer before investing in it.

Three more things we should disclose rather than have you find:

- **About a tenth of the public API is plotting.** 24 of 261 exports are
  `plot_*` functions (5,885 lines). We know data visualisation is no longer in
  scope. `whep` is not a visualisation package, but that surface exists.
- **We are already on CRAN** (0.1.0, 0.2.0, 0.3.0). We have read §5.1.1 and
  understand that this buys nothing procedurally and that review may ask for
  breaking changes. Nothing about this package is under review at any other
  venue, and we are not submitting a software paper concurrently.
- **We are not a data-retrieval package**, and were less so last month than we
  expected to be: we removed our FAOSTAT scraper, and FAOSTAT now reaches the
  package as curated snapshots on a public pins board. Only two third-party
  datasets are fetched directly, both from Zenodo DOIs with published-MD5
  verification.
- **Parts of the public API are project-internal.** A handful of exports are
  named after the colleagues whose method they implement
  (`create_alfredos_typologies()`, `create_typologies_of_josette()`), and some
  are specific to one country's case study. We would expect a review to ask us
  to either generalise or un-export those, and we think that would be right.

- If submitting a statistical package, have you already incorporated
  documentation of standards into your code via the **srr** package?

No — we do not believe this is a statistical package (see above). If an editor
reads it as one, we have not started `srr` work.

- Who is the target audience and what are scientific applications of this
  package?

Researchers in agricultural and environmental history, food-system
sustainability, and environmentally extended input-output analysis. The
package is the computational core of the ERC-funded "Who Has Eaten the Planet"
project, whose subject is the century of agricultural change before 1961,
which is very poorly quantified because most harmonised statistics begin where
FAOSTAT begins. We released it as a package so the pipeline behind those
numbers can be read, re-run and criticised rather than cited as a black box.
We do not claim a user base beyond the project team and collaborators today.

- Are there other R packages that accomplish the same thing? If so, how does
  yours differ or meet your criteria for best-in-category?

Not to our knowledge. `FAOSTAT` (CRAN 2.4.2) is a download client for the
FAOSTAT and WDI APIs; it does not harmonise across FAOSTAT vintages, does not
extend series before 1961, has no polity model, and does no gridding or
balance construction. The methodology our input-output half follows is FABIO
(Bruckner et al. 2019, doi:10.1021/acs.est.9b03554), whose reference
implementation (<https://github.com/fineprint-global/fabio>) is a set of R
scripts rather than a package, and which is national-scale rather than
historical. We are not aware of any package we would be duplicating.

- (If applicable) Does your package comply with your guidance around Ethics,
  Data Privacy and Human Subjects Research?

There are no human subjects and no personal data — all inputs are national and
gridded aggregate statistics. On terms of use: the two datasets we download
directly are CC-BY-4.0 Zenodo records and the licence is cited at the point of
use in the code. We would welcome a reviewer's view on whether our
redistribution of curated third-party snapshots through our own pins board is
documented well enough; we think each source's licence permits it, but it is
not currently summarised in one place in the docs.

- Any other questions or issues we should be aware of?:

**Reviewability.** A reviewer can install the package, run all 261 exports'
examples, run the full test suite (10,830 passing tests, 83% line coverage) and
build the pkgdown site with nothing but a clone — the suite never touches the
network or any local data path, and that is enforced rather than asserted by a
CI job that runs the suite behind a dead proxy, with an empty cache directory,
and with `CI` set false so that even our `skip_on_ci()` tests run. What a
reviewer **cannot** do without tens of GB of third-party archives and a dozen
environment variables is re-run the full pipeline end to end and reproduce a
published number. Fifteen scripts under `inst/scripts/download/` fetch those
archives from their official sources, so it is possible, but it is not an
afternoon. If review requires end-to-end reproduction, we should know now.

**Durability of our own inputs.** 73 curated inputs are served from a public,
unauthenticated WebDAV share on our institution's Nextcloud. It needs no
credentials, but it is not an archive with a DOI, and we are aware that is a
weakness.

**Size.** 102k lines across 166 files, 261 exports, 365 documented topics, 24
imports. We mention it because it bears on your ability to find reviewers, and
because if the answer to question 2 above is "scope the review", the size
problem largely goes away.

**Two open check findings**, so that a `pkgcheck` run against `main` does not
surprise anyone. `R CMD check --as-cran` currently ERRORs on any host with
more than four cores: one code path requests 16 forked workers against the
check limit of two. Our CI runners have four cores, so CI does not see it; the
fix is in an open pull request and will land before any submission. And the
check reports 1 NOTE on every platform — our spell check flags two words
missing from the package word list. Both will be closed before we submit.

## Use of Generative AI

- [x] Generative AI tools were used to produce some of the material in this
  submission.

Extensively, and we would rather say so plainly. Measured on the default
branch: 1,040 of 3,359 commits (31%) carry a `Co-Authored-By: Claude` trailer,
and since June 2026 it is 861 of 1,677 (51%). The repository carries its own
agent instruction file, `CLAUDE.md`
(<https://github.com/eduaguilera/whep/blob/main/CLAUDE.md>), which sets the
rules that work is judged against — chiefly that a change affecting published
numbers must carry measured before/after evidence, that a bug fix must ship a
test that fails before it, and that every result-affecting coefficient must
carry a source at the point of use or an explicit "assumed, unverified" note.
Commit trailers are an imperfect proxy in both directions, but the order of
magnitude is not in doubt.

All generated material is reviewed by the human maintainers before merge, and
the numeric output is checked against independent statistics by a separate
ground-truth harness kept outside the package build. We have read your
February 2026 note and understand that extensive AI use may lengthen the
search for reviewers; we would rather you weigh that at the enquiry stage.

---

## 5. What the answer unblocks or blocks

### If the answer is "in scope" (whole package, unlikely)

#46 has less left in it than the tracker says. Four of its five listed gates
are already closed and measurably passing (§2). What actually remains before a
submission:

1. Merge PR #1047 so `--as-cran` is clean off a 4-core host (#1039).
2. Close the 1 NOTE: add `unanchored` and `unexercised` to `inst/WORDLIST`
   (§2.2). Belongs with #31. Two words, and it is the difference between
   #46's claim and #46's reality.
3. Re-run `pkgcheck::pkgcheck(".", goodpractice = TRUE, use_cache = FALSE)`
   against `main` and confirm the gates from the measured numbers, not from
   #46's stale table. Mind the two traps recorded in #46: the stale
   `/tmp/rcheck/whep.Rcheck` and the false "no continuous integration checks".
4. Agree internally how much breaking change we would accept, since §5.1.1
   warns review can require it and we have CRAN users.

### If the answer is "in scope only if scoped/narrowed" (most likely good case)

This is a **design decision, not readiness work**, and it should be taken
before any more polishing:

- Splitting the harmonisation layer into its own package is weeks of work and
  a permanent change to how the project ships. It should be decided on its own
  merits — a cleaner separation between "harmonised agricultural data" and
  "models built on it" may be worth doing regardless of rOpenSci — not
  accepted as the price of a badge.
- Until that decision is taken, **#46 should be paused**, not worked. Its
  remaining items are cheap; the decision above is not.

### If the answer is "out of scope"

- **#46 should be closed**, with the editor's answer recorded in it. rOpenSci's
  own §5.2.2 points elsewhere: "For packages that are not in the scope of
  rOpenSci, we encourage submitting them to CRAN, BioConductor, as well as
  other R package development initiatives […] and software journals such as
  JOSS, JSS, or the R journal". For a package of this kind, JOSS or
  *Environmental Modelling & Software* is the better target, and #46's own
  "Once accepted" section already lists those.
- Nothing in #46's *engineering* content is wasted — coverage, examples,
  spell-check, metadata are all good for CRAN too, and all already done.
- **#190 is unaffected either way.** Its blockers are CRAN mechanics and
  correctness (#1039, the 1 NOTE in §2.2, #183 tarball size, #184 vignette
  network fetch, and the correctness list), not rOpenSci readiness. Note that
  #190's definition of done demands **0 notes**, so §2.2's spell-check NOTE is
  a #190 blocker in its own right, regardless of what rOpenSci says. #190
  should proceed on its own
  schedule and **should not wait on this enquiry.** The one thing the enquiry
  changes for #190 is the ordering advice in §5.1.1: rOpenSci prefers review
  *before* CRAN publication. Since 0.1.0–0.3.0 are already on CRAN, that ship
  has sailed, and there is no reason to delay 0.4.0 for a review decision.

### Either way

The single highest-value output of this enquiry is a written editor answer in
a public issue that we can point at, so that this question is not reopened in
six months from memory.

## 6. Not verified

Stated so nobody quotes these as settled:

- **A fresh local `pkgcheck` run.** The gate figures in §2 are measured
  directly (coverage from Codecov, tests from the CI log, examples and exports
  from the tree) but `pkgcheck` itself was not re-run in this session, so its
  optional flags — the >80-character line count, the exported-name collisions
  with `transx`/`FSK2R`/`cape`, the cyclomatic-complexity list — are still
  #46's August figures and may have moved.
- **A local `covr::package_coverage()` figure.** A run was started and did not
  finish within the session. The 83% cited is Codecov's number for `main`,
  produced by the same `covr` run in the `test-coverage` workflow, which is
  the right number to quote anyway.
- **Whether rOpenSci has ever scoped a review to part of a package.** The
  policy text does not say, either way. Question 2 in the enquiry asks it
  directly rather than assuming an answer.
- **The line-count split in §3.2 is a judgement call**, not a fact. The file
  assignments are reproducible and listed in the PR that added this document,
  but a different reviewer would draw the boundary somewhat differently. The
  conclusion is robust to reasonable redrawing: the modelling share is
  comfortably above half either way.
- **The maintainers' affirmation that all AI-generated material has been
  reviewed** (§3.4) is written into the draft as a claim, but only the
  maintainers can actually make it. Confirm it before sending.
