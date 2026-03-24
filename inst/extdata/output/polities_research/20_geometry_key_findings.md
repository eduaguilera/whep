# CRITICAL GEOMETRY FINDINGS

## Discovery 1: CShapes 2.0 Already Has Colonial Polygons

**The current pipeline is NOT extracting all available CShapes data.**

CShapes 2.0 includes four categories of dependent territories:
1. **Colonies**
2. **Protectorates**
3. **International mandates**
4. **Occupied territories**

The R package `cshapes::cshp()` returns ALL of these by default. The full dataset
has ~930 country-period entries globally. However, the current pipeline's
`.clean_cshapes()` function produces only 315 entries in `cshapes.csv`.

**This means CShapes likely already has polygons for many of the "missing" polities**:
- British Malaya (1886-1957)
- French Indochina (1887-1954)
- Belgian Congo (1886-1960)
- German East Africa (1886-1919)
- British East Africa (1895-1963)
- French West Africa (1895-1960)
- And many more colonial territories

**Action needed**: Re-examine `.load_cshapes()` and `.filter_relevant_area_changes()`
to understand why these colonial entries aren't making it into the final output.
The `status` column in CShapes data indicates whether an entity is sovereign or
dependent — the current pipeline may be filtering these out.

## Discovery 2: CShapes-Europe Extends to 1816

CShapes 2.0 has a European extension that goes back to **1816**, not just 1886.
This means it should include:

- **All pre-unification Italian states** (Papal States, Two Sicilies, Sardinia,
  Tuscany, Modena, Parma) from 1816-1860
- **Pre-1864 Denmark** (with Schleswig-Holstein)
- **Pre-unification German states** (dozens of microstates)
- **Ottoman Empire in Europe** from 1816

The paper states: CShapes 2.0 covers "dozens of microstates that existed before
the German and Italian unifications."

**Action needed**: Check if `cshapes::cshp()` returns these pre-1886 European
entities. The current code assumes CShapes starts at 1886 (line 484:
`start_year = ifelse(start_year <= 1886, NA, start_year)`), which may be
discarding valuable pre-1886 European data.

## Discovery 3: The cshapes.csv Gap

Looking at `data-raw/constants.R` (line 35-41):
```r
k_cshapes <- .clean_cshapes()
k_cshapes |>
  sf::st_set_geometry(NULL) |>
  readr::write_csv(file.path(polities_inputs_path, "cshapes.csv"))
```

The `k_cshapes` object in `sysdata.rda` contains the geometry, but `cshapes.csv`
strips it ("Can't afford also saving geometry"). The `.clean_cshapes()` function
runs `.filter_relevant_area_changes()` which:
1. Groups by `country_name`
2. Removes same-year entries
3. Aggregates adjacent entries with no area change

This filtering may be what removes colonial/dependent entries. Need to verify
whether `cshapes::cshp()` returns colonial entries and if they survive the filter.

## Discovery 4: Geo-Larhra (Free Italian States Polygons)

A free academic dataset from CNRS/University of Lyon specifically covers
Italian state boundaries 1815-1866:
- **URL**: http://geo-larhra.ish-lyon.cnrs.fr/?q=geocatalogue/vectors
- **Format**: Shapefile
- **Coverage**: International boundaries in Italy, 1815-1866
- **Free**: Yes (academic)

This could fill the Italian pre-unification gap even if CShapes-Europe doesn't work.

## Revised Geometry Strategy

### Before These Findings
We assumed 195 polities lacked geometry and planned 4 different strategies.

### After These Findings
**Most of those 195 polities may already have CShapes geometry that the current
pipeline isn't using.** The revised priority is:

1. **FIRST**: Check what `cshapes::cshp()` actually returns. Install the R package,
   call `cshp()`, examine the `status` column, count entries. This could immediately
   resolve 50-100 of the missing polities.

2. **SECOND**: Check if pre-1886 European data exists in CShapes by examining entries
   with dates before 1886.

3. **THIRD**: For remaining gaps, use GADM/Natural Earth as planned.

4. **FOURTH**: For pre-unification Italian states (if CShapes-Europe doesn't have them),
   use Geo-Larhra or GADM Italian regions as approximation.

## Impact on the Pipeline

If CShapes has colonial polygons, the pipeline needs minimal changes:
- Modify `.clean_cshapes()` to NOT filter out dependent territories
- Modify `rename_cshapes.csv` to add name mappings for colonial entities
- Add `common_names.csv` entries for the new CShapes colonial entries

This is FAR less work than building polygons from GADM.
