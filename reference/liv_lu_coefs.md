# Livestock unit coefficients

Provides livestock unit (LU) conversion factors per head for each animal
class, used to express heterogeneous livestock populations in comparable
units.

## Usage

``` r
liv_lu_coefs
```

## Format

A tibble where each row corresponds to one animal class. It contains the
following columns:

- `Animal_class`: Animal class identifier (e.g., `"Dairy_cows"`,
  `"Cattle"`, `"Sheep_goats"`, `"Broilers"`, `"Hens"`, `"Pigs"`).

- `LU_head`: Livestock units per head (numeric). Dairy cows have a value
  of 1.0 by convention; smaller animals have proportionally lower
  values.

## Source

Based on standard livestock unit definitions from FAO and European
agricultural statistics.
