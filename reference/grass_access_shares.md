# Accessibility and conversion parameters for grazable grass availability.

Accessibility and conversion parameters for grazable grass availability.

## Usage

``` r
grass_access_shares(aboveground = 0.46, grazable = 1, w_c_dm = 0.45)
```

## Arguments

- aboveground:

  Fraction of total grass NPP that is above ground (the grazable
  compartment; LPJmL `pft_npp` is whole-plant). Default 0.46.

- grazable:

  Sustainable fraction of above-ground forage that can be grazed.
  Default 1 (the full above-ground ceiling); set below 1 to impose a
  sustainable-offtake share.

- w_c_dm:

  Carbon-to-dry-matter mass fraction. Default 0.45.

## Value

A named list with `aboveground`, `grazable` and `w_c_dm`.

## Examples

``` r
grass_access_shares(grazable = 0.6)
#> $aboveground
#> [1] 0.46
#> 
#> $grazable
#> [1] 0.6
#> 
#> $w_c_dm
#> [1] 0.45
#> 
```
