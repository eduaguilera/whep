# As-published MCF alternatives to [climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md).

Methane conversion factors transcribed from Table 10.17 of each IPCC
edition, over the same `mms_type` / `climate_zone` key space as
[climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
so that either can be substituted for it. Selected with the `mcf_source`
manure-engine option (see
[manure_engine_options](https://eduaguilera.github.io/whep/reference/manure_engine_options.md)).
Since whep#1022 the `edition == "ipcc_2019"` rows are the **default**:
the 2019 Refinement is the current IPCC guidance, and the six cells of
[climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
whose provenance could not be established should not be what ships.
[climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
stays reachable as `mcf_source = "as_shipped"` so an older run can be
reproduced.

Six cells of
[climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
match no published IPCC value, and three of the `mms_type` labels that
carry them – dry lot, the two composting rows and the anaerobic digester
– are unreachable on the live Tier 2 path, because
[regional_mms_distribution](https://eduaguilera.github.io/whep/reference/regional_mms_distribution.md)
routes manure to only six systems and none of them is one of those. The
cells that *are* live and edition-dependent are pasture/range/paddock,
liquid/slurry and the anaerobic lagoon, and it is those three that the
default change moves.

## Usage

``` r
climate_mcf_ipcc
```

## Format

A tibble with columns:

- edition:

  `"ipcc_2006"` or `"ipcc_2019"`, the two values the `mcf_source` option
  takes.

- mms_type:

  Manure-management system, in the
  [climate_mcf](https://eduaguilera.github.io/whep/reference/climate_mcf.md)
  vocabulary, plus the six 2019 anaerobic-digester classes.

- climate_zone:

  `"Cool"`, `"Temperate"` or `"Warm"`.

- mcf_percent:

  Methane conversion factor (percent of `Bo` achieved). `NA` where the
  edition publishes no default – the 2006 anaerobic digester, which is
  "0-100 percent, calculate".

## Source

IPCC. 2006. *2006 IPCC Guidelines for National Greenhouse Gas
Inventories*, Vol 4, Ch 10, Table 10.17 "MCF values by temperature for
manure management systems", pp. 10.44-10.47 (PDF md5
`97b97e8d0e4ca101c77fac2d63a0cb86`).

IPCC. 2019. *2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories*, Vol 4, Ch 10, Table 10.17 (Updated)
"Methane conversion factors for manure management systems", pp.
10.68-10.70 (PDF md5 `c1784e747af9bb307e93f4170c12679a`).

Both PDFs re-downloaded from `ipcc-nggip.iges.or.jp` and checksummed on
2026-09-08. Every cell here was read off them; the collapse rules above
are the only step that is not a transcription.

## Collapse rules

Neither edition publishes exactly three numbers for every system, so two
collapse rules are applied. Both are WHEP choices, stated here because
they are not IPCC statements – and because the 2019 edition is now the
default, the second of them is live on every Tier 2 run:

- **2006 per-degree rows** (liquid/slurry, uncovered anaerobic lagoon)
  are read at the middle column of each temperature class. Table 10.15
  of the same chapter defines the classes as Cool below 15 C, Temperate
  15 to 25 C and Warm above 25 C, and Table 10.17 groups its 19
  per-degree columns the same way, so the middle columns are 12 C, 20 C
  and 27 C. Liquid/slurry takes the "without natural crust cover" series
  and the optional 40 percent crust reduction is not applied.

- **2019 sub-zone rows** are the unweighted mean over the sub-zones the
  Refinement itself groups into each of Cool (Cool Temperate Moist, Cool
  Temperate Dry, Boreal Moist, Boreal Dry), Temperate (Warm Temperate
  Moist, Warm Temperate Dry) and Warm (Tropical Montane, Tropical Wet,
  Tropical Moist, Tropical Dry). Liquid/slurry is read at the 6-month
  retention time, which is the Refinement's own default where retention
  time is unknown (Table 10.17 footnote 1).

## The 2019 pasture value is half of a pair

The Refinement's single 0.47 percent for pasture, range and paddock is
**not** a drop-in replacement for the 2006 triple. Section 10.4.2 of the
same chapter states that it "must be used in conjunction with a single
B0 value of 0.19 m3 CH4 kg-1 of VS excreted", and that this pair "was
judged by the expert panel to be more accurate than emission factors
estimated from regionally based MCFs and animal category based B0" –
which is precisely what WHEP computes. `.calc_manure_ch4_tier2()`
multiplies one per-species `Bo` from
[ipcc_tier2_bo_values](https://eduaguilera.github.io/whep/reference/ipcc_tier2_bo_values.md)
by the share-weighted MCF, so it cannot hold a system-specific `Bo`
without computing the product per manure stream instead. Because
`"ipcc_2019"` is the default, **the shipped Tier 2 path now runs that
hybrid**: the Refinement's pasture MCF against WHEP's animal-category
`Bo`, which is the combination the Refinement rejects.

Measured, on FAOSTAT 2020 heads at the Temperate default, repricing only
the pasture stream at `Bo` 0.19 moves global Tier 2 manure CH4 by
**+0.18 percent** (14.385 to 14.411 Tg). It is small in total because
the species that reach Tier 2 are dominated by cattle, for which pasture
is only 3.2 percent of the weighted MCF, and because sheep already carry
`Bo` 0.19 exactly. It is not small everywhere: buffalo (`Bo` 0.10) would
rise **30.5 percent**, goats (0.18) 5.6 percent and cattle fall 0.07
percent. Horses (0.30), mules and asses (0.33) and camels (0.26) would
move most of all, but contribute no Tier 2 CH4 today because they are
dropped for want of cohort and energy inputs. Restructuring the kernel
to a per-stream `Bo` is out of scope for the table and open in
whep#1022.

## What the table cannot hold

The 2019 Refinement resolves the anaerobic digester into **six**
leakage-and-storage classes spanning 1.00 to 13.17 percent, so no single
`"Anaerobic Digester"` MCF exists to ship. They are carried as six
distinct `mms_type` values rather than flattened to one number, and no
shipped MMS vocabulary selects one: which class a country's digesters
belong to is an inventory input WHEP does not have (whep#1022). The 2006
digester is `NA` for the same reason – that edition publishes a 0 to 100
percent range and requires the compiler to evaluate its Formula 1.

## Examples

``` r
climate_mcf_ipcc
#> # A tibble: 81 × 4
#>    edition   mms_type      climate_zone mcf_percent
#>    <chr>     <chr>         <chr>              <dbl>
#>  1 ipcc_2006 Daily Spread  Cool                 0.1
#>  2 ipcc_2006 Daily Spread  Temperate            0.5
#>  3 ipcc_2006 Daily Spread  Warm                 1  
#>  4 ipcc_2006 Solid Storage Cool                 2  
#>  5 ipcc_2006 Solid Storage Temperate            4  
#>  6 ipcc_2006 Solid Storage Warm                 5  
#>  7 ipcc_2006 Dry Lot       Cool                 1  
#>  8 ipcc_2006 Dry Lot       Temperate            1.5
#>  9 ipcc_2006 Dry Lot       Warm                 2  
#> 10 ipcc_2006 Liquid/Slurry Cool                20  
#> # ℹ 71 more rows
```
