# GLEAM manure management system shares.

Regional MMS allocation by species and system.

## Usage

``` r
gleam_mms_shares
```

## Format

A tibble with `region`, `species`, `system`, `mms`, `share_percent`.

## Source

The shipped values are unsourced placeholders, but the real GLEAM table
has been located (whep#881): FAO. 2018. *Global Livestock Environmental
Assessment Model, Model description, Version 2.0, Revision 5*. Rome,
FAO, Supplement S1, Tables 4.2-4.11 (regional MMS averages for dairy
cattle, beef cattle, feedlot cattle, dairy and non-dairy buffalo, small
ruminants, backyard/intermediate/industrial pigs and chickens), workbook
<https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Supplement_S1.xlsx>
(md5 `72fd2ea477dfe8b30cd3657b2baa4af1`, retrieved 2026-08-26). The
Version 3.0 description and its Supplement S1 publish MMS *definitions*
(Tables 4.1-4.5) but no regional shares. The shipped values disagree
materially with Table 4.2/4.3: Western Europe dairy cattle is
liquid/slurry 42%, solid storage 30%, pasture 27%, daily spread 1% in
GLEAM against 60/30/10 here, and the Sub-Saharan Africa (90/10), Latin
America (95/5) and South Asia (60/30/10) rows omit the drylot share
GLEAM gives as 35%, 42% and 54% respectively. Not re-ingested here
because no function in `R/` reads this object – the manure chain uses
`regional_mms_distribution`, which is separately unsourced.

## Examples

``` r
gleam_mms_shares
#> # A tibble: 14 × 5
#>    region             species system mms           share_percent
#>    <chr>              <chr>   <chr>  <chr>                 <dbl>
#>  1 Western Europe     Cattle  Dairy  Liquid/Slurry            60
#>  2 Western Europe     Cattle  Dairy  Solid Storage            30
#>  3 Western Europe     Cattle  Dairy  Pasture                  10
#>  4 Western Europe     Cattle  Beef   Pasture                  70
#>  5 Western Europe     Cattle  Beef   Solid Storage            30
#>  6 Sub-Saharan Africa Cattle  All    Pasture                  90
#>  7 Sub-Saharan Africa Cattle  All    Daily Spread             10
#>  8 Latin America      Cattle  All    Pasture                  95
#>  9 Latin America      Cattle  All    Solid Storage             5
#> 10 South Asia         Cattle  All    Daily Spread             60
#> 11 South Asia         Cattle  All    Solid Storage            30
#> 12 South Asia         Cattle  All    Pasture                  10
#> 13 East Asia          Pigs    All    Liquid/Slurry            70
#> 14 East Asia          Pigs    All    Solid Storage            30
```
