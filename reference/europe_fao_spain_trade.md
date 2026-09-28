# Spain's historical export and import series, 1849-1960

Spain's rows of the Export and Import sheets of the compilation
`Europe_FAO_completed.xlsx`, reshaped to long format and converted from
thousand tonnes to Mg of fresh matter. It is the raw historical trade
series that
[`validate_national_trade_raw()`](https://eduaguilera.github.io/whep/reference/validate_national_trade_raw.md)
and
[`compute_trade_flows_raw()`](https://eduaguilera.github.io/whep/reference/compute_trade_flows_raw.md)
compare the provincial model against, after crosswalking `Item` to CBS
items through `cbs_trade_codes` and converting fresh matter to nitrogen.

The workbook itself is a 19-sheet, 35-country compilation and is not
redistributed with the package; only this Spanish extract is. It is
built by `data-raw/europe_fao_spain_trade_extract.R` (workbook to CSV)
and `data-raw/europe_fao_spain_trade.R` (CSV to this dataset).

## Usage

``` r
europe_fao_spain_trade
```

## Format

A tibble with one row per flow, item and year:

- `Element`: Character, `"Export"` or `"Import"`.

- `Item`: Character, trade item name as labelled in the workbook.

- `Year`: Integer year, 1849 to 1960.

- `value_fm`: Double, traded quantity in Mg of fresh matter.

## Source

Spain's rows of `Europe_FAO_completed.xlsx`; see
`data-raw/europe_fao_spain_trade_extract.R` in the source repository.

## Examples

``` r
head(europe_fao_spain_trade)
#> # A tibble: 6 × 4
#>   Element Item                 Year value_fm
#>   <chr>   <chr>               <int>    <dbl>
#> 1 Export  Almonds, with shell  1930   15211.
#> 2 Export  Almonds, with shell  1931   20071.
#> 3 Export  Almonds, with shell  1932   16703.
#> 4 Export  Almonds, with shell  1933   17329.
#> 5 Export  Almonds, with shell  1934   21761.
#> 6 Export  Almonds, with shell  1935   25938.
```
