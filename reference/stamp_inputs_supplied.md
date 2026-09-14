# Record which optional inputs a build consumed

Write the provenance stamp
[`check_inputs_supplied()`](https://eduaguilera.github.io/whep/reference/check_inputs_supplied.md)
reads: a character column naming the optional inputs this build actually
had, comma-separated and sorted, or `"none"` when it had none. The point
of the stamp is that it is a **label**: unlike a total, it cannot be
satisfied by arithmetic, so a consumer can tell an absent layer from a
measured zero years later and without the producer's arguments in hand.

## Usage

``` r
stamp_inputs_supplied(data, supplied, column = "inputs_supplied")
```

## Arguments

- data:

  A tibble to stamp.

- supplied:

  Keys of the inputs that were supplied. May be empty.

- column:

  Name of the stamp column.

## Value

`data` with `column` added or overwritten.

## Examples

``` r
stamp_inputs_supplied(tibble::tibble(x = 1:2), c("ice", "water"))
#> # A tibble: 2 × 2
#>       x inputs_supplied
#>   <int> <chr>          
#> 1     1 ice,water      
#> 2     2 ice,water      
stamp_inputs_supplied(tibble::tibble(x = 1:2), character())
#> # A tibble: 2 × 2
#>       x inputs_supplied
#>   <int> <chr>          
#> 1     1 none           
#> 2     2 none           
```
