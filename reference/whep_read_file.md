# Download, cache and read files

Used to fetch input files that are needed for the package's functions
and that were built in external sources and are too large to include
directly. This is a public function for transparency purposes, so that
users can inspect the original inputs of this package that were not
directly processed here.

If the requested file doesn't exist locally, it is downloaded from a
public link and cached before reading it. This is all implemented using
the [`pins`](https://pins.rstudio.com/index.html) package. It supports
multiple file formats and file versioning.

## Usage

``` r
whep_read_file(file_alias, type = "parquet", version = NULL)
```

## Arguments

- file_alias:

  Internal name of the requested file. You can find the possible values
  in the `alias` column of the
  [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  dataset.

- type:

  The extension of the file that must be read. Possible values:

  - `parquet`: This is the default value for code efficiency reasons.

  - `csv`: Mainly available for those who want a more human-readable
    option. If the `parquet` version is available, this is useless
    because this function already returns the dataset in an `R` object,
    so the origin is irrelevant, and `parquet` is read faster.

  Saving each file in both formats is for transparency and accessibility
  purposes, e.g., having to share the data with non-programmers who can
  easily import a CSV into a spreadsheet. You will most likely never
  have to set this option manually unless for some reason a file could
  not be supplied in e.g. `parquet` format but was in another one.

- version:

  The version of the file that must be read. Possible values:

  - `NULL`: This is the default value. A frozen version is chosen to
    make the code reproducible. Each release will have its own frozen
    versions. The version is the string that can be found in
    [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
    in the `version` column.

  - `"latest"`: This overrides the frozen version and instead fetches
    the latest one that is available. This might or might not match the
    frozen version.

  - Other: A specific version can also be used. For more details read
    the `version` column information from
    [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md).

## Value

A tibble with the dataset. Some information about each dataset can be
found in the code where it's used as input for further processing.

## Examples

``` r
whep_read_file("read_example")
#> ℹ Fetching files for read_example...
#> # A tibble: 1 × 2
#>   col_1                col_2         
#>   <chr>                <chr>         
#> 1 I'm a sample dataset second version
whep_read_file("read_example", type = "parquet", version = "latest")
#> ℹ Fetching files for read_example...
#> # A tibble: 1 × 2
#>   col_1                col_2        
#>   <chr>                <chr>        
#> 1 I'm a sample dataset third version
whep_read_file(
  "read_example",
  type = "csv",
  version = "20250721T152646Z-ce61b"
)
#> ℹ Fetching files for read_example...
#> # A tibble: 1 × 2
#>   col_1                col_2        
#>   <chr>                <chr>        
#> 1 I'm a sample dataset first version
```
