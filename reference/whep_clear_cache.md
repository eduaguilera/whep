# Clear the build pipeline cache

Removes cached results from
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md),
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md),
and
[`build_processing_coefs()`](https://eduaguilera.github.io/whep/reference/build_processing_coefs.md)
so that the next call rebuilds from scratch.

## Usage

``` r
whep_clear_cache()
```

## Value

Invisible `NULL`.

## Memory that clearing the cache does not return

Clearing the cache releases the cached tibbles to R, but the process may
keep far more resident memory than it holds. That excess is not held by
the cache or by any reachable object, so neither this function nor
[`gc()`](https://rdrr.io/r/base/gc.html) recovers it (whep#777).

It is memory R has already freed and the C allocator keeps. On Linux,
glibc serves a large allocation with its own mapping, which goes back to
the operating system on free, only while it is bigger than the "mmap
threshold". Each such free raises that threshold (up to 32 MB), so later
vectors below it come from the heap, and freed heap blocks stay
resident. The build chain frees many vectors of that size.

Measured with
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
and
[`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
for 2008-2012, after [`gc()`](https://rdrr.io/r/base/gc.html), holding
1.17 GB of live R objects: 5.5-5.8 GB resident by default, 3.3 GB with a
fixed threshold, identical outputs. Fixing the threshold switches the
raising off. glibc reads it when the process starts, so it must be set
in the shell that launches R, not in `.Renviron`:

    GLIBC_TUNABLES=glibc.malloc.mmap_threshold=131072 Rscript build.R

Other platforms and allocators are unaffected by the variable.

## Examples

``` r
whep_clear_cache()
#> ✔ Build cache cleared.
```
