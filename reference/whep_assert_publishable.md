# Refuse to publish restricted data

Abort if `data` was read from the restricted board, so it cannot be
uploaded as a public pin. Data counts as restricted when it carries the
attribute `whep_access = "restricted"` or a `data_access` column holding
`"restricted"`, both of which
[`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
sets. Pin upload scripts (`inst/scripts/prepare_upload.R`) call it
before writing.

## Usage

``` r
whep_assert_publishable(data)
```

## Arguments

- data:

  The object about to be published.

## Value

`data`, invisibly, when it carries no restricted mark. Otherwise an
error of class `whep_restricted_publish`.

## Restricted access

WHEP uses public data unless the caller explicitly chooses a restricted
option, and it never switches between the two on its own. A restricted
option is chosen in one of two ways:

- by reading a restricted alias of
  [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  with
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md);

- by picking a `method =` or `source =` value that is labelled
  **restricted access** in the function's documentation. The default of
  such an argument is always a public option.

Choosing a restricted option without working access aborts with an error
of class `whep_restricted_unavailable` (fields `input` and `reason`). It
says why access failed (`not_configured`, `unauthorised`, `unreachable`
or `missing`, when the board holds no such pin), how to set access up,
and which public input the registry's `public_alternative` column
suggests instead. That suggestion is advice only: nothing is read in its
place.

Data read from the restricted board carries a `data_access` column equal
to `"restricted"` (or, for a file path, a `data_access` attribute), so
its provenance stays visible downstream. Public inputs are returned
unchanged.

### Configuring access

Set these environment variables in `~/.Renviron` (not in a project
`.Renviron`, which would hide every other `WHEP_*` path):

- `WHEP_RESTRICTED_BOARD`: the address of your copy of the restricted
  folder. Either your own WebDAV path,
  `https://saco.csic.es/remote.php/dav/files/<login>/<folder>/` (saco \>
  Files \> Settings shows the WebDAV address), or the share link you
  were sent, `https://saco.csic.es/s/<token>`, which is converted to its
  WebDAV form `https://saco.csic.es/public.php/dav/files/<token>/`.

- `WHEP_RESTRICTED_SECRET`: for your own path, an app password (saco \>
  Personal settings \> Security \> Create new app password); for a share
  link, the share's password.

- `WHEP_RESTRICTED_USER` (optional): your login. It is taken from the
  WebDAV path when unset, and is `anonymous` for a share link.

Requests carry `Authorization: Basic` and
`X-Requested-With: XMLHttpRequest`; Nextcloud refuses share-link WebDAV
without the latter. Credentials are read from the environment for each
read or access check and never stored in any returned object or message.

### Republishing restricted data

Data read from the restricted board also carries the attribute
`whep_access = "restricted"`. Pin upload code calls
`whep_assert_publishable()` before writing, which refuses data carrying
either mark. The marks survive
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
and
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
but a summary can drop both, so whoever publishes derived data must
check that no restricted input fed it.

### Offering a restricted option (for package authors)

A function argument that offers a restricted option must:

- keep a public option as its default;

- label the restricted value in its `@param` with the phrase from
  `.restricted_label()`, written as an inline roxygen R expression,
  which renders as "(**restricted access**, see
  [`whep_data_access_status()`](https://eduaguilera.github.io/whep/reference/whep_data_access_status.md))";

- inherit this section with
  `@inheritSection whep_data_access_status Restricted access`;

- validate the argument with `.arg_match_access()`, whose error names
  the restricted values as such;

- read the data through
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md),
  which aborts when access fails and records `data_access`;

- record the chosen method in a `method_<quantity>` column, as for any
  other method choice.

## Examples

``` r
public <- tibble::tibble(x = 1:3)
whep_assert_publishable(public)

restricted <- tibble::tibble(x = 1:3, data_access = "restricted")
try(whep_assert_publishable(restricted))
#> Error in whep_assert_publishable(restricted) : 
#>   Refusing to publish data read from the restricted board.
#> ✖ Restricted inputs are licensed or confidential and must not be republished as
#>   a public pin.
#> ℹ Publish only data that no restricted input fed.
```
