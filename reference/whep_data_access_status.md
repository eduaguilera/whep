# Check access to restricted input datasets

Most inputs in
[`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
are read from a public board. Some cannot be public: licensed or
confidential data, or data shared with the project under an agreement.
Those rows have `access == "restricted"` and are read from a private
board that each authorised person reaches with their own credentials, so
nobody shares a common secret (#1386).

This function lists the restricted inputs registered in
[`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
and whether each is usable on this machine. It never returns or prints
the secret, the login or the full board address.

## Usage

``` r
whep_data_access_status(check = TRUE)
```

## Arguments

- check:

  If `TRUE`, the default, contact the board to verify that it answers,
  accepts the credentials and holds each input. If `FALSE`, only report
  whether the environment variables are set.

## Value

A tibble with one row per restricted input in
[`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md),
or one row with `input = NA` describing the board itself when none is
registered. Columns:

- `input`: the restricted alias.

- `public_alternative`: the public alias suggested instead, or `NA`.

- `configured`: whether `WHEP_RESTRICTED_BOARD` and
  `WHEP_RESTRICTED_SECRET` are set.

- `credential_type`: `"account"` (own WebDAV path), `"share_link"`, or
  `NA` when not configured.

- `host`: the board's host name, or `NA`.

- `status`: `"available"`, `"not_configured"`, `"not_checked"`,
  `"unauthorised"`, `"unreachable"` or `"missing"`.

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
[`whep_assert_publishable()`](https://eduaguilera.github.io/whep/reference/whep_assert_publishable.md)
before writing, which refuses data carrying either mark. The marks
survive
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
  `whep_data_access_status()`)";

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
whep_data_access_status(check = FALSE)
#> # A tibble: 1 × 6
#>   input public_alternative configured credential_type host  status        
#>   <chr> <chr>              <lgl>      <chr>           <chr> <chr>         
#> 1 NA    NA                 FALSE      NA              NA    not_configured
```
