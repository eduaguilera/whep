# N production for Spain ———————————————–

Calculates N production at the provincial level in Spain. Production is
derived from consumption, export, import, and other uses.

## Usage

``` r
create_n_production()
```

## Value

A tibble containing:

- `Year`: Year

- `Province_name`: Spanish province

- `Item`: Product item

- `Box`: Ecosystem box

- `prod`: Produced N (Mg)
