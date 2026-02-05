# NUE for Livestock —————————————————-

Calculates Nitrogen Use Efficiency (NUE) for livestock categories
(excluding pets).

The livestock NUE is defined as the percentage of nitrogen in livestock
products relative to the nitrogen in feed intake: nue = prod_n / feed_n
\* 100

Additionally, a mass balance is calculated to check the recovery of N in
products and excretion relative to feed intake: mass_balance = (prod_n +
excretion_n) / feed_n

## Usage

``` r
calculate_nue_livestock()
```

## Value

A tibble containing:

- `Year`: Year

- `Province_name`: Spanish province

- `Livestock_cat`: Livestock category

- `Item`: Produced item

- `prod_n`: Nitrogen in livestock products (Mg)

- `feed_n`: Nitrogen in feed intake (Mg)

- `excretion_n`: Nitrogen excreted (Mg)

- `nue`: Nitrogen Use Efficiency (%)

- `mass_balance`: Mass balance ratio (%)
