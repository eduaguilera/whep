# N soil inputs and Nitrogen Use Efficiency (NUE) for crop ————-

N inputs (deposition, fixation, synthetic fertilizers, urban sources,
manure) and N production in Spain from 1860 to the present for the GRAFS
model at the provincial level. The crop NUE is defined as the percentage
of produced nitrogen relative to the total nitrogen inputs to the soil.
Total soil inputs are calculated as: inputs = deposition + fixation +
synthetic + manure + urban

## Usage

``` r
calculate_nue_crops()
```

## Value

A tibble containing nitrogen input, production, and NUE data. It
includes the following columns:

- `Year`: Year.

- `Province_name`: The Spanish province.

- `Item`: The item which was produced, defined in `names_biomass_cb`.

- `Box`: One of the two systems of the GRAFS model: cropland or
  semi-natural agroecosystems.

- `deposition`: Atmospheric nitrogen deposition in megagrams (Mg).

- `fixation`: Nitrogen fixation in megagrams (Mg).

- `synthetic`: Synthetic nitrogen fertilizer applied to the land in
  megagrams (Mg).

- `manure`: Nitrogen in manure applied to the land in megagrams (Mg).

- `urban`: Nitrogen in wastewater from human sources in megagrams (Mg).

- `prod`: Produced nitrogen in megagrams (Mg).

- `inputs`: Total nitrogen inputs in megagrams (Mg).
