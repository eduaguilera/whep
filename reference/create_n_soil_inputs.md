# Nitrogen (N) soil inputs for Spain ———————————-

Calculates total nitrogen inputs to soils in Spain at the provincial
level. This includes contributions from:

- Atmospheric deposition (`deposition`)

- Biological nitrogen fixation (`fixation`)

- Synthetic fertilizers (`synthetic`)

- Manure (excreta, solid, liquid) (`manure`)

- Urban sources (`urban`)

Special land use categories and items are aggregated:

- Semi-natural agroecosystems (e.g., Dehesa, Pasture_Shrubland)

- Firewood biomass (e.g., Conifers, Holm oak)

## Usage

``` r
create_n_soil_inputs()
```

## Value

A tibble containing:

- `Year`: Year

- `Province_name`: Spanish province

- `Item`: Crop, land use, or biomass item

- `Box`: Land use or ecosystem box for aggregation

- `deposition`: N input from atmospheric deposition (Mg)

- `fixation`: N input from biological N fixation (Mg)

- `synthetic`: N input from synthetic fertilizers (Mg)

- `manure`: N input from livestock manure (Mg)

- `urban`: N input from urban sources (Mg)
