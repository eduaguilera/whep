# Data Sources for Livestock Coefficients

This document lists the sources for all coefficient tables used in the `whep` package for livestock emission calculations.

| Table Name | Description | Source Document | Table/Page | Notes |
| :--- | :--- | :--- | :--- | :--- |
| `gleam_coefs` | GLEAM 3.0 model parameters | GLEAM 3.0 Supplement S1 | Various Sheets | Extracted from `GLEAM_3.0_Supplement_S1.xlsx` |
| `ipcc_tier1_enteric` | Tier 1 Enteric Fermentation EFs | IPCC 2019 Refinement | Vol 4, Ch 10, Tab 10.10, 10.11 | kg CH4/head/year |
| `ipcc_tier1_manure` | Tier 1 Manure Management EFs | IPCC 2019 Refinement | Vol 4, Ch 10, Tab 10.14 (derived) | kg CH4/head/year, kg N2O-N/head/year |
| `ipcc_tier2_energy` | Tier 2 Energy Requirements | IPCC 2019 Refinement | Vol 4, Ch 10 | Cfi, Ca, Cp coefficients |
| `ipcc_tier2_methane` | Tier 2 Methane Conversion Factors | IPCC 2019 Refinement | Vol 4, Ch 10 | Ym, Bo, MCF values |
| `livestock_weights` | Default animal weights | Literature / IPCC Defaults | - | Representative values (e.g., Cattle 550kg) |
| `feed_parameters` | Feed digestibility and energy params | IPCC 2019 Refinement | Vol 4, Ch 10 | DE%, UE, REM, Ash content |
| `manure_parameters` | Manure characteristics | IPCC 2019 Refinement | Vol 4, Ch 10 | N excretion, N2O EFs |
| `livestock_constants` | Physical constants | IPCC / Physics | - | Energy content of CH4, etc. |
| `gleam_default_cohort_shares`| Default cohort splits | GLEAM 3.0 / Assumption | - | Used when specific GLEAM data missing |
| `gleam_default_system_shares`| Default system splits | GLEAM 3.0 / Assumption | - | Used when specific GLEAM data missing |
