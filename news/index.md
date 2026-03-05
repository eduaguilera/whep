# Changelog

## whep (development version)

## whep 0.3.0

CRAN release: 2026-03-03

- Add
  [`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)
  and
  [`calculate_lmdi()`](https://eduaguilera.github.io/whep/reference/calculate_lmdi.md)
  ([@jinfama](https://github.com/jinfama),
  [\#65](https://github.com/eduaguilera/whep/issues/65)).
- Build datasets for GRAFS model in Spain
  ([@AliceBeckmann](https://github.com/AliceBeckmann),
  [\#18](https://github.com/eduaguilera/whep/issues/18)).
- Add harmonization functions
  ([@justin-morgan-csic](https://github.com/justin-morgan-csic),
  [\#66](https://github.com/eduaguilera/whep/issues/66)).

## whep 0.2.0

CRAN release: 2025-10-15

- Add gapfilling functions
  [`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md),
  [`fill_sum()`](https://eduaguilera.github.io/whep/reference/fill_sum.md)
  ([@eduaguilera](https://github.com/eduaguilera),
  [\#11](https://github.com/eduaguilera/whep/issues/11)).
- Now examples can’t fail because of unavailable Internet resources
  ([\#58](https://github.com/eduaguilera/whep/issues/58)).

## whep 0.1.0

CRAN release: 2025-07-25

- Work in Progress FABIO model implementation:
  - Build supply-use tables
    ([`build_supply_use()`](https://eduaguilera.github.io/whep/reference/build_supply_use.md))
    ([\#17](https://github.com/eduaguilera/whep/issues/17)).
  - Balance bilateral trade
    ([`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md))
    ([\#8](https://github.com/eduaguilera/whep/issues/8),
    [\#9](https://github.com/eduaguilera/whep/issues/9)).
- Create article `Follow the workflow` for new contributors
  ([\#1](https://github.com/eduaguilera/whep/issues/1),
  [\#2](https://github.com/eduaguilera/whep/issues/2),
  [\#29](https://github.com/eduaguilera/whep/issues/29)).
- Download large datasets with
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
  and `pins` package
  ([\#29](https://github.com/eduaguilera/whep/issues/29),
  [\#43](https://github.com/eduaguilera/whep/issues/43)).
- Get raw FAOSTAT data with
  [`get_faostat_data()`](https://eduaguilera.github.io/whep/reference/get_faostat_data.md)
  wrapper ([\#3](https://github.com/eduaguilera/whep/issues/3)).
- Initial CRAN submission.
