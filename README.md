
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Who Has Eaten the Planet <a href="https://eduaguilera.github.io/whep/"><img src="man/figures/logo.png" align="right" height="139" alt="whep website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/eduaguilera/whep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eduaguilera/whep/actions/workflows/R-CMD-check.yaml)
[![whep status
badge](https://eduaguilera.r-universe.dev/whep/badges/version)](https://eduaguilera.r-universe.dev/whep)
<!-- badges: end -->

## Project

#### **Who Has Eaten the Planet? The paths of food systems beyond the safe and just operating space (1850-2020)**

Food production covers the most basic human need, and simultaneously is
the main driver of anthropogenic environmental impacts. These impacts
have resulted in the transgression, during the brief period since the
industrial revolution, of the planetary boundaries defining the safe
operating space of humanity. A rich research literature quantifies the
last 60 years’ fast, heterogeneous, and often unfair development in food
supply and related environmental impacts, and how these depend on
agro-climatic factors, technology, and trade flows, all of which have
greatly changed but with different trajectories around the world.
However, these developments lack an integrated approach, and are very
poorly quantified before 1961. WHEP will bridge these knowledge gaps,
assessing “who has eaten the planet” by answering the questions:

> What are the environmental impacts of food production since 1850?

> What is the role of trade in food supply and in displacing the
> responsibilities for these impacts?

> How are impacts related to planetary boundaries, food supply and
> inequality?

These highly ambitious goals are addressed by four objectives:

1.  Constructing a consolidated global country-level annual database on
    agricultural production and management, using massive data collation
    in combination with modelling.
2.  Estimating the environmental impacts: greenhouse gas emissions and
    carbon, land, water, nitrogen, and phosphorus through spatially
    explicit, integrated, dynamic modelling.
3.  Calculating product footprints and tracing them along international
    trade chains.
4.  Analyzing the observed trajectories in the safe and just operating
    space, by assessing the drivers, and how impacts at the production
    and consumption levels are related to fair and healthy supply. This
    ground-breaking research will shed new light on the environmental
    history of food, opening up many new research frontiers, and
    providing necessary information to design fair and sustainable
    policies.

You can also visit the [European project
site](https://cordis.europa.eu/project/id/101115126).

## R package

The WHEP project heavily relies on data. We use the R programming
language. This repository is built as an R package containing
functionality that we think might be useful to share to others as part
of the project. This will also include functions for easily downloading
the data gathered by the project.

## Installation

The package is still in an early stage and thus a work in progress, so
it’s still not on CRAN. It’s however already available on R-universe:

``` r
install.packages('whep', repos = c('https://eduaguilera.r-universe.dev'))
```

You can also install the development version of `whep` available on
[GitHub](https://github.com/eduaguilera/whep) with:

``` r
pak::pak("eduaguilera/whep")
```

## Usage

You can read more about the package’s functionalities from the
documentation at the [reference
page](https://eduaguilera.github.io/whep/reference/index.html).

## Contributing

We try to follow best coding practices, specifically focused on R
package creation. The process is roughly summarized in:

  - Use git. Work on your own branch.
  - Track dependencies using `renv` R package.
  - Add your new functionality inside `R/` directory as functions.
  - Add function documentation.
  - Write clean code. Follow [Tidyverse style
    guide](https://style.tidyverse.org/).
  - Write tests for your code.
  - Create pull requests. Ask for review.

The project is starting with **contributors** that are still learning
about coding and best practices. For this reason **we have created a
guide** explaining most of the things you need from the previous steps,
covering both git and R package development. You can find the guide
[**here**](https://eduaguilera.github.io/whep/articles/workflow-intro.html).
Anyone is welcome to contribute, but we highly recommend to go through
this guide to become familiar with the workflow if you are still not
used to it.
