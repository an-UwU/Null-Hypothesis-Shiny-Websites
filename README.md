# STA304 Student AI Use Survey

Null Hypothesis — STA304H5F Surveys, Sampling and Observational Data, Fall 2026.

**Website:** [Student AI Use Survey](https://01a20eeb-55f5-084c-dbca-79693b2ad46b.share.connect.posit.cloud/)

## Overview

An interactive R Shiny website exploring AI tool choices, academic purposes, weekly coursework usage, and intentions to switch tools among STA304H5 students.

The survey contains 112 voluntary responses, including 110 AI users. Recruitment through Piazza and in-person invitations produced a self-selected, non-probability sample; findings may not represent all STA304H5 students.

## Features

- Shared filters for primary AI tool, future tool choice, and weekly usage hours.
- Three charts with selectable colour palettes, including tool choices by academic purpose and a weekly-hours histogram.
- Descriptive statistics, exploratory statistical results, and study limitations.
- A searchable response table and aggregate-summary download.

## Analysis

Tool choice and academic purpose are compared using a chi-square test with a conditional Monte Carlo p-value (100,000 simulations; seed 304). Switching intention is modelled using quadratic logistic regression with weekly hours and hours squared. Results update with the filters, with unavailable tests clearly identified.

Skipped answers remain missing. The shared dataset excludes student identifiers. In the academic-purpose chart, Other AI combines DeepSeek and Yuanbao; the chi-square test retains the original categories.

## Run locally

Requires R and the `shiny`, `bslib`, `ggplot2`, and `DT` packages. From the project directory:

```r
source("install_packages.R")
shiny::runApp(".")
```

## Source files

- `app.R`: application entry point and navigation.
- `R/`: filters, charts, statistical analysis, and shared functions.
- `data/survey.csv`: survey data without student identifiers.
- `www/`: styles and website icon.
- `manifest.json`: deployment dependencies and application files.
