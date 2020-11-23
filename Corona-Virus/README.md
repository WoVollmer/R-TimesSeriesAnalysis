
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Corona Virus Dashboard

The **`Corona Virus Dashboard`** provides analysis of the time series
data provided by the **Johns Hopkins University** on GitHub. For links
and references see the Dashboard-file
*Corona\_Virus\_TS\_Dashboard.html* rsp. RMD-file
*Corona\_Virus\_TS\_Dashboard.Rmd*.

## Overview

The **`Corona_Virus_TS_Dashboard`** provides an interactive dashboard
with multiple pages and page navigation.

The R Markdown file **`Corona_Virus_TS_Dashboard.Rmd`** creates the
dashboard with the R package

  - [flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/)
    *flexdashboard: Easy interactive dashboards for R*

## Dashboard Web Site

The [Corona Virus Dashboard](https://wovollmer.github.io/github.io/) is
published as **GitHub page**

## Installation

### Johns Hopkins University data files

<https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series>

Following data/files are required to run R Markdown file
**Corona\_Virus\_TS\_Dashboard.Rmd**

  - **Data files**:
      - *time\_series\_covid19\_confirmed\_global.csv*
      - *time\_series\_covid19\_deaths\_global*
      - *time\_series\_covid19\_recovered\_global.csv* (note: not used
        since data not consistend and w/o real value add)

### GitHub repository files

<https://github.com/WoVollmer/R-TimesSeriesAnalysis/tree/master/Corona-Virus>

  - `Corona_Virus_TS_Dashboard.Rmd` R Markdown file for dashboard
    creation
  - R Markdown child files for separate dashboard pages
      - *`Page_world_map.Rmd`*
      - *`Page_bar_chart.Rmdd`*
      - *`Page_cumulative_and_daily_trend.Rmd`*
      - *`Page_exp_linear_growth.Rmd`*
  - `Corona_raw_data.R` provides function to read and process the time
    series raw data of the John Hopkins University Corona Hopkins
  - `world_population_un.RDS` R object file providing UN world
    population data
  - `References_Corona.bib` Bibtex file providing the references with
    the Bibtexkeys

Not used for dashboard creation:

  - `corona_data.RDS` R object file storing in between generated corona
    data, usable for test purposes

### R package `pkgTS` on GitHub

R package `pkgTS` providing functions for the **T**ime **S**eries
analysis.

<https://github.com/WoVollmer/pkgTS>

R installation by

  - `library("devtools")`
  - `devtools::install_github("WoVollmer/pkgTS")`

The used R function files are

  - `pkgTS/R/ggts_corona.R` - providing the functions to create the
    plots
  - `pkgTS/R/uts_corona.R` - providing utility functions

-----

Any remarks?  
Please let me know or provide some hints.
