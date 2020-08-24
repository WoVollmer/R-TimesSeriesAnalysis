
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Corona Virus Dashboard

The **`Corona Virus Dashboard`** provides analysis of the time series
data provided by the **Johns Hopkins University** on GitHub. For links
and references see the Dashboard-file
*Corona\_Virus\_TS\_Dashboard.html* rsp. RMD-file
*Corona\_Virus\_TS\_Dashboard.Rmd*.

## Installation

Following data/files are required to run R Markdown file
**Corona\_Virus\_TS\_Dashboard.Rmd**

  - **Data files**:
      - *time\_series\_covid19\_confirmed\_global.csv*
      - *time\_series\_covid19\_deaths\_global*
      - *time\_series\_covid19\_recovered\_global.csv* (note: not used
        since w/o real value add)
  - **ggts\_corona.R** R file providing the functions to create the
    plots
  - **world\_population\_un.RDS** R data file providing the population
    numbers of the different countries (based on UN data)
  - **References\_Corona.bib** Bibtex file providing the references with
    the Bibtexkeys

## Overview

The **`Corona_Virus_TS_Dashboard`** provides an interactive dashboard
with multiple pages and page navigation.

The R Markdown file **`Corona_Virus_TS_Dashboard.Rmd`** creates the
dashboard with the R package

  - [flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/)
    *flexdashboard: Easy interactive dashboards for R*

-----

Any remarks?  
Please let me know or provide some hints.
