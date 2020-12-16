
<!-- README.md is generated from README.Rmd. Please edit that file -->

## shinyhydat

[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

This is a shiny app to view and download historical streamflow data from
a downloaded HYDAT SQLite database and real-time data using
[tidyhydat](https://github.com/ropensci/tidyhydat).

## Setup

These instructions assume that you have [installed R and
RStudio](https://github.com/bcgov/bcgov-data-science-resources/wiki/Installing-R-&-RStudio)
already on your computer.

### Package downloads

The first step to getting the shinyhydat up and running on your machine
is to download all the packages needed by the shiny app. All of these
packages are available on CRAN with the exception of `tidyhydat`. Type
the following into R console to install all of them:

``` r
install.packages("tidyhydat")
install.packages("shinydashboard")
install.packages("leaflet")
install.packages("plotly")
install.packages("devtools")
install.packages("shinyWidgets")
install.packages("shiny")
```

### HYDAT download

The second step to using the shinyhydat is to download a version of the
HYDAT database, Environment and Climate Change Canada’s comprehensive
database of historical hydrometric data. The `tidyhydat` package
provides a convenience function to download HYDAT (be patient though
this takes a long time\!):

``` r
tidyhydat::download_hydat()
```

tidyhydat functions will automatically know where to look for HYDAT if
you download HYDAT using this function.

## Usage

  - Open app.R file in RStudio and run the app.

### License

    Copyright 2017 Province of British Columbia
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 
    
       http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
