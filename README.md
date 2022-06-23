
<!-- README.md is generated from README.Rmd. Please edit that file -->

# viscomp

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/georgiosseitidis/viscomp.svg?branch=main)](https://app.travis-ci.com/github/georgiosseitidis/viscomp)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/georgiosseitidis/viscomp?branch=main&svg=true)](https://ci.appveyor.com/project/georgiosseitidis/viscomp)
[![Codecov test
coverage](https://codecov.io/gh/georgiosseitidis/viscomp/branch/main/graph/badge.svg)](https://app.codecov.io/gh/georgiosseitidis/viscomp?branch=main)
<!-- badges: end -->

The goal of **viscomp** is to provide several visualization tools for
exploring the behavior of the components in a network meta-analysis of
complex interventions:

-   components descriptive analysis
-   heat plot of the two-by-two component combinations
-   leaving one component combination out scatter plot
-   violin plot for specific component combinations’ effects
-   density plot for components’ effects  
-   waterfall plot for the interventions’ effects that differ by a
    certain component combination
-   network graph of components
-   rank heat plot of components for multiple outcomes.

## Installation

You can install the development version of **viscomp** like so:

``` r
install.packages("devtools")
devtools::install_github("georgiosseitidis/viscomp")
```

## How to use viscomp

We illustrate how to use **viscomp** in the vignette:

``` r
vignette("viscomp", package = "viscomp")
#> Warning: vignette 'viscomp' not found
```

## How to cite **viscomp**

``` r
citation(package = "viscomp")
#> Warning in citation(package = "viscomp"): no date field in DESCRIPTION file of
#> package 'viscomp'
#> Warning in citation(package = "viscomp"): could not determine year for 'viscomp'
#> from package DESCRIPTION file
#> 
#> To cite package 'viscomp' in publications use:
#> 
#>   Seitidis G, Tsokani S, Christogiannis C, Kontouli K, Fyraridis A,
#>   Nikolakopoulos S, Veroniki A, Mavridis D (????). _viscomp: Visualize
#>   Multi-component Interventions in Network Meta-Analysis_.
#>   https://github.com/georgiosseitidis/viscomp,
#>   https://georgiosseitidis.github.io/viscomp/.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {viscomp: Visualize Multi-component Interventions in Network Meta-Analysis},
#>     author = {Georgios Seitidis and Sofia Tsokani and Christos Christogiannis and Katerina Maria Kontouli and Alexandros Fyraridis and Stavros Nikolakopoulos and Areti Angeliki Veroniki and Dimitris Mavridis},
#>     note = {https://github.com/georgiosseitidis/viscomp,
#> https://georgiosseitidis.github.io/viscomp/},
#>   }
```
