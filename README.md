
<!-- README.md is generated from README.Rmd. Please edit that file -->

# viscomp

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/georgiosseitidis/viscomp?branch=main&svg=true)](https://ci.appveyor.com/project/georgiosseitidis/viscomp)
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
```

## How to cite viscomp

**APA**

Seitidis, G., Tsokani, S., Christogiannis, C., Kontouli, K. M.,
Fyraridis, A., Nikolakopoulos, S., Veroniki, A. A., & Mavridis, D.
(2022). viscomp: Visualize Multi-component Interventions in Network
Meta-Analysis (Version 0.1.0).
<https://github.com/georgiosseitidis/viscomp>

**BibTeX**

@manual{Seitidis_viscomp_Visualize_Multi-component_2022, author =
{Seitidis, Georgios and Tsokani, Sofia and Christogiannis, Christos and
Kontouli, Katerina Maria and Fyraridis, Alexandros and Nikolakopoulos,
Stavros and Veroniki, Areti Angeliki and Mavridis, Dimitris}, title =
{{viscomp: Visualize Multi-component Interventions in Network
Meta-Analysis}}, url = {<https://github.com/georgiosseitidis/viscomp>},
year = {2022} }
