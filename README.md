
<!-- README.md is generated from README.Rmd. Please edit that file -->

# viscomp

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/georgiosseitidis/viscomp?branch=main&svg=true)](https://ci.appveyor.com/project/georgiosseitidis/viscomp)
<!-- badges: end -->

## Description

The goal of **viscomp** is to provide several visualization tools for
exploring the behavior of the components in a network meta-analysis of
multi-component (complex) interventions:

-   components descriptive analysis
-   heat plot of the two-by-two component combinations
-   leaving one component combination out scatter plot
-   violin plot for specific component combinations’ effects
-   density plot for components’ effects  
-   waterfall plot for the interventions’ effects that differ by a
    certain component combination
-   network graph of components
-   rank heat plot of components for multiple outcomes.


## Reference

[Seitidis, G., Tsokani, S., Christogiannis, C., Kontouli, K.-M., Fyraridis, A., Nikolakopoulos, S., Veroniki, A.A. and Mavridis, D. (2022), Graphical tools for visualizing the results of network meta-analysis of multicomponent interventions. Res Syn Meth. Accepted Author Manuscript.](https://doi.org/10.1002/jrsm.1617)


## Installation

### Current development version on GitHub:


Installation using R package
[**devtools**](https://cran.r-project.org/package=devtools):

``` r
install.packages("devtools")
devtools::install_github("georgiosseitidis/viscomp")
```


Installation using R package
[**remotes**](https://cran.r-project.org/package=remotes):

``` r
install.packages("remotes")
remotes::install_github("georgiosseitidis/viscomp")
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
Meta-Analysis (Version 1.0.0).
<https://github.com/georgiosseitidis/viscomp>

**BibTeX**

@manual{Seitidis_viscomp_Visualize_Multi-component_2022, author =
{Seitidis, Georgios and Tsokani, Sofia and Christogiannis, Christos and
Kontouli, Katerina Maria and Fyraridis, Alexandros and Nikolakopoulos,
Stavros and Veroniki, Areti Angeliki and Mavridis, Dimitris}, title =
{{viscomp: Visualize Multi-component Interventions in Network
Meta-Analysis}}, url = {<https://github.com/georgiosseitidis/viscomp>},
year = {2022} }
