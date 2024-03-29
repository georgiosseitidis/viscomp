% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specc.R
\name{specc}
\alias{specc}
\title{Specific Component Combination violin plots}
\usage{
specc(
  model,
  sep = "+",
  combination = NULL,
  components_number = FALSE,
  groups = NULL,
  random = TRUE,
  z_value = FALSE,
  prop_size = TRUE,
  fill_violin = "lightblue",
  color_violin = "lightblue",
  adj_violin = 1,
  width_violin = 1,
  boxplot = TRUE,
  width_boxplot = 0.5,
  errorbar_type = 5,
  dots = TRUE,
  jitter_shape = 16,
  jitter_position = 0.01,
  values = TRUE
)
}
\arguments{
\item{model}{An object of class \code{\link[netmeta]{netmeta}}.}

\item{sep}{A single character that defines the separator between interventions components.}

\item{combination}{A character vector that specifies the component combinations of interest.}

\item{components_number}{\code{logical}. If \code{TRUE} the violins are created based on the number of components included in the interventions.}

\item{groups}{A character vector that contains the clusters of the number of components. Elements of the vector must be integer numbers (e.g. 5 or "5"), or range values (e.g. "3-4" ), or in the "xx+" format (e.g "5+").}

\item{random}{\code{logical}. If \code{TRUE} the random-effects NMA model is used instead of the fixed-effect NMA model.}

\item{z_value}{\code{logical}. If \code{TRUE} z-values are used instead of interventions effects.}

\item{prop_size}{\code{logical}. If \code{TRUE} in the case where \code{z_value == FALSE}, the size of the dots is proportional to the precision of the estimates.}

\item{fill_violin}{fill color of the violin. See \code{\link[ggplot2]{geom_violin}} for more details.}

\item{color_violin}{color of the violin. See \code{\link[ggplot2]{geom_violin}} for more details.}

\item{adj_violin}{adjustment of the violin. See \code{\link[ggplot2]{geom_violin}} for more details.}

\item{width_violin}{width of the violin. See \code{\link[ggplot2]{geom_violin}} for more details.}

\item{boxplot}{\code{logical}. If \code{TRUE} boxplots are plotted.}

\item{width_boxplot}{width of the boxplot. See \code{\link[ggplot2]{geom_boxplot}} for more details.}

\item{errorbar_type}{boxplot's line type. See \code{\link[ggplot2]{stat_boxplot}} for more details.}

\item{dots}{\code{logical}. If \code{TRUE} data points are plotted.}

\item{jitter_shape}{jitter shape. See \code{\link[ggplot2]{geom_jitter}} for more details.}

\item{jitter_position}{jitter position. See \code{\link[ggplot2]{geom_jitter}} for more details.}

\item{values}{\code{logical}. If \code{TRUE} median value of each violin is printed.}
}
\value{
An object of class \code{ggplot}.
}
\description{
The function based on the network meta-analysis (NMA) estimates produces violin plots from interventions that include the
component combinations of interest.
}
\details{
By default the function creates a violin for each component of the network (\code{combination = NULL}). Each violin visualizes the
distribution of the effect estimates, obtained from the interventions that include the corresponding component.
Combinations of interest are specified from the argument \code{combination}. For example, if \code{combination = c("A", "A + B")},
two violin plots are produced. The first one is based on the interventions that contain the component "A", and the second one, based
on the interventions that contain both components A and B.

By setting the argument \code{components_number = TRUE}, the behavior of intervention's effect as the number of components increased
is explored, by producing violins based on the number of components included in the interventions. If the number of
components included in a intervention ranges between 1 and 3, then 3 violins will be produced in total. The violins will be based on
the interventions that include one component, two components, and three components respectively.
The number of components could be also categorized in groups by the argument \code{groups}. For
example if \code{components_number = TRUE} and \code{groups = c("1-3", 4, "5+")}, 3 violins will be created. One for the
interventions that contain less than 3 components, one for the interventions that contain 4 components and one for those
that contain more than 5 components.

The function by default uses the NMA relative effects, but it could be adjusted to use intervention's z-scores by setting \code{z_value = TRUE}.
In the case where the NMA relative effects, the size of dots reflects the precision of the estimates. Larger dots indicates
more precise NMA estimates.
}
\note{
In the case of dichotomous outcomes, the log-scale is used in axis y. Also, the function can be applied
only in network meta-analysis models that contain multi-component interventions.
}
\examples{
data(nmaMACE)
specc(model = nmaMACE, combination = c("B", "C", "B + C"))

}
