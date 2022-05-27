#' Components Rank Heat Plot
#'
#' @description
#' Rank heat plot summarizes the components' p-scores for multiple outcomes.
#'
#' @details
#' The function creates a rank heat plot, where the number of circles depend on the number of outcomes.
#' Each circle is divided by the total number of components, and each sector is colored according
#' the corresponding component p-score. Components' p-scores are summarized by using either the median (\code{median = TRUE})
#' or the mean (\code{median = FALSE}) of the p-scores obtained from the interventions that include the corresponding component.
#' The sector's colors reflect the magnitude of the components p-scores. Red color indicates a low p-score (close to zero),
#' while green color indicates values close to 1. Intervention's p-scores are obtained from the network meta-analysis (NMA) model.
#' By default the random-effects NMA model is used for each outcome (\code{random = TRUE}).
#'
#'
#' @param model A list of \code{\link[netmeta]{netmeta}} models.
#' @param sep A single character that defines the separator between interventions components.
#' @param median \code{logical}. If \code{TRUE} the median is used as a summary measure instead of the mean.
#' @param random \code{logical}. If \code{TRUE} the random-effects NMA model is used instead of the fixed-effects NMA model.
#' @param small.values A \code{character} vector that specifies whether small intervention effects indicate a beneficial (\code{small.values = "good"}) or a harmful (\code{small.values = "bad"}) effect. If \code{small.values = NULL} small values assumed \code{good} for each outcome.
#' @param outcomeNames A character vector that specifies the names of the outcomes.
#' @param cex_components Font size of components' names.
#' @param cex_values Font size of p-scores.
#' @param cex_outcomes Font size of outcomes' names.
#'
#' @importFrom circlize circos.clear circos.par circos.initialize circos.trackPlotRegion circos.text get.cell.meta.data circos.trackPlotRegion draw.sector
#'
#' @return Returns (invisibly) a rank heat plot.
#' @export
#'
#' @examples
#' # Artificial data set
#'
#' t1 <- c("A", "B", "C", "A+B", "A+C", "B+C", "A")
#' t2 <- c("C", "A", "A+C", "B+C", "A", "B", "B+C")
#'
#' TE1 <- c(2.12, 3.24, 5.65, -0.60, 0.13, 0.66, 3.28)
#' TE2 <- c(4.69, 2.67, 2.73, -3.41, 1.79, 2.93, 2.51)
#'
#' seTE1 <- rep(0.1, 7)
#' seTE2 <- rep(0.2, 7)
#'
#' study <- paste0("study_", 1:7)
#'
#' data1 <- data.frame(
#'   "TE" = TE1, "seTE" = seTE1, "treat1" = t1, "treat2" = t2, "studlab" = study,
#'   stringsAsFactors = FALSE
#' )
#'
#' data2 <- data.frame(
#'   "TE" = TE2, "seTE" = seTE2, "treat1" = t1, "treat2" = t2, "studlab" = study,
#'   stringsAsFactors = FALSE
#' )
#'
#' # Network meta-analysis models
#'
#' net1 <- netmeta::netmeta(
#'   TE = TE, seTE = seTE, studlab = studlab, treat1 = treat1,
#'   treat2 = treat2, data = data1, ref = "A"
#' )
#'
#' net2 <- netmeta::netmeta(
#'   TE = TE, seTE = seTE, studlab = studlab, treat1 = treat1,
#'   treat2 = treat2, data = data2, ref = "A"
#' )
#'
#' # Rank heat plot
#'
#' rankheatplot(model = list(net1, net2))
rankheatplot <- function(model, sep = "+", median = TRUE, random = TRUE, small.values = NULL, outcomeNames = c("Outcome 1", "Outcome 2"),
                         cex_components = NULL, cex_values = NULL, cex_outcomes = NULL) {

  ##
  # Check arguments
  ##
  random <- check.arguments(model, median, random, small.values, outcomeNames, cex_components, cex_values, cex_outcomes)

  ##
  # Build all components of the network
  ##

  if (is.null(small.values)) {
    small.values <- rep("good", length(model))
  }

  numOfOutcomes <- length(model)

  listcomponents <- list()
  stop_fun <- NULL
  for (outcome in 1:numOfOutcomes) {
    # Find all components of the data
    nodes <- unique(c(model[[outcome]]$treat1, model[[outcome]]$treat2))

    nodes <- gsub(" ", "", nodes)
    # Components of the network
    uniquecomponents <- unique(unlist(strsplit(nodes, split = paste("[", sep, "]", sep = ""), perl = TRUE)))
    # save components in list
    listcomponents[outcome] <- list(uniquecomponents)

    stop_fun <- c(stop_fun, length(uniquecomponents) == length(nodes))
  }
  components <- unique(do.call(c, listcomponents))

  if (sum(stop_fun) > 0) {
    stop("No additive treatments are included in the NMA model", call. = FALSE)
  }

  results <- build.data(model, median, random, small.values, numOfOutcomes, components, outcomeNames, sep)

  ##
  # Draw the donughts of the rankheatplot
  ##
  cex <- drawDonughts(outcomeNames, results$components, cex_components)

  ##
  # Generate intervals
  ##
  gen_intervals <- generateIntervals(results$df)

  ##
  # Add the background color of the rad
  ##

  addColorBackground(outcomeNames,
    components = results$components, data = results$df,
    gen_intervals$intervals, gen_intervals$mycolors,
    cex = cex, cex_values, cex_outcomes
  )
}
