#' Leaving One Component Combination Out Scatter plot
#'
#' @description
#' The function based on the network meta-analysis (NMA) estimates explores if a set of components has a
#' positive or a negative impact on the outcome, by creating a scatter plot based on the set of
#' interventions that differ by a specific set of components.
#'
#' @details
#' Axis y represents the intervention's effect when the component combination is not included in the
#' intervention, while axis x represents the intervention's effect when is included.
#' Line \eqn{y = x} splits the plot in two parts. For a beneficial outcome, dots above the line
#' indicates that the inclusion of component combination balk the intervention's efficacy, while
#' dots below the line indicate that the inclusion of the component combination increases intervention's efficacy.
#' The opposite holds for harmful outcomes.
#'
#' The component combination of interest is specified by the argument \code{combination}. For example, if \code{combination = "A"}, the
#' function plots all the interventions that differ by the component \code{"A"}. If \code{combination = NULL}, all interventions
#' that differ by one component are plotted.
#'
#' The function by default uses the z-values estimates from the NMA model (\code{z_value = TRUE}). Histograms
#' for the nodes that include and not include the component combination could be added to the scatter plot,
#' by setting the argument \code{histogram = TRUE}.
#'
#'
#' @param model An object of class \code{\link[netmeta]{netmeta}}.
#' @param sep A single character that defines the separator between interventions components.
#' @param combination A single character that specifies the component combination of interest.
#' @param random \code{logical}. If \code{TRUE} the random-effects NMA model is used instead of the fixed-effect NMA model.
#' @param z_value \code{logical}. If \code{TRUE} z-values are used instead of interventions effects.
#' @param histogram \code{logical}. If \code{TRUE} histograms are added to the plot.
#' @param histogram.color A single character that specifies the color of the histogram. See \code{\link[ggExtra]{ggMarginal}} for more details.
#'
#' @importFrom ggplot2 ggplot aes `%+%` geom_point geom_line labs
#' @importFrom ggExtra ggMarginal
#'
#' @return An object of class \code{\link[ggplot2]{ggplot}}.
#' @export
#'
#' @examples
#' data(MACE)
#' NMAdata <- netmeta::pairwise(
#'   studlab = Study, treat = list(treat1, treat2, treat3, treat4),
#'   n = list(n1, n2, n3, n4), event = list(event1, event2, event3, event4), data = MACE, sm = "OR"
#' )
#' net <- netmeta::netmeta(
#'   TE = TE, seTE = seTE, studlab = studlab, treat1 = treat1,
#'   treat2 = treat2, data = NMAdata, ref = "UC"
#' )
#' loccos(model = net, combination = c("B"))
loccos <- function(model, sep = "+", combination = NULL, random = TRUE, z_value = TRUE, histogram = TRUE, histogram.color = "blue") {

  ##
  # Check arguments
  ##

  if (class(model) != "netmeta") {
    stop("The class of model is not of netmeta", call. = FALSE)
  } else if (model$reference.group == "") {
    stop("The netmeta model must have a reference group", call. = FALSE)
  } else if (class(sep) != "character") {
    stop("The class of sep is not logical", call. = FALSE)
  } else if (length(sep) > 1) {
    stop("The length of sep must be one", call. = FALSE)
  } else if (sep == "") {
    stop("Argument sep must be diffent than ''", call. = FALSE)
  } else if (!is.null(combination) & class(combination) != "character") {
    stop("The class of combination is not character", call. = FALSE)
  } else if (!is.null(combination) & length(combination) > 1) {
    stop("The length of combination must be one", call. = FALSE)
  } else if (class(random) != "logical") {
    stop("The class of random is not logical", call. = FALSE)
  } else if (length(random) > 1) {
    stop("The length of random must be one", call. = FALSE)
  } else if (class(z_value) != "logical") {
    stop("The class of z_value is not logical", call. = FALSE)
  } else if (length(z_value) > 1) {
    stop("The length of z_value must be one", call. = FALSE)
  } else if (class(histogram) != "logical") {
    stop("The class of histogram is not logical", call. = FALSE)
  } else if (length(histogram) > 1) {
    stop("The length of histogram must be one", call. = FALSE)
  } else if (!class(histogram.color) %in% c("character", "numeric")) {
    stop("The class of histogram.color must be character or numeric", call. = FALSE)
  } else if (length(histogram.color) != 1) {
    stop("The length of histogram.color must be one", call. = FALSE)
  }

  ##
  # NMA estimates and characteristics
  ##

  # Get NMA z-scores
  z_nma <- nmares(model, random)
  z_nma$Node <- row.names(z_nma) <- gsub(" ", "", z_nma$Node)

  if (z_value) {
    z_nma <- z_nma[, c(1, dim(z_nma)[2])] # z_values
  } else {
    z_nma <- z_nma[, c(1:2)] # TE estimates
  }


  # Find the components of the network
  comp_network <- unique(unlist(strsplit(z_nma$Node, split = paste0("[", sep, "]"), perl = TRUE)))

  if (length(comp_network) == length(z_nma$Node)) {
    stop("No additive treatments are included in the NMA model", call. = FALSE)
  }

  ##
  # Writing nodes as a combination of component's dummy variables
  ##

  dummy <- dummies(z_nma, comp_network, sep)
  dummy <- dummy[, -c(1, 2)]

  # Check if the combination exist
  if (!is.null(combination)) {
    combination <- gsub(" ", "", combination)
    check.combinations(combination, comp_network, sep)
    combination_components <- strsplit(combination, split = paste("[", sep, "]", sep = ""), perl = TRUE)[[1]]
  } else {
    combination_components <- NULL
  }

  # Number of components for each node
  z_nma$n_comp <- apply(dummy, 1, sum)

  ##
  # Find the set of nodes that differ by one component
  ##

  if (length(combination_components) > 1) {
    nodes_elements <- strsplit(z_nma$Node, split = paste("[", sep, "]", sep = ""), perl = TRUE)
  } else {
    nodes_elements <- NULL
  }

  pos <- differ.by.one(M = dummy, combination = combination_components, nodes_elements = nodes_elements)

  ##
  # Construct plot data
  ##

  data <- as.data.frame(cbind(z_nma$Node[pos$pos1], z_nma$Node[pos$pos2]))

  # Add Node's z scores
  data <- merge(data, z_nma, by.x = c("V1"), by.y = c("Node"), all.x = TRUE)
  colnames(data)[c(3, 4)] <- c("z_V1", "n_comp_V1")
  data <- merge(data, z_nma, by.x = c("V2"), by.y = c("Node"), all.x = TRUE)
  colnames(data)[c(5, 6)] <- c("z_V2", "n_comp_V2")

  data_plot <- data
  t <- which(!data$n_comp_V1 > data$n_comp_V2)

  if (length(t) > 0) {
    data_plot[t, "V1"] <- data$V2[t]
    data_plot[t, "z_V1"] <- data$z_V2[t]
    data_plot[t, "n_comp_V1"] <- data$n_comp_V2[t]
    data_plot[t, "V2"] <- data$V1[t]
    data_plot[t, "z_V2"] <- data$z_V1[t]
    data_plot[t, "n_comp_V2"] <- data$n_comp_V1[t]
  }

  # Network comparisons
  data_plot$comparison <- paste(data_plot$V1, data_plot$V2, sep = "vs")

  if (sum(is.nan(data_plot$z_V1)) > 0 | sum(is.nan(data_plot$z_V2)) > 0) {
    not_a_number <- c(which(is.nan(data_plot$z_V1) == TRUE), which(is.nan(data_plot$z_V2) == TRUE))
    excluded <- paste(data_plot$V2[not_a_number], data_plot$V1[not_a_number], sep = " and ", collapse = " , ")

    if (length(not_a_number) > 1) {
      dot <- "Dots"
      was_were <- "were"
    } else {
      dot <- "Dot"
      was_were <- "was"
    }
    ref <- model$reference.group # reference treatment


    if (dim(data_plot)[1] - length(not_a_number) == 0) {
      stop(paste(dot, "between", excluded, was_were, "excluded since the z-value for the", ref, "vs", ref, "is NaN"), call. = FALSE)
    } else {
      warning(paste(dot, "between", excluded, was_were, "excluded since the z-value for the", ref, "vs", ref, "is NaN"), call. = FALSE)
      data_plot <- data_plot[-not_a_number, ]
    }
  }

  # Limits of the plot
  lim <- data.frame(
    y = c(max(data_plot$z_V1, data_plot$z_V2), min(data_plot$z_V1, data_plot$z_V2)),
    x = c(max(data_plot$z_V1, data_plot$z_V2), min(data_plot$z_V1, data_plot$z_V2))
  )

  ##
  # Plot
  ##

  p <- ggplot2::ggplot(
    data = NULL
  ) +
    ggplot2::geom_point(ggplot2::aes(
      x = data_plot$z_V1,
      y = data_plot$z_V2
    )) +
    ggplot2::geom_line(ggplot2::aes(
      y = lim$y,
      x = lim$x
    )) +
    ggplot2::labs(
      x = paste("Nodes including", combination),
      y = paste("Nodes not including", combination)
    )

  if (histogram) {
    p <- ggExtra::ggMarginal(p, type = "histogram", fill = histogram.color)
  }

  p
}
