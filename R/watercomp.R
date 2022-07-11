#' Waterfall plot
#'
#' @description
#' The function produces a waterfall plot based on the z-values from the interventions that differ
#' by one specific component combination.
#'
#' @details
#' The function based on the intervention's z-values obtained from the network meta-analysis (NMA) model
#' visualizes all the observed interventions that differ by one specific component
#' combination, in order to explore if the one extra component combination from every comparison
#' has a positive or negative impact. Bars above or below of the \eqn{y = 0} line,
#' indicates that the inclusion of the extra specific component combination has an impact on the
#' intervention. The direction of the impact (positive or negative), depends on the outcomes’ nature
#' (beneficial or harmful).
#'
#' The combination of interest is defined from the argument \code{combination}. By default the
#' function visualizes the interventions that differ by one component (\code{combination = NULL}).
#' If for example \code{combination = "A+B"}, the function plots the interventions that differ
#' by "A+B".
#'
#' @note
#' In the case of dichotomous outcomes, the log-scale is used. Also, the function can be applied
#' only in network meta-analysis models that contain multi-component interventions.
#'
#' @param model An object of class \code{\link[netmeta]{netmeta}}.
#' @param sep  A single character that defines the separator between interventions components.
#' @param combination A single character that specifies the component combination of interest.
#' @param random \code{logical}. If \code{TRUE} z-values are obtained from the random-effects NMA model instead of the fixed-effect NMA model.
#'
#' @return An object of class \code{ggplot}.
#' @export
#'
#' @importFrom ggplot2 ggplot aes `%+%` geom_bar position_dodge labs theme_classic
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
#' watercomp(net)
watercomp <- function(model, sep = "+", combination = NULL, random = TRUE) {

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
  }

  ##
  # NMA estimates and characteristics
  ##

  # Get NMA estimates
  nma_est <- nmares(model, random)
  nma_est <- nma_est[, -c(3:dim(nma_est)[2])]
  nma_est$Node <- row.names(nma_est) <- gsub(" ", "", nma_est$Node)

  # Reference category
  ref <- as.character(model$reference.group)

  # Components of the network
  comp_network <- unique(unlist(strsplit(nma_est$Node, split = paste("[", sep, "]", sep = ""), perl = TRUE)))

  if (length(comp_network) == length(nma_est$Node)) {
    stop("No additive treatments are included in the NMA model", call. = FALSE)
  }

  ##
  # Writing nodes as a combination of component's dummy variables
  ##

  dummy <- dummies(nma_est, comp_network, sep)
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
  nma_est$n_comp <- apply(dummy, 1, sum)

  ##
  # Find the set of nodes that differ one component
  ##

  if (length(combination_components) > 1) {
    nodes_elements <- strsplit(nma_est$Node, split = paste("[", sep, "]", sep = ""), perl = TRUE)
  } else {
    nodes_elements <- NULL
  }

  pos <- differ.by.one(M = dummy, combination = combination_components, nodes_elements = nodes_elements)

  ##
  # Make Plot data
  ##

  data_plot <- as.data.frame(cbind(nma_est$Node[pos$pos1], nma_est$Node[pos$pos2]))

  # Add node's summary measures
  data_plot <- merge(data_plot, nma_est, by.x = c("V1"), by.y = c("Node"), all.x = TRUE)
  colnames(data_plot)[c(3, 4)] <- c("TE_V1", "n_comp_V1")
  data_plot <- merge(data_plot, nma_est, by.x = c("V2"), by.y = c("Node"), all.x = TRUE)
  colnames(data_plot)[c(5, 6)] <- c("TE_V2", "n_comp_V2")

  # Calculate differences based on the number of components
  data_plot$diff <- ifelse(data_plot$n_comp_V1 > data_plot$n_comp_V2,
    data_plot$TE_V1 - data_plot$TE_V2,
    data_plot$TE_V2 - data_plot$TE_V1
  )

  ##
  # Calculate variances
  ##

  ifelse(random, covmat <- model$Cov.random, covmat <- model$Cov.fixed) # Covariance matrix
  colnames(covmat) <- rownames(covmat) <- gsub(" ", "", colnames(covmat))
  v <- NULL

  for (i in 1:dim(data_plot)[1]) {

    # Calculate Covariance
    a <- which(rownames(covmat) == paste(data_plot$V1[i], ref, sep = ":"))
    b <- which(colnames(covmat) == paste(data_plot$V2[i], ref, sep = ":"))
    ##
    if (length(a) == 0 | length(b) == 0) { # ref vs ref

      r <- which(row.names(covmat) %in% c(
        paste(data_plot$V1[i], data_plot$V2[i], sep = ":"),
        paste(data_plot$V2[i], data_plot$V1[i], sep = ":")
      ))
      v[i] <- covmat[r, r]
    } else {
      covariance <- covmat[a, b]
      var_a <- which(colnames(covmat) == paste(data_plot$V1[i], ref, sep = ":"))
      var_b <- which(colnames(covmat) == paste(data_plot$V2[i], ref, sep = ":"))
      ##
      v[i] <- covmat[var_a, var_a] + covmat[var_b, var_b] - 2 * covariance
    }
  }

  # Calculate Z-values
  data_plot$z <- data_plot$diff / v
  data_plot$compar <- paste(data_plot$V2, data_plot$V1, sep = " vs ")

  ##
  #  Plot
  ##

  p <- ggplot2::ggplot(
    data = NULL,
    ggplot2::aes(
      x = data_plot$compar,
      y = data_plot$z
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      width = 0.7,
      position = ggplot2::position_dodge(width = 0.4),
      color = "red"
    ) +
    ggplot2::labs(
      x = "Comparison",
      y = "Standardized change"
    ) +
    ggplot2::theme_classic()

  p
}
