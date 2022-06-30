#' Components Density Plot
#'
#' @description
#' The function creates density plots in order to explore the efficacy of the components.
#'
#'
#' @details
#' If the length of the argument \code{combination} is 1, the function creates two density plots. The first is produced based on the
#' interventions that include the component combination of interest (which is specified by the argument \code{combination}),
#' while the second on the interventions that do not include the underlying component combination.
#'
#' If the argument \code{combination} includes more than one elements, the number of densities is equal with the length of
#' the argument \code{combination}, and each density is based on the interventions that include the relative component combination.
#' For example, if \code{combination = c("A + B", "B + C", "A")} the function will produce 3 density plots that are based on
#' the interventions that includes components \code{"A"} and \code{"B"}, the interventions that include components \code{"B"} and \code{"C"} and
#' interventions that includes component \code{"A"}, respectively.
#'
#' The function by default uses the intervention's z-values \code{z_value = TRUE} obtained from the random-effects network
#' meta-analysis (NMA) model (\code{random = TRUE}). It could be also adjusted to use the intervention's effect estimates
#' instead of the z-values, by setting \code{z_value = FALSE}.
#'
#' @note
#' The efficacy of the components could be explored via violins plots instead of density plots, by setting \code{violin = TRUE}.
#' Also, in the case of dichotomous outcomes, the log-scale is used.
#'
#'
#' @param model An object of class \code{\link[netmeta]{netmeta}}.
#' @param sep A single character that defines the separator between interventions components.
#' @param combination A character vector that contains the component combinations of interest.
#' @param violin \code{logical}. If \code{TRUE} the density is visualized via violins instead of density plots.
#' @param random \code{logical}. If \code{TRUE} the random-effects NMA model is used, instead of the fixed-effect NMA model.
#' @param z_value \code{logical}. If \code{TRUE} z-values are used, instead intervention effects.
#'
#'
#' @return An object of class \code{ggplot}.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes `%+%` geom_density theme_classic xlab ylab xlim labs scale_fill_discrete
#' @importFrom stats density
#' @importFrom plyr mapvalues
#'
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
#' denscomp(model = net, combination = "C")
denscomp <- function(model, sep = "+", combination, violin = FALSE, random = TRUE, z_value = TRUE) {

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
  } else if (class(combination) != "character") {
    stop("The class of combination is not character", call. = FALSE)
  } else if (class(violin) != "logical") {
    stop("The class of violin is not logical", call. = FALSE)
  } else if (length(violin) > 1) {
    stop("The length of violin must be one", call. = FALSE)
  } else if (class(random) != "logical") {
    stop("The class of random is not logical", call. = FALSE)
  } else if (length(random) > 1) {
    stop("The length of random must be one", call. = FALSE)
  } else if (class(z_value) != "logical") {
    stop("The class of z_value is not logical", call. = FALSE)
  } else if (length(z_value) > 1) {
    stop("The length of z_value must be one", call. = FALSE)
  }

  combination <- gsub(" ", "", combination)
  if (length(combination) > 1) {
    combination <- unique.combinations(combination, sep)
  }
  ##
  # Find the components of the network
  ##

  ifelse(random, type <- "random", type <- "fixed")

  if (z_value) {
    sm <- "statistic"
    xlabel <- "z-value"
  } else {
    sm <- "TE"
    xlabel <- "Intervention Effect"
  }

  ref <- as.character(model$reference.group) # Reference category

  # Get networks treatment effects
  nma_sm <- data.frame("SM" = model[[paste(sm, type, sep = ".")]][, ref])
  nma_sm$Node <- rownames(nma_sm) <- gsub(" ", "", as.character(rownames(nma_sm)))

  if (sum(is.na(as.numeric(nma_sm$SM))) != 0) {
    non_num <- which(is.na(as.numeric(nma_sm$SM)))

    ref_exc <- which(nma_sm$Node[non_num] == ref)

    if (length(ref_exc) != 0) { # reference included

      if (length(non_num) > 1) {
        warning(paste0("Nodes ", paste0(nma_sm$Node[non_num[-ref_exc]], collapse = ", "), " were excluded since the ", tolower(xlabel), " could not be determined for these nodes"))
      }
    } else { # reference not included

      if (length(non_num) == 1) {
        warning(paste0("Node ", nma_sm$Node[non_num], " was excluded since the ", tolower(xlabel), " could not be determined"))
      } else {
        warning(paste0("Nodes ", paste0(nma_sm$Node[non_num], collapse = ", "), " were excluded since the ", tolower(xlabel), " could not be determined for these nodes"))
      }
    }

    nma_sm <- nma_sm[-non_num, ]
  }

  # Components of the network
  comp_network <- unique(unlist(strsplit(nma_sm$Node, split = paste("[", sep, "]", sep = ""), perl = TRUE)))

  if (length(comp_network) == length(nma_sm$Node)) {
    stop("No additive treatments are included in the NMA model", call. = FALSE)
  }

  # Check if combination's components are included in network's components
  component_elements <- strsplit(combination, split = paste("[", sep, "]", sep = ""), perl = TRUE)
  included <- lapply(component_elements,
    FUN = function(x) {
      sum(x %in% comp_network) == length(x)
    }
  )

  if (sum(sapply(included, sum)) != length(included)) {
    stop(paste("Argument combination must includes network's components"), call. = FALSE)
  }

  ##
  # Write the network's nodes as a combination of components dummy variables
  ##

  dummy <- dummies(nma_sm, comp_network, sep)

  # Check if the combinations can be obtained
  combination_exist <- sapply(component_elements,
    FUN = function(x) {
      sum(apply(as.matrix(dummy[, x]), 1, sum) == length(x)) > 1 # two data-points required for the density
    }
  )

  if (sum(combination_exist) == 0) {
    stop("At least two datapoints required for the density, which were not found", call. = FALSE)
  }

  if (sum(!combination_exist) > 0) {
    if (sum(!combination_exist) == 1) {
      warning(paste(
        paste(combination[!combination_exist]),
        "is excluded since it was not included in at least two nodes"
      ),
      call. = FALSE
      )
    } else {
      warning(paste(
        paste(combination[!combination_exist], collapse = ", "),
        "are excluded since they were not included in at least two nodes"
      ),
      call. = FALSE
      )
    }
    combination <- combination[combination_exist]
  }

  ##
  # Make plot data
  ##

  if (length(combination) > 1) {
    # More than one combination

    select_comp <- NULL
    n <- NULL
    index <- NULL

    for (i in 1:length(combination)) {
      combination_components <- unlist(strsplit(combination[i], split = paste("[", sep, "]", sep = ""), perl = TRUE)[[1]])

      if (length(combination_components) > 1) {
        rows <- which(apply(dummy[, combination_components], 1, sum) == length(combination_components))
      } else {
        rows <- which(dummy[, combination_components] == 1)
      }
      ##
      select_comp_i <- dummy[rows, "SM"]
      n <- c(n, length(select_comp_i))
      index <- c(index, rep(combination[i], length(select_comp_i)))
      ##
      select_comp <- c(select_comp, select_comp_i)
    }

    plot.data <- data.frame(
      "SM" = c(select_comp),
      "combination" = rep(combination, times = n)
    )

    lab <- paste("Interventions including", combination)
  } else {
    # One component

    combination_components <- unlist(strsplit(combination, split = paste("[", sep, "]", sep = ""), perl = TRUE)[[1]])

    if (length(combination_components) > 1) {
      rows_include <- which(apply(dummy[, combination_components], 1, sum) == length(combination_components))
      rows_not_include <- which(apply(dummy[, combination_components], 1, sum) != length(combination_components))
    } else {
      rows_include <- which(dummy[, combination_components] == 1)
      rows_not_include <- which(dummy[, combination_components] == 0)
    }
    ##
    select_comp_incl <- dummy[rows_include, "SM"]
    select_comp_not_incl <- dummy[rows_not_include, "SM"]
    select_comp <- c(select_comp_incl, select_comp_not_incl)

    ##
    xl <- paste("Treatment Effect with and without", combination)
    lab <- c(
      paste("Interventions including", combination),
      paste("Interventions not including", combination)
    )

    plot.data <- data.frame(
      "SM" = c(select_comp_incl, select_comp_not_incl),
      "combination" = rep(c(lab[1], lab[2]), times = c(length(select_comp_incl), length(select_comp_not_incl)))
    )
  }

  ##
  # Plot
  ##


  if (violin) {
    p <- ggplot2::ggplot(
      data = NULL,
      ggplot2::aes(x = plot.data$combination, y = plot.data$SM)
    ) +
      ggplot2::geom_violin(trim = TRUE, fill = "lightblue") +
      ggplot2::geom_boxplot(width = 0.2, ggplot2::aes(fill = plot.data$combination)) +
      ggplot2::geom_jitter(
        shape = 16,
        position = ggplot2::position_jitter(0.01)
      ) +
      ggplot2::ylab(xlabel) +
      ggplot2::xlab("") +
      ggplot2::ylim(
        min(select_comp) - 0.5,
        max(select_comp) + 0.5
      ) +
      ggplot2::theme(legend.position = "none")
  } else {
    p <- ggplot2::ggplot(
      data = NULL,
      ggplot2::aes(
        x = plot.data$SM,
        fill = plot.data$combination
      )
    ) +
      ggplot2::geom_density(alpha = 0.7, color = NA) +
      ggplot2::xlab(xlabel) +
      ggplot2::ylab("Density") +
      ggplot2::xlim(
        min(select_comp) - 0.5,
        max(select_comp) + 0.5
      ) +
      ggplot2::scale_fill_discrete(labels = lab) +
      ggplot2::theme_classic() +
      ggplot2::labs(fill = "") +
      ggplot2::theme(legend.position = "bottom")
  }

  p
}
