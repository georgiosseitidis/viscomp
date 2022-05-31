#' Components Heat Plot
#'
#' @description
#' The function creates a heat plot based on the two-by-two component combinations, obtained from the
#' network meta-analysis (NMA) model.
#'
#' @details
#' Diagonal elements refer to components, while off-diagonal to components' combinations. Each element summarizes
#' the z-values (if \code{z_value = TRUE}) of the interventions that includes the corresponding
#' component combination. Combinations that were not observed in the NMA model, are
#' denoted by the letter "X". As a summary measure, the median is used by default (\code{median = TRUE}).
#' The magnitude of each z-score is reflected by the color's intensity. Estimates close to zero
#' are denoted by white color, while above and below zero are denoted by warm colors (such as red)
#' and cold colors (such as blue) respectively.
#'
#' Intervention effects can be used instead of z-values by setting \code{z_value = FALSE}.
#' Also, by setting \code{median = FALSE}, the mean is used instead of the median as a summary measure.
#'
#'
#' @param model An object of class \code{\link[netmeta]{netmeta}}.
#' @param sep A single character that defines the separator between interventions components.
#' @param median \code{logical}. If \code{TRUE} the median is used instead of the mean as a summary measure.
#' @param random \code{logical}. If \code{TRUE} the random-effects NMA model is used instead of the fixed-effect NMA model.
#' @param z_value \code{logical}. If \code{TRUE} z-values are used instead of interventions effects.
#' @param legend_name A single character that specifies the title of the legend.
#'
#'
#' @importFrom  reshape2 melt
#' @importFrom  MASS mvrnorm
#' @importFrom  stats median quantile sd
#' @importFrom  ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2 theme_minimal theme theme element_blank
#'
#'
#' @return An object of class \code{ggplot}.
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
#' heatcomp(model = net)
heatcomp <-
  function(model, sep = "+", median = TRUE, random = TRUE, z_value = TRUE, legend_name = NULL) {

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
    } else if (class(median) != "logical") {
      stop("The class of median is not logical", call. = FALSE)
    } else if (length(median) > 1) {
      stop("The length of median must be one", call. = FALSE)
    } else if (class(random) != "logical") {
      stop("The class of random is not logical", call. = FALSE)
    } else if (length(random) > 1) {
      stop("The length of random must be one", call. = FALSE)
    } else if (class(z_value) != "logical") {
      stop("The class of z_value is not logical", call. = FALSE)
    } else if (length(z_value) > 1) {
      stop("The length of z_value must be one", call. = FALSE)
    } else if (!is.null(legend_name)) {
      if (!is.vector(legend_name)) {
        stop("legend_name should be a vector of length one", call. = FALSE)
      } else if (length(legend_name) != 1) {
        stop("legend_name should be a vector of length one", call. = FALSE)
      }
    }

    ##
    # NMA data
    ##

    summodel <- summary(model) # summary of NMA model
    ref <- model$reference.group # reference group

    # Set NMA type and measurement unit
    ifelse(random, type <- "random", type <- "fixed")
    ifelse(z_value, o <- "statistic", o <- "TE")

    # Mean effects vs reference category
    mean_eff <- summodel[[type]][[o]][-which(colnames(summodel[[type]][[o]]) == ref), ref]
    names(mean_eff) <- gsub(" ", "", labels(mean_eff))

    # Find all components of the data. Components must be separated by " + "
    nodes <- unique(c(model$treat1, model$treat2))
    nodes <- nodes[-which(nodes == ref)]
    nodes <- gsub(" ", "", nodes)

    # Components of the network
    components <- unique(unlist(strsplit(nodes, split = paste("[", sep, "]", sep = ""), perl = TRUE)))

    if (length(components) == length(nodes)) {
      stop("No additive treatments are included in the NMA model", call. = FALSE)
    }

    Heatdata <- phd(components, mean_eff, median, sep)
    melted_data <- Heatdata$data
    txt <- Heatdata$text

    # Data are in upper triangle form. So exclude NAs in order to not get warnings in the ggplot
    melted_data <- melted_data[which(is.nan(melted_data$value) | !is.na(melted_data$value)), ]
    txt <- txt[which(!is.na(txt))]

    ##
    # Heat Plot
    ##

    # Set legend title
    if (is.null(legend_name)) {
      if (z_value == TRUE) {
        if (median == TRUE) {
          titleg <- "Median Z-value"
        } else {
          titleg <- "Mean Z-value"
        }
      } else {
        if (median == TRUE) {
          titleg <- "Median TE"
        } else {
          titleg <- "Mean TE"
        }
      }
    } else {
      titleg <- as.character(legend_name)
    }

    p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = melted_data$Var1, y = melted_data$Var2, fill = melted_data$value)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = txt)) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::scale_fill_gradient2(
        low = "blue", high = "red", mid = "white",
        midpoint = 0, na.value = "white",
        limit = c(min(melted_data$value, na.rm = TRUE), max(melted_data$value, na.rm = TRUE)),
        space = "Lab",
        name = titleg
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank())

    p
  }
