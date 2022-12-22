#' Components Heat Plot
#'
#' @description
#' The function creates a heat plot based on the two-by-two component combinations, obtained from the
#' network meta-analysis (NMA) model.
#'
#' @details
#' Diagonal elements refer to components, while off-diagonal to components' combinations. Each element summarizes by default
#' the NMA relative effects (\code{z_value = FALSE}) of the interventions that includes the corresponding
#' component combination. Combinations that were not observed in the NMA model, are denoted by the letter "X".
#' Frequency of component combinations observed in the NMA is printed by default (\code{freq = TRUE}). As a summary measure,
#' the median is used by default (\code{median = TRUE}). The magnitude of each relative effect is reflected by the color's intensity.
#' Estimates close to zero are denoted by white color, and indicates a small magnitude of the corresponding component combination, while
#' deep green and red colors indicate a large magnitude of the corresponding component combination.
#' Outcomes nature (beneficial or harmful) is defined in the \code{netmeta} model.
#'
#' The function can be also adjusted to include z-scores by setting the argument \code{z_value = TRUE}.
#' Z-scores quantify the strength of statistical evidence. Thus, dark green (or red) indicates strong statistical evidence that
#' the corresponding component (or combination of components) performs better (or worse) than the reference intervention.
#'
#' @note
#' In the case where the NMA relative effects are used, the uncertainty of the NMA estimates are reflected by the size of the grey boxes.
#' The bigger the box, the more precise the estimate.
#'
#' By setting \code{median = FALSE}, the mean is used instead of the median as a summary measure.
#'
#' The function can be applied only in network meta-analysis models that contain multi-component interventions.
#'
#'
#' @param model An object of class \code{\link[netmeta]{netmeta}}.
#' @param sep A single character that defines the separator between interventions components.
#' @param median \code{logical}. If \code{TRUE} the median is used instead of the mean as a summary measure.
#' @param random \code{logical}. If \code{TRUE} the random-effects NMA model is used instead of the fixed-effect NMA model.
#' @param z_value \code{logical}. If \code{TRUE} z-values are used instead of interventions effects.
#' @param freq \code{logical}. If \code{TRUE} the frequency of component combinations are printed.
#' @param legend_name A single character that specifies the title of the legend.
#'
#' @import netmeta
#' @importFrom  reshape2 melt
#' @importFrom  MASS mvrnorm
#' @importFrom  stats median quantile sd
#' @importFrom  ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2 theme_minimal theme theme element_blank element_text
#' @importFrom  ggnewscale new_scale_fill
#'
#' @return An object of class \code{ggplot}.
#' @export
#'
#' @examples
#' data(nmaMACE)
#' heatcomp(model = nmaMACE)
#'
heatcomp <-
  function(model, sep = "+", median = TRUE, random = TRUE, z_value = FALSE, freq = TRUE, legend_name = NULL) {

    ##
    # Check arguments
    ##

    if (inherits(model, "netmeta") == FALSE) {
      stop("The class of model is not of netmeta", call. = FALSE)
    } else if (model$reference.group == "") {
      stop("The netmeta model must have a reference group", call. = FALSE)
    } else if (inherits(sep, "character") == FALSE) {
      stop("The class of sep is not character", call. = FALSE)
    } else if (length(sep) > 1) {
      stop("The length of sep must be one", call. = FALSE)
    } else if (sep == "") {
      stop("Argument sep must be diffent than ''", call. = FALSE)
    } else if (inherits(median, "logical") == FALSE) {
      stop("The class of median is not logical", call. = FALSE)
    } else if (length(median) > 1) {
      stop("The length of median must be one", call. = FALSE)
    } else if (inherits(random, "logical") == FALSE) {
      stop("The class of random is not logical", call. = FALSE)
    } else if (length(random) > 1) {
      stop("The length of random must be one", call. = FALSE)
    } else if (inherits(z_value, "logical") == FALSE) {
      stop("The class of z_value is not logical", call. = FALSE)
    } else if (length(z_value) > 1) {
      stop("The length of z_value must be one", call. = FALSE)
    } else if (inherits(freq, "logical") == FALSE) {
      stop("The class of freq is not logical", call. = FALSE)
    } else if (length(freq) > 1) {
      stop("The length of freq must be one", call. = FALSE)
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
    components <- strsplit(nodes, split = paste("[", sep, "]", sep = ""), perl = TRUE)


    if (sum(sapply(components, FUN = function(x) {
      length(x) > 1
    })) == 0) {
      stop("No additive treatments are included in the NMA model", call. = FALSE)
    } else {
      components <- sort(unique(unlist(components)))
    }

    if (model$sm %in% c("OR", "RR") & z_value == FALSE) {
      exponen <- TRUE
    } else {
      exponen <- FALSE
    }

    Heatdata <- phd(components, mean_eff, median, sep, freq, exponen)
    melted_data <- Heatdata$data
    txt <- Heatdata$text

    # Data are in upper triangle form. So exclude NAs in order to not get warnings in the ggplot
    keep <- which(is.nan(melted_data$value) | !is.na(melted_data$value))
    melted_data <- melted_data[keep, ]
    txt <- txt[which(!is.na(txt))]

    # calculate se.TE for each combination
    if (z_value == FALSE) {
      se.TE <- summodel[[type]][["seTE"]][-which(colnames(summodel[[type]][["seTE"]]) == ref), ref]
      names(se.TE) <- gsub(" ", "", labels(se.TE))
      weights <- phd(components, 1 / se.TE, median, sep, freq, exponen = FALSE)$data
      weights <- weights[keep, ]
    }


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

    small_val <- model$small.values

    if (small_val == "bad") { # beneficial
      low_col <- "red"
      high_col <- "green"
    } else {
      low_col <- "green"
      high_col <- "red"
    }

    if (model$sm %in% c("OR", "RR") & z_value == FALSE) {
      mid <- 1
      lim <- c(0, max(melted_data$value, na.rm = TRUE))
    } else {
      mid <- 0
      lim <- c(min(melted_data$value, na.rm = TRUE), max(melted_data$value, na.rm = TRUE))
    }

    p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = melted_data$Var1, y = melted_data$Var2, fill = melted_data$value)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = txt)) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::scale_fill_gradient2(
        low = low_col, high = high_col, mid = "white",
        midpoint = mid, na.value = "white",
        limit = lim,
        space = "Lab",
        name = titleg
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 12, face = "bold", color = "black")
      )

    if (z_value == FALSE) {
      weights$size <- (weights$value - 0.1) / (10 - 0.1) # scale 0.1 - 10

      over1 <- which(weights$size >= 1)
      if (length(over1) > 0) {
        weights$size[over1] <- 0.98
      }

      zeros <- which(round(weights$size, 1) == 0)
      if (length(zeros) > 0) {
        weights$size[zeros] <- 0.1
      }

      w_Nas <- which(is.na(weights$size) == T | is.nan(weights$size) == T)
      if(length(w_Nas) > 0){
        weights[w_Nas, c("value", "size")] <- 0
      }

      p <- p + ggnewscale::new_scale_fill() +
        ggplot2::geom_tile(
          data = NULL, ggplot2::aes(x = weights$Var1, y = weights$Var2, height = weights$size, width = weights$size),
          alpha = 0.1
        )
    }

    p
  }
