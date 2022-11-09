#' Specific Component Combination violin plots
#'
#' @description
#' The function based on the network meta-analysis (NMA) estimates produces violin plots from interventions that include the
#' component combinations of interest.
#'
#' @details
#' By default the function creates a violin for each component of the network (\code{combination = NULL}). Each violin visualizes the
#' distribution of the effect estimates, obtained from the interventions that include the corresponding component.
#' Combinations of interest are specified from the argument \code{combination}. For example, if \code{combination = c("A", "A + B")},
#' two violin plots are produced. The first one is based on the interventions that contain the component "A", and the second one, based
#' on the interventions that contain both components A and B.
#'
#' By setting the argument \code{components_number = TRUE}, the behavior of intervention's effect as the number of components increased
#' is explored, by producing violins based on the number of components included in the interventions. If the number of
#' components included in a intervention ranges between 1 and 3, then 3 violins will be produced in total. The violins will be based on
#' the interventions that include one component, two components, and three components respectively.
#' The number of components could be also categorized in groups by the argument \code{groups}. For
#' example if \code{components_number = TRUE} and \code{groups = c("1-3", 4, "5+")}, 3 violins will be created. One for the
#' interventions that contain less than 3 components, one for the interventions that contain 4 components and one for those
#' that contain more than 5 components.
#'
#' The function by default uses the NMA relative effects, but it could be adjusted to use intervention's z-scores by setting \code{z_value = TRUE}.
#' In the case where the NMA relative effects, the size of dots reflects the precision of the estimates. Larger dots indicates
#' more precise NMA estimates.
#'
#' @note
#' In the case of dichotomous outcomes, the log-scale is used in axis y. Also, the function can be applied
#' only in network meta-analysis models that contain multi-component interventions.
#'
#'
#' @param model An object of class \code{\link[netmeta]{netmeta}}.
#' @param sep A single character that defines the separator between interventions components.
#' @param combination A character vector that specifies the component combinations of interest.
#' @param components_number \code{logical}. If \code{TRUE} the violins are created based on the number of components included in the interventions.
#' @param groups A character vector that contains the clusters of the number of components. Elements of the vector must be integer numbers (e.g. 5 or "5"), or range values (e.g. "3-4" ), or in the "xx+" format (e.g "5+").
#' @param random \code{logical}. If \code{TRUE} the random-effects NMA model is used instead of the fixed-effect NMA model.
#' @param z_value \code{logical}. If \code{TRUE} z-values are used instead of interventions effects.
#' @param prop_size \code{logical}. If \code{TRUE} in the case where \code{z_value == FALSE}, the size of the dots is proportional to the precision of the estimates.
#' @param fill_violin fill color of the violin. See \code{\link[ggplot2]{geom_violin}} for more details.
#' @param color_violin color of the violin. See \code{\link[ggplot2]{geom_violin}} for more details.
#' @param adj_violin adjustment of the violin. See \code{\link[ggplot2]{geom_violin}} for more details.
#' @param width_violin width of the violin. See \code{\link[ggplot2]{geom_violin}} for more details.
#' @param boxplot \code{logical}. If \code{TRUE} boxplots are plotted.
#' @param width_boxplot width of the boxplot. See \code{\link[ggplot2]{geom_boxplot}} for more details.
#' @param errorbar_type boxplot's line type. See \code{\link[ggplot2]{stat_boxplot}} for more details.
#' @param dots \code{logical}. If \code{TRUE} data points are plotted.
#' @param jitter_shape jitter shape. See \code{\link[ggplot2]{geom_jitter}} for more details.
#' @param jitter_position jitter position. See \code{\link[ggplot2]{geom_jitter}} for more details.
#' @param values \code{logical}. If \code{TRUE} median value of each violin is printed.
#'
#' @importFrom stats median
#' @importFrom ggplot2 ggplot aes `%+%` geom_violin geom_boxplot geom_jitter position_jitter geom_text stat_boxplot labs scale_x_discrete scale_y_log10 guides
#' @importFrom Hmisc all.is.numeric
#'
#' @return An object of class \code{ggplot}.
#' @export
#'
#' @examples
#' data(nmaMACE)
#' specc(model = nmaMACE, combination = c("B", "C", "B + C"))
#'
specc <- function(model, sep = "+", combination = NULL, components_number = FALSE, groups = NULL, random = TRUE, z_value = FALSE,
                  prop_size = TRUE, fill_violin = "lightblue", color_violin = "lightblue", adj_violin = 1, width_violin = 1,
                  boxplot = TRUE, width_boxplot = 0.5, errorbar_type = 5, dots = TRUE,
                  jitter_shape = 16, jitter_position = 0.01, values = TRUE) {

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
  } else if (!is.null(combination)) {
    if (inherits(combination, "character") == FALSE) {
      stop("The class of combination is not character", call. = FALSE)
    }

    combination <- unique.combinations(combination, sep) # unique combinations
  } else if (inherits(components_number, "logical") == FALSE) {
    stop("The class of components_number is not logical", call. = FALSE)
  } else if (length(components_number) > 1) {
    stop("The length of components_number must be one", call. = FALSE)
  } else if (components_number == TRUE & is.null(groups) == FALSE) {
    if (inherits(groups, "character") == FALSE) {
      stop("The class of groups is not character", call. = FALSE)
    }

    if (sum(table(groups) > 1) > 0) {
      stop("Argument groups must contain different elements", call. = FALSE)
    }

    if (check.groups(groups) == FALSE) {
      stop("The elements of groups must be either integer (string) values, or either a range (e.g 1-2), or greiter than an integer number (e.g 5+)", call. = FALSE)
    }
  } else if (inherits(random, "logical") == FALSE) {
    stop("The class of random is not logical", call. = FALSE)
  } else if (length(random) > 1) {
    stop("The length of random must be one", call. = FALSE)
  } else if (inherits(z_value, "logical") == FALSE) {
    stop("The class of z_value is not logical", call. = FALSE)
  } else if (length(z_value) > 1) {
    stop("The length of z_value must be one", call. = FALSE)
  } else if (inherits(boxplot, "logical") == FALSE) {
    stop("The class of boxplot is not logical", call. = FALSE)
  } else if (length(boxplot) > 1) {
    stop("The length of boxplot must be one", call. = FALSE)
  } else if (inherits(dots, "logical") == FALSE) {
    stop("The class of dots is not logical", call. = FALSE)
  } else if (length(dots) > 1) {
    stop("The length of dots must be one", call. = FALSE)
  } else if (inherits(values, "logical") == FALSE) {
    stop("The class of values is not logical", call. = FALSE)
  } else if (length(values) > 1) {
    stop("The length of values must be one", call. = FALSE)
  }

  ##
  # NMA estimates and characteristics
  ##

  # Get NMA estimates
  nma_est <- nmares(model, random)
  nma_est$Node <- row.names(nma_est) <- gsub(" ", "", nma_est$Node)


  # Components of the network
  comp_network <- strsplit(nma_est$Node, split = paste("[", sep, "]", sep = ""), perl = TRUE)

  if (sum(sapply(comp_network, FUN = function(x) {
    length(x) > 1
  })) == 0) {
    stop("No additive treatments are included in the NMA model", call. = FALSE)
  } else {
    comp_network <- unique(unlist(comp_network))
  }

  ##
  # Write the network's nodes as a combination of components' dummy variables
  ##

  dummy <- dummies(nma_est, comp_network, sep)
  ##
  data_plot <- NULL # data for the plot

  if (components_number) {

    ##
    # Data for the number of components
    ##

    dummy$n_comp <- apply(dummy[, (which(names(dummy) == "z_stat") + 1):(dim(dummy)[2])], 1, sum)

    if (!is.null(groups)) {

      # Exclude the groups that cannot be calculated
      exclude_groups <- NULL

      num_elements <- sapply(groups, Hmisc::all.is.numeric)
      exclude_num <- !groups[num_elements] %in% dummy$n_comp
      if (sum(exclude_num) > 0) {
        exclude_groups <- groups[num_elements][exclude_num]
        groups <- groups[-which(groups %in% exclude_groups)]
      }

      if (length(groups) > 0) {
        ranges <- strsplit(groups, split = "-")
        range_elements <- sapply(ranges,
          FUN = function(x) {
            Hmisc::all.is.numeric(x) & length(x) == 2
          }
        )

        if (sum(range_elements) > 0) {
          exclude_ranges <- sapply(ranges[range_elements],
            FUN = function(x) {
              inc <- as.numeric(x[1]):as.numeric(x[2]) %in% dummy$n_comp
              sum(inc) == 0
            }
          )
          if (sum(exclude_ranges) > 0) {
            exclude_ranges <- groups[range_elements][exclude_ranges]
            exclude_groups <- c(exclude_groups, exclude_ranges)
            groups <- groups[-which(groups %in% exclude_groups)]
          }
        }
      }

      if (length(groups) > 0) {
        plus <- strsplit(groups, split = "+")
        plus_elements <- sapply(plus,
          FUN = function(p) {
            if (length(p) != 2) {
              FALSE
            } else if (Hmisc::all.is.numeric(p[1]) & p[2] == "+") {
              TRUE
            }
          }
        )
        if (sum(plus_elements) > 0) {
          exclude_plus <- sapply(plus[plus_elements],
            FUN = function(p) {
              p[1] > max(dummy$n_comp)
            }
          )
          if (sum(exclude_plus) > 0) {
            exclude_plus <- groups[plus_elements][exclude_plus]
            exclude_groups <- c(exclude_groups, exclude_plus)
            groups <- groups[-which(groups %in% exclude_groups)]
          }
        }
      }


      if (length(groups) == 0) {
        stop("Argument groups contains classes that cannot obtained", call. = FALSE)
      }

      if (!is.null(exclude_groups)) {
        warning(paste(paste(exclude_groups, collapse = ", "), ifelse(length(exclude_groups) == 1, paste("was"), paste("were")),
          "excluded from the groups arguments since it cannot be obtained from the network geometry",
          collapse = ""
        ),
        call. = FALSE
        )
      }

      # Data for each component number group

      for (i in groups) {
        if (grepl("-", i)) {
          group_range <- strsplit(i, split = "-")[[1]]
          data_j <- dummy[which(dummy$n_comp <= max(group_range) & dummy$n_comp >= min(group_range)), ]
        } else if ("+" %in% strsplit(i, "+")[[1]]) {
          data_j <- dummy[which(dummy$n_comp >= as.numeric(strsplit(i, "+")[[1]][1])), ]
        } else {
          data_j <- dummy[which(dummy$n_comp == as.numeric(i)), ]
        }

        data_j$Combination <- i
        data_plot <- rbind(data_plot, data_j)
      }
    } else {
      dummy$Combination <- dummy$n_comp
      data_plot <- dummy
    }

    # Exclude the reference rows
    data_plot <- data_plot[-which(data_plot$Node == model$reference.group), ]
    axis_x <- "Number of components"
  } else {

    ##
    # Data for the combination of components
    ##

    dummy$Combination <- NA
    axis_x <- "Components"

    if (!is.null(combination)) {
      combination <- gsub(" ", "", combination)

      check.combinations(combination, comp_network, sep)
      # Specific component combinations
      for (i in 1:length(combination)) {
        dummy_comb_i <- strsplit(combination[i], split = paste0("[", sep, "]"), perl = TRUE)[[1]]
        columns <- which(names(dummy) %in% dummy_comb_i) # corresponding columns

        # Find the rows that have those components
        data_j <- dummy
        ##
        for (k in 1:length(columns)) {
          data_j <- data_j[which(data_j[, columns[k]] == 1), ]
        }

        if (dim(data_j)[1] > 0) {
          data_j$Combination <- combination[i]
          data_plot <- rbind(data_plot, data_j)
        } else {
          warning(paste("Combination", combination[i], "cannot obtained due to network geometry"), call. = FALSE)
        }
      }

      if (is.null(data_plot)) {
        stop("Argument combinations containes component combinations that cannot be obtained", call. = FALSE)
      }
    } else {

      # Network's components

      for (i in comp_network) {
        # Find the rows that have those components
        data_j <- dummy[which(dummy[, i] == 1), ]
        data_j$Combination <- i
        data_plot <- rbind(data_plot, data_j)
      }
      # Exclude the reference category
      ref_exc <- which(data_plot$Combination == model$reference.group)
      if (length(ref_exc) > 0) {
        data_plot <- data_plot[-ref_exc, ]
      }
    }
  }

  # Effect measure to be used
  if (z_value == TRUE) {
    mes <- "z_stat"
    yax <- "Standardized effects"

    # exclude NaN or NA
    if (sum(is.nan(data_plot$z_stat)) > 0 | sum(is.na(data_plot$z_stat)) > 0) {
      ex_z <- which(is.nan(data_plot$z_stat) == TRUE | is.na(data_plot$z_stat) == TRUE)
      data_plot <- data_plot[-ex_z, ]
    }

    data_plot$size <- 1
    a <- 1
  } else {
    mes <- "TE"
    a <- 0.3

    if (model$sm %in% c("OR", "RR")) { # dichotomous outcomes
      data_plot[, mes] <- exp(data_plot[, mes])
    }
    yax <- "Treatment effects"

    # dots size
    if (prop_size == TRUE) {
      data_plot$size <- (1 / data_plot$seTE - 0.1) / (10 - 0.1) # scale 0.1 - 10

      over1 <- which(data_plot$size > 1)
      if (length(over1) > 0) {
        data_plot$size[over1] <- 1
      }

      zeros <- which(round(data_plot$size, 1) == 0)
      if (length(zeros) > 0) {
        data_plot$size[zeros] <- 0.2
      }

      data_plot$size <- 8 * data_plot$size
    } else {
      data_plot$size <- 1
      a <- 1
    }
  }

  ##
  # Plot
  ##

  lees_2 <- table(data_plot$Combination) < 2
  r <- 1:dim(data_plot)[1]
  if (sum(lees_2) > 0) {
    warning("Violin plot requires at least 2 data point", call. = FALSE)
    no_violin <- labels(lees_2)[[1]][which(lees_2 == TRUE)]
    r <- which(!data_plot$Combination %in% no_violin)
  }

  data_plot$Combination <- as.character(data_plot$Combination)

  p <- ggplot2::ggplot(
    data = NULL,
    ggplot2::aes(
      x = data_plot$Combination,
      y = data_plot[, mes]
    )
  ) +
    ggplot2::geom_violin(
      ggplot2::aes(
        x = data_plot$Combination[r],
        y = data_plot[r, mes]
      ),
      trim = TRUE,
      fill = fill_violin,
      color = color_violin,
      adjust = adj_violin,
      width = width_violin
    ) +
    ggplot2::labs(
      y = yax,
      fill = ""
    ) +
    ggplot2::scale_x_discrete(name = axis_x) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title = ggplot2::element_text(size = 12)
    ) +
    ggplot2::guides(fill = guide_legend(nrow = 1))

  if (boxplot) {
    p <- p + ggplot2::geom_boxplot(
      width = width_boxplot,
      outlier.shape = NA,
      ggplot2::aes(fill = data_plot$Combination)
    ) +
      ggplot2::stat_boxplot(
        geom = "errorbar",
        linetype = errorbar_type
      )
  }

  if (dots) {
    p <- p + ggplot2::geom_jitter(
      size = data_plot$size, alpha = a,
      shape = jitter_shape,
      position = ggplot2::position_jitter(jitter_position)
    )
  }

  if (values) {
    medz <- tapply(data_plot[, mes], data_plot$Combination, stats::median)
    p_meds <- data.frame(combination = names(medz), med = round(medz, 2))

    p <- p + ggplot2::geom_text(
      data = NULL,
      ggplot2::aes(
        x = p_meds$combination,
        y = round(p_meds$med, 2),
        label = format(p_meds$med, n.small = 2)
      ),
      color = "red",
      size = 5,
      hjust = 2.2,
      vjust = -0.6
    )
  }

  if (model$sm %in% c("OR", "RR") & z_value == FALSE) {
    p <- p + ggplot2::scale_y_log10()
  }

  p
}
