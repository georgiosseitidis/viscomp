#' Components descriptive analysis
#'
#' @description
#' The function performs a descriptive analysis regarding the frequency of the components in the network meta-analysis model.
#'
#' @note
#' The function can be applied only in network meta-analysis models that contain multi-component interventions.
#'
#'
#' @param model An object of class \code{\link[netmeta]{netmeta}}.
#' @param sep A single character that defines the separator between interventions components.
#' @param heatmap \code{logical}. If \code{TRUE} a heat matrix of the component's frequency is plotted.
#' @param percentage \code{logical}. If \code{TRUE} combinations' percentages are printed as a number instead of fraction value in the \code{heatmap}.
#' @param digits A single integer value that specifies the percentages' decimal places in the \code{heatmap}.
#'
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 geom_text coord_fixed xlab ylab guides guide_legend
#'
#' @return A list containing three items
#' \item{crosstable}{A cross-table containing the frequency of the components. Each cell represents the number of arms where the corresponding component combination was observed.}
#' \item{frequency}{A \code{data.frame} that contains the component's frequency. Columns
#' \itemize{ \item{ \code{Component}} {denotes the name of each component}
#'  \item{\code{Frequency}} {denotes the number of arms where the component was observed}
#'  \item{\code{A}} {denotes the number of studies in which the corresponding component was included in all arms}
#'  \item{\code{A_percent}} {denotes the percentage of studies in which the corresponding component was included in all arms}
#'  \item{\code{B}} {denotes the number of studies in which the corresponding component was included in at least one arm}
#'  \item{\code{B_percent}} {denotes the percentage of studies in which the corresponding component was included in at least one arm}
#'  \item{\code{C}} {denotes the number of studies in which the corresponding component was not included in any arm}
#'  \item{\code{C_percent}} {denotes the percentage of studies in which the corresponding component was not included in any arm}
#'  \item{\code{A.B}} {denotes the ratio of columns \code{A} and \code{B}}.
#'   }}
#'   \item{heatmat}{An object of class \code{ggplot} that visualizes item \code{crosstable}. Diagonal elements refer to the components and in parentheses the proportion of study
#'   arms including that component is provided, while off-diagonal elements to the frequency of component’s combinations and in parentheses the proportion of study arms with both components
#'   out of those study arms that have the component in the row is provided. Also, the intensity of the color is proportional to the relative frequency of the component combination.}
#'
#' @export
#'
#' @examples
#' data(nmaMACE)
#' compdesc(model = nmaMACE)
#'
compdesc <- function(model, sep = "+", heatmap = TRUE, percentage = TRUE, digits = 2) {
  if (inherits(model, "netmeta") == FALSE) {
    stop("The class of model is not of netmeta", call. = FALSE)
  } else if (inherits(sep, "character") == FALSE) {
    stop("The class of sep is not character", call. = FALSE)
  } else if (length(sep) > 1) {
    stop("The length of sep must be one", call. = FALSE)
  } else if (sep == "") {
    stop("Argument sep must be diffent than ''", call. = FALSE)
  } else if (inherits(heatmap, "logical") == FALSE) {
    stop("The class of heatmap is not logical", call. = FALSE)
  } else if (length(heatmap) > 1) {
    stop("The length of heatmap must be one", call. = FALSE)
  } else if (inherits(percentage, "logical") == FALSE) {
    stop("The class of percentage is not logical", call. = FALSE)
  } else if (length(percentage) > 1) {
    stop("The length of percentage must be one", call. = FALSE)
  } else if (inherits(digits, c("numeric", "integer")) == FALSE) {
    stop("The class of digits is not integer", call. = FALSE)
  } else if (length(digits) > 1) {
    stop("The length of digits must be one", call. = FALSE)
  } else if (digits < 0) {
    stop("Argument digits must be a non-negative number", call. = FALSE)
  } else if (digits %% 1 != 0) {
    stop("Argument digits must be an interger number", call. = FALSE)
  }


  # Get the NMA-CNMA data
  data <- model$data[, c(".studlab", ".treat1", ".treat2")]
  names(data) <- c("studlab", "treat1", "treat2")


  # Find all components of the data
  nodes <- unique(c(model$treat1, model$treat2))
  nodes <- gsub(" ", "", nodes)

  components <- strsplit(nodes, split = paste("[", sep, "]", sep = ""), perl = TRUE)

  if (sum(sapply(components, FUN = function(x) {
    length(x) > 1
  })) == 0) {
    stop("No additive treatments are included in the NMA model", call. = FALSE)
  } else {
    components <- unique(unlist(components))
  }

  # Wide to long format
  data_long <- reshape2::melt(data = data, id.vars = "studlab", value.name = "Node")

  # Keep unique interventions
  data_long <- unique(data_long[, c("studlab", "Node")])
  data_long$Node <- gsub(" ", "", data_long$Node)

  # Calculate components frequency
  dum <- dummies(data_long, components, sep)

  # Frequency of each component
  comp_freq <- apply(dum[, -c(1:2)], 2, sum)

  # Additive for each component
  comp_add <- data.frame(
    "Component" = labels(comp_freq), "Frequency" = comp_freq, "A" = NA,
    "A_percent" = NA, "B" = NA, "B_percent" = NA, "C" = NA,
    "C_percent" = NA, "A/B" = NA
  )

  # cross table
  crosstab <- cross_ratio <- as.data.frame(matrix(ncol = length(components), nrow = length(components)))
  names(crosstab) <- names(cross_ratio) <- row.names(cross_ratio) <- row.names(crosstab) <- components

  studies <- split(dum, dum$studlab)

  for (i in components) {
    comp_add[which(comp_add$Component == i), "A"] <- sum(sapply(studies, FUN = function(x) {
      sum(x[, i]) == dim(x)[1]
    })) # component included in each arm
    comp_add[which(comp_add$Component == i), "B"] <- sum(sapply(studies, FUN = function(x) {
      sum(x[, i]) > 0
    })) # component included in at least one arm
    comp_add[which(comp_add$Component == i), "C"] <- sum(sapply(studies, FUN = function(x) {
      sum(x[, i]) == 0
    })) # component not included in any arm
  }

  comp_add$A_percent <- comp_add$A / length(studies)
  comp_add$B_percent <- comp_add$B / length(studies)
  comp_add$C_percent <- comp_add$C / length(studies)

  comp_add$A.B <- comp_add$A / comp_add$B
  # Cross-tabulations for each component
  m_i <- 0
  m_j <- 0
  for (i in components) {
    m_i <- m_i + 1
    for (j in components) {
      m_j <- m_j + 1
      crosstab[i, j] <- length(which(dum[, i] + dum[, j] == 2))

      if (m_i > m_j) {
        cross_ratio[i, j] <- paste0(length(which(dum[, i] + dum[, j] == 2)), "/", length(which(dum[, i] + dum[, i] == 2)))
      } else if (m_i < m_j) {
        cross_ratio[i, j] <- paste0(length(which(dum[, i] + dum[, j] == 2)), "/", length(which(dum[, i] + dum[, i] == 2)))
      } else {
        cross_ratio[i, j] <- paste0(length(which(dum[, i] + dum[, j] == 2)), "/", dim(data_long)[1])
      }
    }
    m_j <- 0
  }


  exp <- list("crosstable" = crosstab, "frequency" = comp_add)

  if (heatmap == TRUE) {
    data_heat <- t(crosstab) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("f_id") %>%
      tidyr::pivot_longer(-c("f_id"), names_to = "samples", values_to = "counts")

    ratios_heat <- t(cross_ratio) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("f_id") %>%
      tidyr::pivot_longer(-c("f_id"), names_to = "samples", values_to = "counts")

    # Calculate percentage
    data_heat$perc <- round(100 * sapply(ratios_heat$counts, FUN = function(x) {
      eval(parse(text = x))
    }), digits = digits)

    if (percentage == TRUE) {
      ratios_heat$label <- paste(data_heat$counts, "\n", paste0("(", data_heat$perc, "%)"))
      cap <- paste0("Total number of study arms: ", dim(data_long)[1])
    } else {
      ratios_heat$label <- paste(data_heat$counts, "\n", paste0("(", ratios_heat$counts, ")"))
      cap <- NULL
    }

    p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = data_heat$f_id, y = data_heat$samples, fill = data_heat$perc)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "white", high = "red", limit = c(0, 100)) +
      ggplot2::geom_text(ggplot2::aes(label = ratios_heat$label), color = "black", size = 4) +
      ggplot2::coord_fixed() +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::labs(caption = cap) +
      ggplot2::guides(fill = ggplot2::guide_legend("% arms")) +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 12, face = "bold", color = "black"),
        plot.caption = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_text(size = 12, face = "bold")
      )

    exp[["heatmat"]] <- p
  }

  exp
}
