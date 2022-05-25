#' Components Network Graph
#'
#' @description
#' The Components Network Graph is meant to visualize the frequency of components’ combinations found in the network.
#'
#' @details
#' The function resembles a network plot where nodes represent the individual components found in the network
#' and edges represent the combination of components found in at least one treatment arm of the trials included in the
#' network meta-analysis model. Each edge’s color represents one of the unique interventions (components’ combination)
#' found in the network of interventions. Edges’ thickness indicate the frequency by which each intervention
#' (combination of components) was observed in the network (number of arms in which the combination was assigned).
#' The number of the most frequent combinations can be modified from the argument \code{mostF}. The function by
#' default plots the five most frequent components' combinations found in the network.
#'
#'
#' @param model An object of class \code{\link[netmeta]{netmeta}}.
#' @param sep A single character that defines the separator between interventions components.
#' @param mostF Number of most frequent combinations of the network.
#' @param excl A character vector that specifies the combinations to be excluded from the plot.
#' @param title A single character that specifies the overall title of the plot.
#' @param print_legend \code{logical}. If \code{TRUE} the legend is printed.
#' @param size_legend size of the legend.
#'
#' @importFrom qgraph qgraph
#' @importFrom graphics par plot legend
#'
#' @return Returns (invisibly) a \code{\link[qgraph]{qgraph}} object.
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
#'   treat2 = treat2, data = NMAdata, ref = "A"
#' )
#' compGraph(model = net)
compGraph <- function(model, sep = "+", mostF = 5, excl = NULL, title = "Most frequent combinations of components",
                      print_legend = TRUE, size_legend = 0.825) {

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
  } else if (!class(mostF) %in% c("numeric", "integer")) {
    stop("The class of mostF must be numeric or integer", call. = FALSE)
  } else if (length(mostF) > 1) {
    stop("The length of mostF must be one", call. = FALSE)
  } else if (mostF <= 0) {
    stop("mostF must be positive number", call. = FALSE)
  } else if (mostF %% 1 != 0) {
    stop("mostF must be an interger number", call. = FALSE)
  } else if (!is.null(excl)) {
    if (class(excl) != "character") {
      stop("The class of excl is not character", call. = FALSE)
    }
  } else if (class(title) != "character") {
    stop("The class of title is not character", call. = FALSE)
  } else if (length(title) > 1) {
    stop("The length of title must be one", call. = FALSE)
  } else if (class(print_legend) != "logical") {
    stop("The class of print_legend is not logical", call. = FALSE)
  } else if (length(print_legend) > 1) {
    stop("The length of print_legend must be one", call. = FALSE)
  } else if (!class(size_legend) %in% c("numeric", "integer")) {
    stop("The class of size_legend must be numeric or integer", call. = FALSE)
  } else if (length(size_legend) > 1) {
    stop("The length of size_legend must be one", call. = FALSE)
  } else if (size_legend <= 0) {
    stop("size_legend must be a positive number", call. = FALSE)
  }

  ##
  # Construct the data of the plot
  ##

  data <- unique(data.frame("t" = c(model$treat1, model$treat2), "study" = c(model$studlab, model$studlab)))

  comp.freq <- table(data$t)

  if (!is.null(excl)) {
    excl_true <- which(excl %in% comp.freq)
    if (length(excl_true) > 0) {
      excl_f <- excl[!excl_true]
      excl <- excl[excl_true]

      if (length(excl_f) == 1) {
        warning(paste("Combination", excl_f, "was excluded since it was not observed in the network"))
      } else {
        warning(paste("Combinations", paste(excl_f, collapse = ", "), "were excluded since they were not observed in the network"))
      }

      if (length(excl) == 0) {
        excl <- NULL
      }
    }

    if (length(excl) == length(comp.freq)) {
      stop(paste("The length of excl is equal with the total number of observed combinations in the network"), .call = FALSE)
    }
  }

  if (mostF >= length(comp.freq) - 1) {
    stop(paste("mostF must be smaller than the number of treatments in the network"), .call = FALSE)
  }
  ntwrk <- sort(comp.freq[!(names(comp.freq) %in% excl)], decreasing = TRUE)[1:mostF]

  Combs <- gsub(" ", "", names(ntwrk))
  Weights <- as.numeric(ntwrk)

  res1 <- strsplit(Combs, split = paste("[", sep, "]", sep = ""), perl = TRUE)

  # tables to be merged
  res4 <- lapply(res1, FromTo)

  # merge the tables
  FrToMat <- do.call("rbind", res4)

  if (sum(sapply(res4, length) <= 2) == length(res4)) {
    groups <- rep(1, length(res4))
  } else {
    groups <- sapply(sapply(res4, matrix, ncol = 2), nrow)
  }

  # Weights vector
  Wghts <- unlist(mapply(rep, x = Weights, each = groups))

  E <- (data.frame(from = FrToMat[, 1], to = FrToMat[, 2], width = Wghts))

  # Colors vector

  clrs <- grDevices::palette.colors(n = mostF, palette = "Tableau")

  CLRS <- unlist(mapply(rep, x = clrs, each = groups))

  ##
  # plot
  ##
  graphics::par(cex = 0.75, mai = c(0.1, 0.1, 1, 0.1) + 1)
  graphics::par(fig = c(0, 0.75, 0, 1))
  qgraph::qgraph(E,
    mode = "direct", edge.color = CLRS, fade = FALSE, arrows = FALSE, layout = "circle",
    title = title
  )
  graphics::par(fig = c(0.7, 1, 0, 1), new = TRUE, cex = 1)
  graphics::par(mar = c(1, 1, 1, 1))
  graphics::plot(c(0, 1), c(0, 1), ann = FALSE, bty = "n", type = "n", xaxt = "n", yaxt = "n")

  if (print_legend) {
    graphics::legend(
      x = "left", legend = c("Combination", as.character(Combs), "# of arms", Weights),
      ncol = 2, bty = "n", cex = size_legend
    )
  }
}
