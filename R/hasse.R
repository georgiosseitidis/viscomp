#' Hasse plot of multiple outcomes
#'
#' @description
#' The function produces a hasse diagram to represent a finite partially ordered set,
#' by drawing curves between interventions in network meta-analysis (NMA).
#'
#' @details
#' The function is a wrapper function for R function \code{\link[netmeta]{hasse}} in R package \bold{netmeta}.
#' Function \code{hasse} can only be used if R package \bold{hasseDiagram} is installed.
#'
#' @param ls A list of \code{\link[netmeta]{netmeta}} models.
#' @param random A \code{logical} vector that specifies whether the random-effects or the fixed-effect NMA model is used. If \code{random = NULL} the random-effects NMA model is used for each outcome.
#' @param small.values A \code{character} vector that specifies whether small intervention effects indicate a beneficial (\code{small.values = "good"}) or a harmful (\code{small.values = "bad"}) effect. If \code{small.values = NULL} small values assumed \code{good} for each outcome.
#'
#' @importFrom netmeta netrank netposet hasse
#'
#'
#' @return A hasse diagram.
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
#' # Hasse plot assuming that the first outcome is beneficial while the second is harmful.
#'
#' hasse(ls = list(net1, net2), small.values = c("bad", "good"))
#'
hasse <- function(ls, random = NULL, small.values = NULL) {

  ##
  # Check arguments
  ##

  if (class(ls) != "list") {
    stop("The class of ls is not list", call. = FALSE)
  } else if (!sum(sapply(ls, class) == "netmeta") == length(ls)) {
    stop("ls must contains objects class of netmeta", call. = FALSE)
  } else if (!is.null(random) & class(random) != "logical") {
    stop("The class random is not logical", call. = FALSE)
  } else if (!is.null(random) & length(random) != length(ls)) {
    stop("The length of ramdom must be equal with the length of ls", call. = FALSE)
  } else if (!is.null(small.values) & class(small.values) != "character") {
    stop("The class small.values is not character", call. = FALSE)
  } else if (!is.null(small.values) & length(small.values) != length(ls)) {
    stop("The length of small.values must be equal with the length of ls", call. = FALSE)
  } else if (!is.null(small.values) & sum(unique(small.values) %in% c("good", "bad")) != length(unique(small.values))) {
    stop("values of small.values must be either good or bad", call. = FALSE)
  }

  ##
  # Extract the p-scores from results
  ##

  if (is.null(random)) {
    random <- rep(TRUE, length(ls))
  }
  if (is.null(small.values)) {
    small.values <- rep("good", length(ls))
  }

  pscores <- pscores_method <- out <- treat <- a <- list()

  # Change the name of the list to be numeric
  names(ls) <- 1:length(ls)

  for (i in 1:length(ls)) {
    pscores[[i]] <- netmeta::netrank(ls[[i]], small.values = small.values[i])
    ifelse(random[i], pscores_method[[i]] <- pscores[[i]]$Pscore.random, pscores_method[[i]] <- pscores[[i]]$Pscore.fixed)
    # Extract the names of interventions
    treat[[i]] <- names(pscores_method[[i]])
  }

  ##
  # Find the common interventions of all outcomes and extract the corresponding p-scores
  ##

  common_inter <- Reduce(intersect, treat)
  for (i in 1:length(ls)) {
    a[[i]] <- which(names(pscores_method[[i]]) %in% common_inter)
    out[[i]] <- pscores_method[[i]][a[[i]]]
  }

  ##
  # Create a dataframe included the extracted information
  ##

  df <- data.frame(matrix(unlist(out), ncol = length(out)))
  rownames(df) <- names(out[[1]])

  ##
  # Hasse diagram
  ##

  po <- netmeta::netposet(df)
  p <- netmeta::hasse(po)
}
