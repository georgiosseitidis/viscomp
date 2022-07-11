check.arguments <- function(model, median, random, small.values, outcomeNames, cex_components, cex_values, cex_outcomes) {
  if (inherits(model, "list") == FALSE) {
    stop("The class of model is not list", call. = FALSE)
  } else if (length(model) < 2) {
    stop("The length of model must be at least two", call. = FALSE)
  }
  numOfOutcomes <- length(model)

  # Check random argument
  if (length(random) == 1) {
    if (inherits(random, "logical") == FALSE) {
      stop("The class of random is not logical", call. = FALSE)
    } else {
      random <- rep(random, numOfOutcomes)
    }
  } else if (length(random) != numOfOutcomes) {
    stop("The length of random must be equal with the number of the outcomes", call. = FALSE)
  }

  # Check small.values argument

  if (!is.null(small.values) & inherits(small.values, "character") == FALSE) {
    stop("The class small.values is not character", call. = FALSE)
  } else if (!is.null(small.values) & length(small.values) != length(model)) {
    stop("The length of small.values must be equal with the length of model", call. = FALSE)
  } else if (!is.null(small.values) & sum(unique(small.values) %in% c("good", "bad")) != length(unique(small.values))) {
    stop("values of small.values must be either good or bad", call. = FALSE)
  }


  # Check cex arguments
  if (!is.null(cex_components)) {
    if (inherits(cex_components, "numeric") == FALSE) {
      stop("The class of cex_components is not numeric", call. = FALSE)
    } else if (length(cex_components) > 1) {
      stop("The length of cex_components must be one", call. = FALSE)
    } else if (cex_components < 0) {
      stop("Argument cex_components must be a positive number", call. = FALSE)
    }
  }

  if (!is.null(cex_values)) {
    if (inherits(cex_values, "numeric") == FALSE) {
      stop("The class of cex_values is not numeric", call. = FALSE)
    } else if (length(cex_values) > 1) {
      stop("The length of cex_values must be one", call. = FALSE)
    } else if (cex_values < 0) {
      stop("Argument cex_values must be a positive number", call. = FALSE)
    }
  }

  if (!is.null(cex_outcomes)) {
    if (inherits(cex_outcomes, "numeric") == FALSE) {
      stop("The class of cex_outcomes is not numeric", call. = FALSE)
    } else if (length(cex_outcomes) > 1) {
      stop("The length of cex_outcomes must be one", call. = FALSE)
    } else if (cex_outcomes < 0) {
      stop("Argument cex_outcomes must be a positive number", call. = FALSE)
    }
  }

  # Check length of outcome names
  if (length(outcomeNames) != numOfOutcomes) {
    stop("The length of outcome names must be equal with the number of the outcomes", call. = FALSE)
  }

  # Check model and median argument
  for (outcome in 1:numOfOutcomes) {
    if (inherits(model[[outcome]], "netmeta") == FALSE) {
      stop(paste("The class of model", outcome, "is not of netmeta"), call. = FALSE)
    } else if (inherits(median, "logical") == FALSE) {
      stop("The class of median is not logical", call. = FALSE)
    } else if (length(median) > 1) {
      stop("The length of median must be one", call. = FALSE)
    } else if (inherits(random[[outcome]], "logical") == FALSE) {
      stop(paste("The class of random", outcome, "is not logical"), call. = FALSE)
    }
  }

  random
}
