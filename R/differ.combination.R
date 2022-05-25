differ.combination <- function(nodes_elements, combination_components) {
  pair <- NULL

  for (i in 1:length(nodes_elements)) {
    elements_i <- nodes_elements[[i]]

    differ <- sapply(nodes_elements, FUN = function(x) {
      A <- sum(combination_components %in% elements_i) == length(combination_components) # element i includes the combination
      A_none <- sum(combination_components %in% elements_i) == 0

      # x includes the combination
      B <- sum(combination_components %in% x) == length(combination_components) # element i includes the combination
      B_none <- sum(combination_components %in% x) == 0

      # components of element of i except the combination
      if (A) {
        A_rest <- elements_i[-which(elements_i %in% combination_components)]
      } else if (A_none) {
        A_rest <- elements_i
      }

      if (B) {
        B_rest <- x[-which(x %in% combination_components)]
      } else if (B_none) {
        B_rest <- x
      }

      # same rest components
      if (((A & B_none) | (B & A_none))) {
        if (length(A_rest) == length(B_rest) & length(A_rest) != 0) {
          C <- sum(A_rest[order(A_rest)] == B_rest[order(B_rest)]) == length(A_rest)
        } else {
          C <- FALSE
        }
      } else {
        C <- FALSE
      }

      C
    })

    if (sum(differ) > 0) {
      if (sum(differ) == 1) {
        pair <- c(pair, i, which(differ == TRUE))
      } else {
        differ_pos <- which(differ == TRUE)
        for (j in 1:length(differ_pos)) {
          pair <- c(pair, i, differ_pos[j])
        }
      }
    }
  }

  if (!is.null(pair)) {
    pair <- matrix(pair, ncol = 2, byrow = TRUE)
    colnames(pair) <- c("pos1", "pos2")
  } else {
    stop("Nodes that differs by this combination was not found", call. = FALSE)
  }

  pair
}
