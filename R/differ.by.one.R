differ.by.one <- function(M, combination = NULL, nodes_elements = NULL) {
  pos1 <- pos2 <- NULL
  k <- 1

  if (is.null(combination)) {
    # Differ by one component

    for (i in 1:dim(M)[1]) {
      for (j in 1:dim(M)[1]) {
        if (sum(abs(M[i, ] - M[j, ])) == 1) {
          pos1[k] <- i
          pos2[k] <- j
          k <- k + 1
        }
      }
    }
  } else {
    if (length(combination) == 1) {
      # Differ by one specific component

      for (i in 1:dim(M)[1]) {
        for (j in 1:dim(M)[1]) {
          if (sum(abs(M[i, ] - M[j, ])) == 1 & abs(M[i, combination] - M[j, combination]) == 1) {
            pos1[k] <- i
            pos2[k] <- j
            k <- k + 1
          }
        }
      }
    } else {
      # Differ by one specific component combination
      pairs <- differ.combination(nodes_elements, combination)
      pos1 <- pairs[, 1]
      pos2 <- pairs[, 2]
    }
  }

  if (is.null(pos1) | is.null(pos2)) {
    if (is.null(combination)) {
      stop("Comparisons that differ by one component were not identified", call. = FALSE)
    } else {
      stop("Comparisons that differ by the component ", paste(combination, collapse = ", "), " were not identified", call. = FALSE)
    }
  }

  pos <- cbind(pos1, pos2)

  # Exclude duplicate cases
  pos <- as.data.frame(matrix(pos[!duplicated(t(apply(pos, 1, sort))), ], ncol = 2))
  names(pos) <- c("pos1", "pos2")

  pos
}
