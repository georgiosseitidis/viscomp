unique.combinations <- function(combination, sep) {
  elements <- strsplit(combination, split = paste0("[", sep, "]"), perl = TRUE)
  excluded <- NULL
  i <- 1

  while (i <= length(combination) & i <= length(elements)) {
    elements_i <- elements[[i]]
    same_i <- sapply(elements, FUN = function(x) {
      sum(elements_i %in% x) == length(elements_i) & length(x) == length(elements_i)
    })
    if (sum(same_i) > 1) {
      pos <- which(same_i == TRUE)
      pos <- pos[!pos == i]
      excluded <- c(excluded, sapply(elements[pos], FUN = function(x) {
        (paste(x, collapse = " "))
      }))
      elements <- elements[-pos]
    }
    i <- i + 1
  }

  if (length(excluded) > 0) {
    warning(paste(paste(excluded, collapse = ", "), "excluded from argument combination as duplicate"), call. = FALSE)
  }

  combination <- sapply(elements, FUN = function(x) {
    (paste(x, collapse = sep))
  })
  combination
}
