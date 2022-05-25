check.combinations <- function(x, y, sep) {
  elements <- strsplit(x, split = paste0("[", sep, "]"), perl = TRUE)
  included <- sapply(elements,
    FUN = function(z) {
      ifelse(sum(z %in% y) == length(z), TRUE, FALSE)
    }
  )
  if (sum(included) != length(included)) {
    stop("Argument combination must includes network's components combinations", call. = FALSE)
  }
  included
}
