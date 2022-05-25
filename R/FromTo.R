FromTo <- function(X) {
  if (length(X) > 2) {
    X2 <- c(rep(X, each = 2)[-1], rep(X, each = 2)[1])
    X3 <- matrix(X2, ncol = 2, byrow = TRUE)
  } else {
    X3 <- X
  }
}
