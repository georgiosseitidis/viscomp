check.groups <- function(x) {
  pass <- TRUE

  # Numeric elements
  num <- Hmisc::numeric.string(x) & grepl("+", x, fixed = T) == FALSE
  if (sum(as.numeric(x[num]) %% 1 != 0) > 0) {
    stop("Argument group must contains integer values", call. = FALSE)
  }

  if (sum(num) == length(x)) {
    pass
  } else {
    x <- x[which(!num)]

    # Range elements
    ranges <- strsplit(x, split = "-")
    ranges_check <- sapply(ranges,
      FUN = function(r) {
        Hmisc::all.is.numeric(r) & length(r) == 2 & sum(grepl("+", r, fixed = TRUE)) == 0
      }
    )
    if (sum(ranges_check) > 0) {
      range_integer <- sapply(ranges[ranges_check],
        FUN = function(r) {
          sum(as.numeric(r) %% 1 != 0) > 0
        }
      )
      if (sum(range_integer) > 0) {
        stop("Range must be between integer numbers", call. = FALSE)
      }

      range_x1x2 <- sapply(ranges[ranges_check],
        FUN = function(r) {
          r[1] > r[2]
        }
      )
      if (sum(range_x1x2) > 0) {
        stop("The first element of range must be smaller than the last one", call. = FALSE)
      }
    }
    x <- x[!ranges_check]

    # Plus check
    if (length(x) > 0) {
      plus <- strsplit(x, split = "+", fixed = TRUE)
      plus_check <- sapply(plus,
        FUN = function(p) {
          if (length(p) != 1) {
            FALSE
          } else {
            Hmisc::all.is.numeric(p)
          }
        }
      )

      if (sum(plus_check) > 0) {
        plus_integer <- sapply(plus[plus_check],
          FUN = function(r) {
            as.numeric(r[1]) %% 1 != 0
          }
        )
        if (sum(plus_integer) > 0) {
          stop("Integer number must be used for the the above a number", call. = FALSE)
        }
      }

      x <- x[!plus_check]
    }

    if (length(x) > 0) {
      pass <- FALSE
    }

    pass
  }
}
