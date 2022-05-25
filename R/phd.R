# Prepare Heat Data
phd <- function(components, mean_eff, median, sep) {

  # Dataframe to store estimates
  df <- data.frame(matrix(nrow = length(components), ncol = length(components)))
  colnames(df) <- rownames(df) <- components
  dat <- matrix(mean_eff, nrow = 1)

  # Components of each node
  nodes <- labels(mean_eff)
  components_node <- strsplit(nodes, split = paste("[", sep, "]", sep = ""), perl = TRUE)

  # Set measurement unit
  ifelse(median, funct <- "stats::median", funct <- "mean")

  for (i in 1:dim(df)[1]) {
    for (j in 1:dim(df)[1]) {

      # Find the nodes that include components i and j
      ind <- unlist(lapply(components_node, FUN = function(x) {
        sum(c(colnames(df)[i], colnames(df)[j]) %in% x) == 2
      }))

      # Calculate the mean estimate
      if (length(as.vector(dat[, ind])) == 0) { # The comparison was not observed
        df[i, j] <- NaN
      } else { # The comparison was observed
        df[i, j] <- apply(matrix(dat[, ind], ncol = 1), 2, FUN = eval(parse(text = funct)))
      }
    }
  }

  # Upper triangle form
  df[upper.tri(df)] <- NA
  melted_data <- reshape2::melt(as.matrix(df))

  # Text to be printed
  txt <- paste(format(round(melted_data$value, 2), nsmall = 2))

  # For the non observed comparisons print "X"
  txt[is.na(round(melted_data$value))] <- NA
  txt[is.nan(melted_data$value)] <- "X"

  # Rename the columns of the melted dataset
  colnames(melted_data) <- c("Var1", "Var2", "value")

  Heatdata <- list("data" = melted_data, "text" = txt)
}
