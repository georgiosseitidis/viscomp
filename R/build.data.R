build.data <- function(model, median, random, small.values, numOfOutcomes, components, outcomeNames, sep) {

  # Dataframe to store estimates
  df <- data.frame(matrix(nrow = length(components), ncol = numOfOutcomes))
  rownames(df) <- components
  colnames(df) <- outcomeNames
  for (outcome in 1:numOfOutcomes) {
    pscores <- netmeta::netrank(model[[outcome]], small.values = small.values[outcome])

    # Components of each node
    nodes <- names(pscores$ranking.fixed)
    nodes <- gsub(" ", "", nodes)

    components_node <- strsplit(nodes, split = paste("[", sep, "]", sep = ""), perl = TRUE)

    # select effect
    if (random[outcome] == TRUE) {
      pscoresvalues <- pscores$Pscore.random
    } else {
      pscoresvalues <- pscores$Pscore.fixed
    }

    for (i in 1:dim(df)[1]) {
      result <- NA
      val <- c()

      for (j in 1:length(components_node)) { # from pscores table
        if (grepl(rownames(df)[i], components_node[j])) {
          val <- c(val, pscoresvalues[j])
        }
      }

      # select median OR mean
      if (!is.null(val)) {
        if (median == TRUE) {
          result <- median(val)
        } else {
          result <- mean(val)
        }
      } else {
        result <- NULL
      }


      if (is.null(result)) {
        df[i, outcome] <- NA
      } else {
        df[i, outcome] <- round(result, digits = 2) * 100
      }
    }
  }
  dat <- list(df = df, components = components)

  dat
}
