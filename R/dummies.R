dummies <- function(data, components, sep) {

  # Create dummy variables for each component
  for (i in 1:length(components)) {
    data[, components[i]] <- 0
  }

  # Write each node as a combination of components
  for (i in 1:dim(data)[1]) {

    # Components of each intervention
    dummy_comp_i <- strsplit(data$Node[i], split = paste0("[", sep, "]"), perl = TRUE)[[1]]

    for (j in 1:length(dummy_comp_i)) {
      if (dummy_comp_i[j] %in% components) {
        data[i, dummy_comp_i[j]] <- 1
      }
    }
  }
  data
}
