nmares <- function(model, random) {
  ref <- as.character(model$reference.group) # Reference category
  summ_NMA <- summary(model) # Summary of NMA model

  # Set NMA type
  ifelse(random, type <- "random", type <- "fixed")

  # data.frame with the NMA results
  results <- data.frame(
    "Node" = as.character(rownames(summ_NMA[[type]][["TE"]])),
    "TE" = summ_NMA[[type]][["TE"]][, ref],
    "lb" = summ_NMA[[type]][["lower"]][, ref],
    "ub" = summ_NMA[[type]][["upper"]][, ref],
    "z_stat" = summ_NMA[[type]][["statistic"]][, ref]
  )

  # Order based on the absolute treatment effect
  results <- results[order(abs(results$TE), decreasing = TRUE), ]

  results
}
