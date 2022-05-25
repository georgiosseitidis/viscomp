drawDonughts <- function(outcomeNames, components, cex_components = NULL) {
  if (!is.null(cex_components)) {
    cex_c <- cex_components
  } else {
    cex_c <- 0.65
  }

  circlize::circos.clear()
  # par(mar = c(0, 0, 0,0))
  no <- length(outcomeNames)
  addComponent <- c("Outcomes", components)
  circlize::circos.par(points.overflow.warning = FALSE, track.margin = c(0, 0), start.degree = 100)
  circlize::circos.initialize(factors = addComponent, xlim = c(0, 10))

  circlize::circos.trackPlotRegion(addComponent, ylim = c(0, 100), bg.border = NA, track.height = 0.05, panel.fun = function(x, y) {
    circlize::circos.text(5, 100, facing = "bending", cex = cex_c, circlize::get.cell.meta.data("sector.index"))
  })

  if (no == 1 | no == 2) {
    trHigh <- 0.3
    cex <- 0.5
  } else if (no == 3) {
    trHigh <- 0.17
    cex <- 0.5
  } else if (no == 4) {
    trHigh <- 0.13
    cex <- 0.4
  } else if (no == 5) {
    trHigh <- 0.09
    cex <- 0.4
  } else if (no >= 6) {
    trHigh <- 0.05
    cex <- 0.4
  }
  for (i in 1:no) {
    circlize::circos.trackPlotRegion(addComponent, ylim = c(0, 100), bg.border = NA, track.height = trHigh)
  }

  cex
}
