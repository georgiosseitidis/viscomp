addColorBackground <- function(outcomeNames, components, data, intervals, mycolors, cex,
                               cex_values = NULL, cex_outcomes = NULL) {
  if (!is.null(cex_values)) {
    cex_v <- cex_values
  } else {
    cex_v <- cex
  }

  if (!is.null(cex_outcomes)) {
    cex_o <- cex_outcomes
  } else {
    cex_o <- cex
  }

  for (k in 1:length(outcomeNames)) {
    start <- circlize::get.cell.meta.data("cell.start.degree", "Outcomes", k + 1)
    end <- circlize::get.cell.meta.data("cell.end.degree", "Outcomes", k + 1)
    top <- circlize::get.cell.meta.data("cell.top.radius", "Outcomes", k + 1)
    bottom <- circlize::get.cell.meta.data("cell.bottom.radius", "Outcomes", k + 1)
    circlize::draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2 = bottom, border = NA)
    circlize::circos.text(5, 50, sector.index = "Outcomes", facing = "downward", track.index = k + 1, labels = outcomeNames[k], cex = cex_o)
  }

  for (i in 1:dim(data)[1]) {
    for (j in 1:length(outcomeNames)) {
      start <- circlize::get.cell.meta.data("cell.start.degree", components[i], j + 1)
      end <- circlize::get.cell.meta.data("cell.end.degree", components[i], j + 1)
      top <- circlize::get.cell.meta.data("cell.top.radius", components[i], j + 1)
      bottom <- circlize::get.cell.meta.data("cell.bottom.radius", components[i], j + 1)

      if (is.na(data[i, j]) == TRUE) { # case data is NA: draw a white sector
        circlize::draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2 = bottom, border = "#f2f2f2")
      } else {
        for (k in 1:32) {
          if (as.numeric(data[i, j]) >= intervals[k] && as.numeric(data[i, j]) < intervals[k + 1]) {
            circlize::draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2 = bottom, border = NA, col = mycolors[k])
          }
        }
        circlize::circos.text(5, 50, sector.index = components[i], facing = "downward", track.index = j + 1, labels = round(data[i, j], 2), cex = cex_v)
      }
    }
  }
}
