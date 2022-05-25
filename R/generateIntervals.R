generateIntervals <- function(data) {
  intervals <- c(0, 3.1, 6.2, 9.3, 12.4, 15.5, 18.6, 21.7, 24.8, 27.9, 31, 34.1, 37.2, 40.3, 43.4, 46.5, 49.6, 52.7, 55.8, 58.9, 62, 65.1, 68.2, 71.3, 74.4, 77.5, 80.6, 83.7, 86.8, 89.9, 93, 96.1, 100)
  mycolors <- c("#e00000", "#ef0000", "#FF0000", "#FF3000", "#FF4000", "#FF5000", "#FF6000", "#FF7000", "#FF8000", "#FF9000", "#FFA000", "#FFB000", "#FFC000", "#FFD000", "#FFE000", "#FFF000", "#FFFF00", "#F0FF00", "#d0ff00", "#C0FF00", "#B0FF00", "#A0FF00", "#90FF00", "#70FF00", "#50FF00", "#0ffc00", "#0ff200", "#0ee800", "#0ee000", "#0ed800", "#0ecc00", "#0dc100")
  gen_intervals <- list(intervals = intervals, mycolors = mycolors)

  gen_intervals
}
