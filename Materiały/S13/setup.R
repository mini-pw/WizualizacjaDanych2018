install.packages("r2d3")

library(r2d3)
r2d3(
  data = c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20),
  script = system.file("examples/barchart.js", package = "r2d3")
)

r2d3(data = c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20),
     script = "./Materiały/S13/r2d3-example.js")
