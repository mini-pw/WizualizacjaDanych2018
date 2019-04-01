library(ggplot2)
library(patchwork)

data <- airquality[1:31,]

p1 <- ggplot(data=data, aes(x=Day, y=Ozone)) +
  geom_line(color='red')

p2 <- ggplot(data=data, aes(x=Day, y=Wind)) +
  geom_line(color='blue')

plots <- p2 / p1


cairo_ps("wyjsciowka4.eps", height = 7.5, width = 8, family = "Dyuthi")
plots
dev.off()