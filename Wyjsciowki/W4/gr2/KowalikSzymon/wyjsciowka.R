library(dplyr)
library(ggplot2)
library(patchwork)

p <- ChickWeight %>% 
  ggplot(aes(x = Time, y = weight, group=Chick, color=Diet)) +
  geom_line() + geom_point() +
  facet_wrap(~Diet, nrow = 1)

dev.off()

ggsave(filename = "p.pdf", plot = p, height = 10, width = 25, device = "pdf")
