library(dplyr)
library(ggplot2)
library(magick)

data <- ChickWeight %>% group_by(Time, Diet) %>%
 summarise(weight = mean(weight))

# ggplot(data, aes(x=Time)) + 
#   geom_line(aes(y = weight, color = Diet))


p <- ggplot(ChickWeight, aes(x=Time, y = weight, color = Diet)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Diet, ncol = 1) + 
  ggtitle('Comparison of different diet affects on chicken weights')

p
ggsave(filename = "p.pdf", plot = p, device = "pdf")


