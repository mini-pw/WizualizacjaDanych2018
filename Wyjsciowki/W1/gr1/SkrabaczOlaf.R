library(ggplot2)
library(dplyr)
library(SmarterPoland)
library(magrittr)
library(ggthemes)
death_density <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2)

p1 <- death_density + ggthemes::theme_calc()
p2 <- death_density + ggthemes::theme_economist()
p3 <- death_density + ggthemes::theme_gdocs()
p4 <- death_density + ggthemes::theme_fivethirtyeight()
grid.arrange(p1, p2, p3, p4, 
             ncol = 2)
