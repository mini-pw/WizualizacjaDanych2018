library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(ggthemes)


death_boxplot <- ggplot(data = na.omit(countries), aes(y = death.rate, x = continent)) +
  geom_boxplot()

density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.35)
p1 <- density_death + theme_wsj()
p2 <- density_death +theme_economist()
p3 <- density_death + theme_excel_new()
p4 <- density_death + theme_gdocs()
(p1 + p2)/ (p3 + p4)
