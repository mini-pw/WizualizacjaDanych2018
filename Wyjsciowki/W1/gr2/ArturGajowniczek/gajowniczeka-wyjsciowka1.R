library(ggplot2)
library(SmarterPoland)
library(dplyr)
filtered <- filter(countries, continent == 'Africa' | continent == 'Europe')
clr <- c('blue', 'red')
p1 <- ggplot(data=filtered, aes(x=population, fill=continent)) + geom_density(alpha = 0.5)
p2 <- ggplot(data=filtered, aes(x=population, fill=continent)) + geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"))
p3 <- ggplot(data=filtered, aes(x=population, fill=continent)) + geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("green", "blue"))
p4 <- ggplot(data=filtered, aes(x=population, fill=continent)) + geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#e9a3c9", "#a1d76a"))
p4

library(patchwork)
(p1 + p2)/(p3 + p4)
