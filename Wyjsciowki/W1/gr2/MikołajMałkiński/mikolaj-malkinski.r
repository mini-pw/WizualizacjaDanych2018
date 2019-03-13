library(ggplot2)
library(SmarterPoland)
library(dplyr)


filtered_countries <- countries %>% filter(continent == 'Europe' | continent == 'Africa')
plot1 <- ggplot(
    data = na.omit(filtered_countries),
    aes(x = population, fill = continent)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("red", "green"))
plot2 <- ggplot(
  data = na.omit(filtered_countries),
  aes(x = population, fill = continent)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("blue", "black"))
plot3 <- ggplot(
  data = na.omit(filtered_countries),
  aes(x = population, fill = continent)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("yellow", "pink"))
plot4 <- ggplot(
  data = na.omit(filtered_countries),
  aes(x = population, fill = continent)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("blue", "orange"))

source("https://install-github.me/thomasp85/patchwork")
library(patchwork)


(plot1 + plot2) / (plot3 + plot4)