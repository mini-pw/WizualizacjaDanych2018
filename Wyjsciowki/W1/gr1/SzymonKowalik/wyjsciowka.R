library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(ggthemes)
library(patchwork)

density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2)

(density_death + theme_bw()) + (density_death + theme_dark()) +
(density_death + theme_calc()) + (density_death + theme_classic()) +
plot_layout(ncol = 2)
