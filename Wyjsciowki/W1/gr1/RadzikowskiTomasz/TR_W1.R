
library(ggplot2)
library(SmarterPoland)
library(dplyr)

density_death <- ggplot(data = na.omit(countries_f), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2)
library(gridExtra)
library(grid)
library(ggthemes)
grid.arrange(density_death+
               theme(legend.position = "none"), density_death+theme_economist()+
               theme(legend.position = "none"), density_death+theme_calc()+
               theme(legend.position = "none"), density_death+theme_excel()+
               theme(legend.position = "none"), get_legend(density_death),
             ncol = 2)
