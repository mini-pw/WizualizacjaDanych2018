library(dplyr)
library(ggplot2)
library(gridExtra)
library(SmarterPoland)


population_density_plot <- countries %>% 
  filter(continent %in% c("Asia", "Europe")) %>% 
  ggplot(aes(x = population, fill = continent)) + 
  geom_density()

grid.arrange(
  population_density_plot + scale_fill_manual(values = c("red", "gray")),
  population_density_plot + scale_fill_manual(values = c("blue", "yellow")),
  population_density_plot + scale_fill_manual(values = c("green", "navyblue")),
  population_density_plot + scale_fill_manual(values = c("black", "orange"))
)
