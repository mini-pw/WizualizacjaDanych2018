base_plot <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  coord_flip() 

library(ggthemes)
grid.arrange(base_plot + theme_excel(), base_plot + theme_calc(), base_plot + theme_dark(), base_plot + theme_pander())
