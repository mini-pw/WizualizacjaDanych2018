density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  coord_flip() +
  theme(legend.position = "none")

grid.arrange(density_death + theme_classic(), density_death + theme_dark(), density_death + theme_gray(), density_death + theme_light(),
             ncol = 2, heights = c(0.5, 0.5), widths = c(0.5, 0.5))

