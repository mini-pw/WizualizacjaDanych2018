library(ggthemes)
library(ggplot2)
library(SmarterPoland)
library(gridExtra)
library(grid)


density_death_1 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.4) + theme_calc()

density_death_2 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.4) + theme_excel()

density_death_3 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.4) + theme_pander()

density_death_4 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.4) + theme_gdocs()



grid.arrange(density_death_1, density_death_2, density_death_3, density_death_4, nrow=2)

