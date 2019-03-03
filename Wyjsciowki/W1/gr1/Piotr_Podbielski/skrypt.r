library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(gridExtra)
library(grid)

plot1 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme_bw(base_size = 14) +
  facet_wrap(~ continent) +
  ggtitle("density with theme_bw")

plot2 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme_dark(base_size = 14)+
  facet_wrap(~ continent) +
  ggtitle("density with theme_dark")

plot3 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ continent) +
  ggtitle("density with theme_minimal")

plot4 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme_void(base_size = 14) +
  facet_wrap(~ continent)  +
  ggtitle("density with theme_void")

grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)
