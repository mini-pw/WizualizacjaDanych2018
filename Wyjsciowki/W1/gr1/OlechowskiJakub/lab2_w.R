install.packages('ggthemes')
library(ggthemes)
library(ggplot2)
library(SmarterPoland)
library(dplyr)

gg1 <- ggplot(countries, aes(fill = continent, x = death.rate)) +
  geom_density(alpha = 0.2)

gg1 * theme_classic() + gg1 * theme_dark() + gg1 * theme_foundation() + gg1 * theme_solarized()

  