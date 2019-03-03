#Przedstawic gestosc rozkladu smiertelnosci w podziale na kontynenty.
#Pokazac ten sam wykres z uzyciem czterech tematów z pakietu ggthemes. 
#Wynikowy wykres pokazac na jednym rysunku (z uzyciem np. grid.arrange).

library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(patchwork)

density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme(legend.position = "none")

p1 <- density_death + ggtitle('theme_bw') & theme_bw()
p2 <- density_death + ggtitle('theme_dark') & theme_dark()
p3 <- density_death + ggtitle('theme_minimal')& theme_minimal()

(p1 + p2) / (p3 + density_death)