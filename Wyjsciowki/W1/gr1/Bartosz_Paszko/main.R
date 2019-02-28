# Przedstawic gestosc rozkladu smiertelnosci w podziale na kontynenty. 
# Pokazac ten sam wykres z uzyciem czterech tematów z pakietu ggthemes. 
# Wynikowy wykres pokazac na jednym rysunku (z uzyciem np. grid.arrange).

library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(gridExtra)


p1 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme_classic()

p2 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme_bw()

p3 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme_minimal()

p4 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme_dark()


grid.arrange(p1, p2, p3, p4, ncol=2)
