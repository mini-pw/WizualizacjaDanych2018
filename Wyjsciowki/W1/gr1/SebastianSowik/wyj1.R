library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(patchwork)
install.packages("ggthemes")
library(ggthemes)
#Przedstawic gestosc rozkladu smiertelnosci w podziale 
#na kontynenty. Pokazac ten sam wykres z uzyciem czterech 
#tematów z pakietu ggthemes. Wynikowy wykres pokazac na 
#jednym rysunku (z uzyciem np. grid.arrange).

head(countries)

density_death1 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density() + 
  theme_classic()

density_death2 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density() + 
  theme_classic()

density_death3 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density() + 
  theme_classic()

density_death4 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density() + 
  theme_classic()


(density_death1 + density_death2 )/(density_death3 + density_death4)
