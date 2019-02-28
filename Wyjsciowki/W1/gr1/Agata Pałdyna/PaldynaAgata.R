#-----------------------------------------------

# Laboratoria nr 2
# 25.02.2019

#-----------------------------------------------

# Wyjściówka

#-----------------------------------------------

# Przedstawic gestosc rozkladu smiertelnosci w podziale na kontynenty. 
# Pokazac ten sam wykres z uzyciem czterech tematów z pakietu ggthemes. 
# Wynikowy wykres pokazac na jednym rysunku (z uzyciem np. grid.arrange).

library(ggplot2)
library(SmarterPoland)
library(dplyr)

density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2)

install.packages("ggthemes")
library(ggthemes)

density_death1 <- density_death + theme_stata()
density_death2 <- density_death + theme_light()
density_death3 <- density_death + theme_economist()
density_death4 <- density_death + theme_dark()

# Wynikowy wykres
grid.arrange(density_death1, density_death2, density_death3, density_death4, ncol = 2)
