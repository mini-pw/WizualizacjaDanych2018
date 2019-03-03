#Wyjściówka 25.02.2019
#Przedstawic gestosc rozkladu smiertelnosci w podziale na kontynenty. 
#Pokazac ten sam wykres z uzyciem czterech tematów z pakietu ggthemes. 
#Wynikowy wykres pokazac na jednym rysunku (z uzyciem np. grid.arrange).

library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(ggthemes)

#ggplot(countries_f, aes(x = death.rate)) +
#  geom_density() +
#  facet_wrap(~ continent)

density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme(legend.position = "none")

p1<-density_death + theme_excel()
p2<-density_death + theme_bw()
p3<-density_death + theme_economist()
p4<-density_death + theme_foundation()

grid.arrange(p1, p2, p3,p4, 
             ncol = 2)
