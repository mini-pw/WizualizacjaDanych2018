#Przedstawic gestosc rozkladu populacji tylko dla Europy i Afryki, 
#gdzie kontynent jest zaznaczony jako wypelnienie (fill). 
#Pokazac ten sam wykres z uzyciem czterech roznych palet kolorow.
#Wynikowy wykres pokazac na jednym rysunku (z uzyciem np. grid.arrange)

library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(patchwork)
 

wykres <- ggplot(data = filter(countries, continent == "Europe" | continent == "Africa"), aes(x = population , fill= continent ) ) + 
         geom_density(alpha = 0.4)

p1 = wykres
p2 = wykres + scale_fill_manual(values = c("red", "grey"))
p3 = wykres + scale_fill_manual(values = c("pink", "violet"))
p4 = wykres + scale_fill_manual(values = c("green", "blue"))

(p1 + p2) / (p3 + p4)
