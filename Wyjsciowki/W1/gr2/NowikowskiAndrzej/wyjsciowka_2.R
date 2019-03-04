# Przedstawic gestosc rozkladu populacji tylko dla Europy i Afryki, gdzie kontynent jest zaznaczony jako wypelnienie (fill). 
# Pokazac ten sam wykres z uzyciem czterech roznych palet kolorow. Wynikowy wykres pokazac na jednym rysunku (z uzyciem np. grid.arrange).

library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(patchwork)

zig <- countries %>% 
  filter(continent == "Africa" | continent == "Europe")

p <- ggplot(data=zig, aes(x = population, fill=continent)) + 
  geom_density()

(p + scale_fill_manual(values=c('red','blue')) + 
    p + scale_fill_manual(values=c('blue','orange')))/(
      p + scale_fill_manual(values=c('#c0feee','#d3d3d3')) +
        p + scale_fill_manual(values=c('black','white')))