# Wyjściówka

# Przedstawic gestosc rozkladu populacji tylko dla Europy i Afryki, gdzie kontynent jest zaznaczony jako wypelnienie (fill).
# Pokazac ten sam wykres z uzyciem czterech roznych palet kolorow.
# Wynikowy wykres pokazac na jednym rysunku (z uzyciem np. grid.arrange).

library('SmarterPoland')
library('patchwork')

countries_AE <- countries %>% filter(continent %in% c("Europe", "Africa"))

(p1 <- ggplot(countries_AE,
       aes(
           x = population,
           fill = continent
       )) + 
    geom_density(alpha=0.7))

(p2 <- p1 + scale_fill_hue(h = c(10, 50)))

(p3 <- p1 + scale_fill_manual(values = c("black", "white")))

(p4 <- p1 + scale_fill_brewer(palette = "Greens"))

p1 + p2 + p3 + p4
