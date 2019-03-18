library(dplyr)
library(ggplot2)

dat <- data.frame(variable = c("Liczba studentów", "Liczba pracowników"),
                  value = c(1000, 153))

p <- ggplot(dat, aes(x = variable, y = value)) +
  geom_col() + 
  theme_bw() +
  scale_x_discrete("") +
  scale_y_continuous("")

# obrazki rasterowe w ggplot2 ---------------------------

library(magick)

mini <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/a/ae/Gmach_Wydzia%C5%82u_MiNI.jpg/467px-Gmach_Wydzia%C5%82u_MiNI.jpg")

mini_mini <- mini %>%
  image_scale("140") %>% 
  image_border("grey", "50x50") %>%
  image_annotate("MiNI", color = "black", size = 40, 
                 location = "+0+10", gravity = "north")

p + annotation_raster(as.raster(mini_mini), 0.75, 1.25, 400, 800)

# obrazki w geometriach ------------------------------

library(ggimage)

dat <- data.frame(kraj = c("Niemcy", "Polska", "Dania"),
                  populacja = c(82793800, 38433600, 5749000),
                  flaga = c("https://upload.wikimedia.org/wikipedia/commons/b/ba/Flag_of_Germany.svg",
                            "https://upload.wikimedia.org/wikipedia/en/1/12/Flag_of_Poland.svg",
                            "https://upload.wikimedia.org/wikipedia/commons/9/9c/Flag_of_Denmark.svg"))

ggplot(dat, aes(x = kraj, y = populacja, image = flaga)) + 
  geom_image(size = .05) 

# waffle ----------------------
# devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
library(emojifont)  
library(dplyr)
library(SmarterPoland)

mutate(countries, continent = as.character(continent)) %>% 
  waffle_iron(aes_d(group = continent)) %>% 
  ggplot(aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() + 
  scale_fill_waffle() + 
  theme_waffle()

# alternatywa: 
# https://github.com/hrbrmstr/waffle

# praca z inkscape: przygotowanie obrazka ----------------------

library(SmarterPoland)
library(patchwork)
library(latex2exp)

p1 <- ggplot(data = countries, aes(x = continent, y = death.rate, color = continent)) +
  geom_boxplot() +
  scale_x_discrete("ĄĘŻŹĆ") +
  scale_y_continuous(TeX("$\\frac{\\alpha}{\\beta \\times \\log 10}$"))

set.seed(1410)
p2 <- ggplot(data = countries, aes(x = continent, y = death.rate, color = continent)) +
  geom_point(position = "jitter") 

p3 <- ggplot(data = countries, aes(x = continent, fill = continent)) +
  geom_bar()


cairo_ps("learning-inkscape.eps", height = 7.5, width = 8)
((p1 + p2) / p3) * theme_bw() * theme(legend.background = element_rect(fill = "green"))
dev.off()

# alternatywa: eksport do svg pakietem gridSVG

library(gridSVG)

svg("learning-inkscape.svg", height = 7.5, width = 8)
((p1 + p2) / p3) * theme_bw() * theme(legend.background = element_rect(fill = "green"))
dev.off()
