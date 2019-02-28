library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(ggthemes)
library(patchwork)

p1 <- ggplot(data = countries, aes(x = death.rate, fill = continent)) + geom_density(alpha = 0.2)

((p1 + theme_solarized() + theme(legend.position = "none")) +
    (p1 + theme_dark()) + theme(legend.position = "none")) / ((p1 + theme_classic()) +
         (p1 + theme_excel() + theme(legend.position = "none")))
