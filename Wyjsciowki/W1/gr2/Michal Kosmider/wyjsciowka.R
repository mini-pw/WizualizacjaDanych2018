library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(RColorBrewer)
library(patchwork)

data <- countries %>% filter(continent == 'Europe' | continent == 'Africa') %>% na.omit()

a <- ggplot(data, aes(x = population, fill = continent)) + 
  geom_density(alpha = 0.2) + scale_fill_manual(values = c("red", "black"))

b <- ggplot(data, aes(x = population, fill = continent)) + 
  geom_density(alpha = 0.2) + scale_fill_manual(values = c("orange", "blue"))
c <- ggplot(data, aes(x = population, fill = continent)) + 
  geom_density(alpha = 0.2) + scale_fill_manual(values = c("green", "yellow"))
d <- ggplot(data, aes(x = population, fill = continent)) + 
  geom_density(alpha = 0.2) + scale_fill_manual(values = c("grey", "magenta"))

(a + b) / (c + d) & theme_dark()
