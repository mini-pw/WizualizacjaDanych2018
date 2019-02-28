library(ggplot2)
library(SmarterPoland)
library(dplyr)

p <- countries %>% 
  filter(continent == 'Asia' | continent == 'Europe') %>%
  ggplot(aes(x = population, fill = continent)) +
  geom_density()

p1 <- p + scale_fill_manual(values = c('orange', 'black'))
p2 <- p + scale_fill_discrete()
p3 <- p + scale_fill_manual(values = c('pink', 'blue'))
p4 <- p + scale_fill_manual(values = c('navyblue', 'grey'))
grid.arrange(p1,p2,p3,p4)

