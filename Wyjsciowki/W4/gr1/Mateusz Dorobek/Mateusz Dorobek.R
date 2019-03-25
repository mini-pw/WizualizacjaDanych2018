head(airquality)
library(ggplot2)
library(dplyr)

filter(airquality, Month == 5) %>% 
  ggplot(aes(x = Day, y = Ozone)) +
  geom_line()

library(reshape2)

p <- filter(airquality, Month == 5) %>% 
  select(Day, Ozone, Wind) %>% 
  mutate(Day = factor(Day)) %>% 
  melt %>% 
  ggplot(aes(x = Day, y = value, col=variable)) +
  geom_point()

svg("Mateusz_Dorobek.svg", height = 7.5, width = 8)
p
dev.off()
