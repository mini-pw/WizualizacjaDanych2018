library(ggplot2)
library(dplyr)
library(patchwork)

head(airquality)

data= airquality %>% filter(Month == 9)

p1 = ggplot(data = data, aes(x = Day, y=Ozone)) +
  geom_line()
p2 = ggplot(data = data, aes(x = Day, y=Wind)) +
  geom_line()
p1 + p2
