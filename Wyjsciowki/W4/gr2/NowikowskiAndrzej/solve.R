library(ggplot2)
library(ggimage)
library(magick)
library(patchwork)
str(ChickWeight)

p1 <- ggplot(data=ChickWeight, aes(x=Time,y=weight, color=Diet,group=Chick)) + 
  geom_point(alpha=0.5) + 
  ylab("Waga kurczaka") +
  xlab("Czas w dniach") +
  geom_line() + 
  facet_wrap(~ Diet)

p2 <- ggplot(data=ChickWeight, aes(x=Time,y=weight, color=Diet)) + 
  geom_smooth(se=TRUE) +
  ylab("Waga kurczaka") +
  xlab("Czas w dniach")

p1 / p2

# kurczak wzięty z
# https://www.flaticon.com/free-icon/chicken_22573