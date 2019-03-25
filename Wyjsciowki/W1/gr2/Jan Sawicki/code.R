library(ggplot2)
library(SmarterPoland)
library(dplyr)

Euro_afr=filter(countries,countries$continent == "Africa" | countries$continent == "Europe")
countries[countries$continent=="Europe", countries$continent=="Africa",]

p1 <- ggplot(data = Euro_afr, aes(x = continent, fill = continent), colors="grey") +
  geom_density()+ 
  scale_fill_manual(values = c("red", "grey", "black", "navyblue", "green"))
p2 <- ggplot(data = Euro_afr, aes(x = continent, fill = continent), colors="grey") +
  geom_density()+ 
  scale_fill_manual(values = c("red", "blue", "black", "navyblue", "green"))
p3 <- ggplot(data = Euro_afr, aes(x = continent, fill = continent), colors="grey") +
  geom_density()+ 
  scale_fill_manual(values = c("red", "white", "black", "navyblue", "green"))
p4 <- ggplot(data = Euro_afr, aes(x = continent, fill = continent), colors="grey") +
  geom_density()+ 
  scale_fill_manual(values = c("green", "navyblue", "black", "navyblue", "green"))

grid.arrange(p1,p2,p3,p4)
