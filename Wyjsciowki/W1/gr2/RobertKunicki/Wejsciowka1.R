library(ggplot2)
library(SmarterPoland)
library(dplyr)

head(countries)
countries = countries %>% filter(continent == "Africa" | continent=="Europe") 

head(countries)

populacja <- ggplot(data = na.omit(countries), aes(x = population, fill = continent)) + geom_density(alpha = 0.2) + coord_flip() + theme(legend.position = "none")

grid.arrange(populacja + scale_fill_manual(values = c("red", "grey")), populacja + scale_fill_manual(values = c("red", "black")), populacja + scale_fill_manual(values = c("red", "grey")), populacja + scale_fill_manual(values = c("green", "grey")),
             ncol = 2, heights = c(0.5, 0.5), widths = c(0.5, 0.5))


