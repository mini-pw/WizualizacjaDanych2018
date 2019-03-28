library(ggplot2)

ggplot(ChickWeight, aes(x = Time, y = weight)) + 
  geom_point(aes(col = Chick)) + facet_grid(Diet ~ .) + 
  stat_smooth() + 
  theme(legend.position = 'none')
