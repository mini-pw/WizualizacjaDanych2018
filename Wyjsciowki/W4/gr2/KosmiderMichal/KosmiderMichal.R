library(ggplot2)

ggplot(ChickWeight, aes(x = Time, y = weight)) + 
  geom_point(aes(col = Chick)) + 
  facet_grid(Diet ~ ., labeller = label_both) + 
  stat_smooth(se = FALSE) + 
  theme(legend.position = 'none')
