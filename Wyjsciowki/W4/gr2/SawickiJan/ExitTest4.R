library(ggplot2)

ChickWeight

ggplot(data = ChickWeight, aes(
  x = Time,
  group = ChickWeight$Chick,
  colour = Diet
)) +
  geom_line(aes(y = weight)) +
  geom_point(aes(y = weight)) +
  facet_wrap(ChickWeight$Diet) +
  ylab("Weight") + 
  ggtitle("Chicken weight with diet") +
  scale_color_manual(values = c("#ECA237", "#D44F22", "#A13C1A", "#522402")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        )