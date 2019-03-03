d1 <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2)+
  theme(legend.position = "none")
(d1 * theme_calc() +d1*theme_classic())/(d1* theme_igray() + d1*theme_excel())
