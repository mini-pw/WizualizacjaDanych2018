library(gridExtra)


# gestosc rozkladu smiertelnosci w podziale na kontynenty
density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2)

# wynikowy wykres z uzyciem czterech tematów z pakietu ggthemes
grid.arrange(density_death + theme_base(),
             density_death + theme_economist() + theme(legend.position = "none"),
             density_death + theme_dark() + theme(legend.position = "none"),
             density_death + theme_excel() + theme(legend.position = "none"),
             ncol = 2)
