install.packages("ggthemes")
library(ggthemes)
library(patchwork)
density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  theme(legend.position = "none")

(density_death + theme_base()  + density_death + theme_classic() ) / (density_death + theme_dark() + density_death + theme_excel())

