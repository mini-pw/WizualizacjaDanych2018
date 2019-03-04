library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(grid)
library(gridExtra)
library(ggthemes)

density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
   geom_density(alpha = 0.2) +
   theme_bw(base_size = 14) +
   scale_color_brewer(palette="Dark2")

get_legend <- function(gg_plot) 
{
   grob_table <- ggplotGrob(gg_plot)
   grob_table[["grobs"]][[which(sapply(grob_table[["grobs"]], function(x) x[["name"]]) == "guide-box")]]
}
grid.arrange(density_death + theme_wsj() + theme(legend.position = "none"), 
             density_death + theme_tufte() + theme(legend.position = "none"),
             get_legend(density_death),
             density_death + theme_excel() + theme(legend.position = "none"), 
             density_death + theme_economist() + theme(legend.position = "none"),
             ncol = 3, nrow = 2, widths = c(0.3, 0.3, 0.2))
