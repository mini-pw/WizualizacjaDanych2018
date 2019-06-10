require(ggplot2)
require(dplyr)
require(plotrix)
require(SmarterPoland)
require(ggsci)

description <- 'Exploded pie chart in 3D'

population_by_continent <- countries %>%
    select(population, continent) %>%
    group_by(continent) %>%
    summarise(population = round(sum(population) / sum(countries$population) * 100)) %>%
    arrange(desc(population)) %>%
    mutate(continent = factor(continent, levels = continent))

pie3D(population_by_continent$population, labels = population_by_continent$continent, main = "Population percentage by continent",
      explode = 0.1, radius = .9, labelcex = 1.2, height = 0.2, shade=0.5, start = 0, mar=c(0,0,0,6),
      col=pal_jco()(5))
bad_pie_chart_1_plot <- recordPlot()

good_pie_chart_1_plot <- ggplot(data = population_by_continent, aes(x = continent, y = population, fill = continent)) +
    geom_col() +
    geom_text(aes(y = population + 1, label = paste0(population, '%'))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 63), breaks = seq(0, 65, by = 10), labels = function(x) paste0(x, '%')) +
    scale_x_discrete() +
    scale_fill_jco() +
    ggtitle("Population percentage by continent") +
    guides(fill = FALSE) +
    theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.major.x = element_blank()
    )
