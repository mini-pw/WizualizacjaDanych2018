require(ggplot2)
require(dplyr)
require(SmarterPoland)
require(ggsci)

description <- 'Too many objects on a pie chart'

new_countries <- c('Czech Republic', 'Cyprus', 'Estonia', 'Latvia', 'Lithuania', 'Hungary', 'Malta', 'Poland', 'Slovenia', 'Slovakia')
population_by_country <- countries %>%
    select(country, population) %>%
    filter(country %in% new_countries) %>%
    mutate(population = round(population / sum(population) * 100)) %>%
    arrange(desc(population)) %>%
    mutate(country = factor(country, levels = country))

bad_pie_chart_2_plot <- ggplot(population_by_country, aes(x = "", y = population, fill = country)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    scale_fill_jco() +
    ggtitle("Population of countries which joined Europe in 2004") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 16))

good_pie_chart_2_plot <- ggplot(data = population_by_country, aes(x = country, y = population, fill = country)) +
    geom_col() +
    geom_text(aes(y = population + 1, label = paste0(population, '%'))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 53), labels = function(x) paste0(x, '%')) +
    scale_x_discrete() +
    scale_fill_jco() +
    ggtitle("Population of countries which joined Europe in 2004") +
    guides(fill = FALSE) +
    theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16)
    )
