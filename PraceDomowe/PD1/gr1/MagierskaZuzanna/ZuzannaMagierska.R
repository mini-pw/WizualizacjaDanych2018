library(dplyr)
library(ggplot2)
library(patchwork)

countries <- c('Estonia', 'The Netherlands', 'Estonia', 'Romania',
               'The Netherlands', 'Poland', 'Slovakia',
               'Spain', 'Slovakia', 'Poland', 'Hungary',
               'Switzerland', 'Hungary', 'Romania', 'Switzerland', 
               'Bulgaria', 'Spain', 'Bulgaria', 'Kosovo',
               'Albania', 'Macedonia', 'Macedonia')

cities <- c('Tallin', 'Amsterdam', 'Tartu', 'Pitesti', 
            'Sint Maartensbrug', 'Warsaw', 'Trencin', 
            'Aviles', 'Bratislava', 'Cracow', 'Sajoszentpeter',
            'Bern', 'Budapest', 'Bucarest', 'Lugano', 
            'Vidin', 'Madrid', 'Sofia', 'Dramjak', 
            'Korce', 'Tetovo', 'Skopje')

days <- c(4, 4, 3, 32, 22, 86, 35, 127, 23, 164, 76, 2, 46,
          41, 9, 166, 20, 71, 67, 61, 293, 162)

annual_concentration <- c(15.7, 22.5, 16.8, 33.9, 27.6, 41.6,
                          29.4, 45.8, 29.1, 56.7, 35.9, 19.3,
                          33.5, 35.5, 19.9, 61.1, 24.0, 40.0,
                          33.7, 40.2, 97.3, 84.1)

smog_data <- data.frame(countries, cities, days, annual_concentration)

countries_order <- group_by(smog_data, countries) %>% 
  summarise(days_sum = mean(days)) %>% 
  arrange(days_sum) %>% 
  pull(countries)

cities_order <- smog_data %>%
  mutate(countries = factor(countries, levels = countries_order)) %>% 
  arrange(desc(countries), desc(days)) %>% 
  pull(cities)

smog_data$cities <- factor(smog_data$cities, levels = cities_order)

days_plot <- ggplot(data = smog_data, aes(x = cities, y = days, fill = countries)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = days), size=3) +
  geom_abline(slope=0, intercept=35,  col = "red", lty = 2) + 
  geom_text(x=19, y=35, label="EU limit of days", vjust = 0, size=3) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

concentration_plot <- ggplot(data = smog_data, aes(x = cities, y = annual_concentration, fill = countries)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = annual_concentration), size=3) +
  geom_abline(slope=0, intercept=35,  col = "red", lty = 2) + 
  geom_text(x=19, y=35, label="EU annual concentration limit", vjust = 0, size=3) +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))

get_legend <- function(gg_plot) {
  grob_table <- ggplotGrob(gg_plot)
  grob_table[["grobs"]][[which(sapply(grob_table[["grobs"]], function(x) x[["name"]]) == "guide-box")]]
}

(days_plot + theme(legend.position = "none")) + get_legend(days_plot) + 
  concentration_plot + plot_spacer() + plot_layout(ncol = 2, heights = c(0.5, 0.5), widths = c(0.8, 0.2))
