library(eurostat)
library(ggplot2)
library(dplyr)
library(maps)

lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")

europe_lp <- lp %>% 
  filter(long > -30 & lat > 30 & LEVL_CODE %in% c(0, 1))

names_df <- filter(lp, LEVL_CODE == 1) %>%
  group_by(CNTR_CODE) %>% 
  summarise(long = mean(long),
            lat = mean(lat),
            nuts1_count = n_distinct(NUTS_NAME))

europe_lp %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = CNTR_CODE)) + 
  geom_polygon() +
  geom_text(data = names_df, aes(x = long, y = lat, label = sprintf("%s, (%s)", CNTR_CODE, nuts1_count)), inherit.aes = FALSE) +
  coord_map()
