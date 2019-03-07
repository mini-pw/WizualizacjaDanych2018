library(dplyr)
library(ggplot2)
library(eurostat)
library(maps)

lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")

names_df <- filter(lp, LEVL_CODE == 0) %>%
  group_by(NUTS_NAME) %>% 
  summarise(long = mean(long),
            lat = mean(lat))

filter(lp, long > -30, lat > 30) %>% 
group_by(NUTS_NAME) %>% 
ggplot(aes(x = long, y = lat, group = group, fill = CNTR_CODE)) + 
  geom_polygon() +
  geom_text(data = names_df, aes(x = long, y = lat, label = NUTS_NAME), inherit.aes = FALSE) +
  coord_map()