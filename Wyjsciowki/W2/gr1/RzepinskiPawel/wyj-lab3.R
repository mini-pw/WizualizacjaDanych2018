library(dplyr)
library(ggplot2)
library(eurostat)
library(mapproj)

lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")
s1 <- search_eurostat("Curative", type = "table")
t3 <- get_eurostat(s1[1, "code"])

left_join(lp, t3, by = c("geo" = "geo")) %>%
  filter(long > -30, lat > 30) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = values)) + 
  geom_polygon(color = "black") +
  coord_map() +
  geom_text()

names_df2 <- filter(lp, LEVL_CODE == 1, long > -30, lat > 30) %>%
  group_by(CNTR_CODE) %>% 
  summarise(long = mean(long),
            lat = mean(lat),
            counter = n_distinct(NUTS_NAME))

filter(lp, LEVL_CODE == 0, long > -30, lat > 30) %>%
  group_by(CNTR_CODE) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = CNTR_CODE)) + 
  geom_polygon(color = "black") +
  geom_text(data = names_df2, aes(x = long, y = lat, label = paste0(CNTR_CODE, " (", counter, ")")), inherit.aes = FALSE) +
  coord_map()
