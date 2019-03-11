library(dplyr)
library(ggplot2)
library(eurostat)
library(maps)
lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")

head(lp)

num.nuts <- lp %>%
  filter(long > -30, lat > 30) %>% 
  filter(LEVL_CODE == 1) %>% 
  group_by(CNTR_CODE) %>% 
  summarise(cx = mean(long), cy = mean(lat), num_nuts=length(unique(NUTS_ID)))

lp %>% 
  filter(long > -30, lat > 30) %>% 
  filter(LEVL_CODE == 0) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = NUTS_NAME)) + 
  geom_polygon(color = "black") +
  geom_text(aes(x = cx, y = cy, label = paste(CNTR_CODE, num_nuts)), num.nuts, size=2, inherit.aes = FALSE) +
  coord_map()
