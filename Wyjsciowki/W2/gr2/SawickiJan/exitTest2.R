library(dplyr)
library(ggplot2)
library(eurostat)
library(maps)

lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")

table(lp[["CNTR_CODE"]])

lp=filter(lp,lp$long > -30, lp$lat > 30)

#ggplot(lp,aes(x = long, y = lat, group = group, fill = CNTR_CODE)) + 
#  geom_polygon()
 
names_df <- filter(lp, LEVL_CODE == 1) %>%
  group_by(CNTR_CODE) %>% 
  summarise(long = mean(long),
            lat = mean(lat), nuts_level = length(unique(NUTS_NAME)))

filter(lp, LEVL_CODE == 0) %>%
  group_by(NUTS_NAME) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = NUTS_NAME)) + 
  geom_polygon(color = "black") +
  geom_text(data = names_df, aes(x = long, y = lat, label = paste0(CNTR_CODE," ",nuts_level) ), inherit.aes = FALSE) +
  coord_map()
