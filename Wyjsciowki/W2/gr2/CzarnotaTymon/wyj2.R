library(dplyr)
library(ggplot2)
library(eurostat)
library(maps)
library(mapproj)


lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")


t3 <- get_eurostat(s1[1, "code"])

names_df <- filter(lp, LEVL_CODE == 0) %>%
  group_by(NUTS_NAME) %>% 
  summarise(long = mean(long),
            lat = mean(lat))
View(names_df)


filter(lp,long > -30, lat > 30 , LEVL_CODE == 0) %>%
group_by(NUTS_NAME) %>% 
ggplot(aes(x = long, y = lat, group = group, fill = NUTS_NAME)) + 
geom_polygon(color = "black") +
geom_text(data = names_df, aes(x = long, y = lat, label = NUTS_NAME), inherit.aes = FALSE) +
coord_map()
