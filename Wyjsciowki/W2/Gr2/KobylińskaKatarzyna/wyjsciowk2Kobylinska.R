#Narysowac mape Europy (filter(long > -30, lat > 30)) z wyszczegolnionymi krajami, 
#podpisac kazdy kraj i w nawiasie napisac ile ma nuts poziomu 1, np "PL 7".

library(dplyr)
library(ggplot2)
library(eurostat)
library(maps)


lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")


names_df <- filter(lp, LEVL_CODE == 0) %>%
  group_by(CNTR_CODE) %>% 
  summarise(long = mean(long),
            lat = mean(lat))


library(dplyr)
library(ggplot2)
library(eurostat)
library(maps)


lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")


names_df <- filter(lp, LEVL_CODE == 1,long >-30, lat > 30) %>%
  group_by(CNTR_CODE) %>% 
  summarise(long = mean(long),
            lat = mean(lat))


filter(lp, long >-30, lat > 30) %>% 
  ggplot(aes(x = long, y = lat, group = group, 
             fill = CNTR_CODE)) + 
  geom_polygon(color = "black") +
  geom_text(data = names_df, aes(x = long, y = lat, label = CNTR_CODE), inherit.aes = FALSE) +
  coord_map() 

  

