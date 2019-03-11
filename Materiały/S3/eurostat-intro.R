library(dplyr)
library(ggplot2)
library(eurostat)
library(maps)

lp <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = "all")

table(lp[["CNTR_CODE"]])

ggplot(lp, aes(x = long, y = lat, group = group, fill = CNTR_CODE)) + 
  geom_polygon()

ggplot(lp, aes(x = long, y = lat, group = group, fill = CNTR_CODE)) + 
  geom_polygon() +
  coord_map()

#alterntywnie ggalt::coord_proj

# kraje poza europa
# https://ec.europa.eu/eurostat/documents/345175/501899/NUTS-regions-2015-EU28-CC-EFTA.png

nuts_levels <- lapply(0L:3, function(ith_code) 
  filter(lp, CNTR_CODE == "PL", LEVL_CODE == ith_code) %>% 
    ggplot(aes(x = long, y = lat, group = group, fill = NUTS_NAME)) + 
    geom_polygon(color = "black") +
    ggtitle(paste0("LEVL_CODE = ", ith_code)) +
    coord_map()
)

nuts_levels[[4]]

filter(lp, CNTR_CODE == "PL", LEVL_CODE == 2) %>%
  group_by(NUTS_NAME) %>% 
  mutate(nice_label = c(first(NUTS_NAME), rep("", length(NUTS_NAME) - 1))) %>% 
  ggplot(aes(x = long, y = lat, group = group, 
             fill = NUTS_NAME, label = nice_label)) + 
  geom_polygon(color = "black") +
  geom_text() +
  coord_map()

# najlepsze rozwiazanie (gratulacje dla gr 4)

names_df <- filter(lp, CNTR_CODE == "PL", LEVL_CODE == 3) %>%
  group_by(NUTS_NAME) %>% 
  summarise(long = mean(long),
            lat = mean(lat))

filter(lp, CNTR_CODE == "PL", LEVL_CODE == 3) %>%
  group_by(NUTS_NAME) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = NUTS_NAME)) + 
  geom_polygon(color = "black") +
  geom_text(data = names_df, aes(x = long, y = lat, label = NUTS_NAME), inherit.aes = FALSE) +
  coord_map()

# search eurostat

s1 <- search_eurostat("students", type = "table")

s1

as.list(s1[1, ])

t1 <- get_eurostat(s1[1, "code"])

left_join(lp, t1, by = c("geo" = "geo")) %>% 
  filter(CNTR_CODE == "PL") %>% 
  na.omit %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = values)) + 
  geom_polygon(color = "black") +
  coord_map()

left_join(lp, t1, by = c("geo" = "geo")) %>% 
  filter(CNTR_CODE == "PL") %>% 
  na.omit %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = values)) + 
  geom_polygon(color = "black") +
  coord_map() +
  facet_wrap(~ time)

t3 <- get_eurostat(s1[1, "code"])

left_join(lp, t3, by = c("geo" = "geo")) %>%
  filter(long > -30, lat > 30) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = values)) + 
  geom_polygon(color = "black") +
  coord_map()
