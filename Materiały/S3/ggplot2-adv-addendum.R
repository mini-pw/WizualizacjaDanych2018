library(ggplot2)
library(SmarterPoland)
library(dplyr)

# warstwy rysunku ---------------------------

ggplot(data = countries, aes(x = continent, fill = continent)) +
  geom_bar()

# kod ponizej nie dziala

ggplot(data = countries, aes(x = continent, fill = continent, label = ..count..)) +
  geom_bar() +
  geom_text()

# stat nie jest dziedziczony, kazda geometria ma przypisany wlasny stat
# dla geom_text domyslny stat to identity

geom_text

ggplot(data = countries, aes(x = continent, fill = continent, label = ..count..)) +
  geom_bar() +
  geom_text(stat = "count")

ggplot(data = countries, aes(x = continent, fill = continent)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count")

ggplot(data = countries, aes(x = continent, fill = continent)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1)

# powloki (layers) ---------------------------------

ggplot(data = countries, aes(x = continent, fill = continent, label = ..count..)) +
  layer(geom = "bar", stat = "count", position = position_dodge(width = 0.5))


ggplot(data = countries, aes(x = continent, fill = continent, label = ..count..)) +
  layer(geom = "bar", stat = "count", position = position_dodge(width = 0.5)) +
  layer(geom = "text", stat = "count", position = position_dodge(width = 0.5))

# rodzaje atrybutow wizualnych

ggplot(countries, aes(x = birth.rate, y = death.rate, shape = continent)) +
  geom_point()

ggplot(countries, aes(x = birth.rate, y = death.rate, shape = continent)) +
  geom_point() +
  scale_shape_manual(values = c(14L:19))

filter(economics_long, variable %in% c("unemploy", "uempmed")) %>% 
  ggplot(aes(x = date, y = value01, linetype = variable)) + 
  geom_line()

filter(economics_long, variable %in% c("unemploy", "uempmed")) %>% 
  ggplot(aes(x = date, y = value01, linetype = variable)) + 
  geom_line() +
  scale_x_date(date_labels = "%Y")
