# data source: http://stat.gov.pl/obszary-tematyczne/kultura-turystyka-sport/turystyka/turystyka-w-unii-europejskiej-dane-za-2017-rok,11,4.html

#libraries

library(dplyr)
library(ggplot2)
library(ggrepel)
source("https://install-github.me/thomasp85/patchwork")
library(patchwork)

#data preparation

df1 <- read.csv(file.path(dirname(sys.frame(1)$ofile), 'number_of_objects.csv'), header = TRUE) # absolute path to '*.csv' files required
df2 <- read.csv(file.path(dirname(sys.frame(1)$ofile), 'number_of_beds.csv'), header = TRUE) # absolute path to '*.csv' files required

source.data <- data.frame(Countries = df1$Countries, Number.of.objects = df1$X2017, Number.of.beds = df2$X2017) %>%
  filter(Number.of.objects != '.', Number.of.beds != '.') %>%
  mutate(Countries = as.character(Countries), Number.of.beds = as.numeric(gsub(',','',Number.of.beds)), Number.of.objects = as.numeric(levels(factor(Number.of.objects)))[as.integer(factor(Number.of.objects))]) %>%
  na.omit()
source.data <- source.data %>%
  mutate(Average.number.of.beds = Number.of.beds / Number.of.objects, Label.for.plot = ifelse(Countries == 'Poland' | Number.of.objects %in% range(Number.of.objects) | Average.number.of.beds %in% range(Average.number.of.beds), Countries, ''))

#displaying data

p1 <- ggplot(source.data, aes(x = Number.of.objects, y = Average.number.of.beds, label = Label.for.plot, color = ifelse(Label.for.plot == '', NA, 'red'))) +
  geom_point() +
  geom_label_repel() +
  labs(title = 'Average number of beds per object to number of touristic objects in countries of EU in the year 2017') +
  theme(legend.position = "none")

p2 <- ggplot(source.data) +
  geom_density(aes(x = Average.number.of.beds)) +
  coord_flip() +
  scale_y_reverse() +
  ylab('Density')

p3 <- ggplot(source.data) +
  geom_density(aes(x = Number.of.objects)) +
  ylab('Density')

plot <- p1 + p2 + p3 + plot_spacer() +
  plot_layout(ncol = 2, heights = c(0.7, 0.3), widths = c(0.7, 0.3))

print(plot)
