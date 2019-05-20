library('SmarterPoland')

data <- countries
names(data) <- c('country', 'birthRate', 'deathRate', 'population', 'continent')

library(dplyr)
data1 <- data %>%
  na.omit() %>%
  group_by(continent) %>%
  summarise(birthRate = mean(birthRate))


data2 <- data %>%
  na.omit() %>%
  group_by(continent) %>%
  summarise(deathRate = mean(deathRate))

names(data2) <- c('group', 'value') 
names(data1) <- c('group', 'value') 

write.csv(data2, "D:\\mgr\\wizualizacja_danych\\WizualizacjaDanych2018\\Wyjsciowki\\W7\\gr1\\KowalskiPatryk\\data2.csv", row.names = FALSE)
write.csv(data1, "D:\\mgr\\wizualizacja_danych\\WizualizacjaDanych2018\\Wyjsciowki\\W7\\gr1\\KowalskiPatryk\\data1.csv", row.names = FALSE)
