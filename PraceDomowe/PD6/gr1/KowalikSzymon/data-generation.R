library(dplyr)

# Utworzenie wektorów z danymi, na podstawie których rysowany będzie wykres
jornada <- seq(10, 24)
barcelona <- c(21, 24, 24, 25, 28, 31, 34, 37, 40, 43, 46, 49, 50, 51, 54)
atletico <- c(19, 20, 23, 24, 25, 28, 31, 34, 35, 38, 41, 44, 44, 44, 47)
real <- c(14, 17, 20, 20, 23, 26, 29, 30, 30, 33, 36, 39, 42, 45, 45)

# Zebranie danych w data frame
puntos.data <- data.frame(jornada, barcelona, atletico, real)

data <- rbind(select(puntos.data, day=jornada, points=barcelona) %>% mutate(team='barcelona'),
              select(puntos.data, day=jornada, points=atletico) %>% mutate(team='atletico'),
              select(puntos.data, day=jornada, points=real) %>% mutate(team='real'))

write.csv(data, 'data.csv')
