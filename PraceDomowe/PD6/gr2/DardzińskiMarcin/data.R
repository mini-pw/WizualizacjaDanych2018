library(reshape2)

jornada <- seq(10, 24)
barcelona <- c(21, 24, 24, 25, 28, 31, 34, 37, 40, 43, 46, 49, 50, 51, 54)
atletico <- c(19, 20, 23, 24, 25, 28, 31, 34, 35, 38, 41, 44, 44, 44, 47)
real <- c(14, 17, 20, 20, 23, 26, 29, 30, 30, 33, 36, 39, 42, 45, 45)

puntos.data <- data.frame(jornada, barcelona, atletico, real)

puntos.long <- melt(puntos.data, id = "jornada", measure = c("barcelona", "atletico", "real"))
colnames(puntos.long) <-  c('day', 'team', 'score')

write.csv(puntos.long, file = 'data.csv', row.names = F)
