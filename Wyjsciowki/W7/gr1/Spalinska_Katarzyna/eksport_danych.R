library(SmarterPoland)
library(dplyr)

data = countries

data = data %>% 
  rename(
    death_rate = death.rate,
    birth_rate = birth.rate
  )

data_1 <- data %>% group_by(continent) %>% summarise(value = mean(birth_rate,na.rm = TRUE)) 

data_2 <- data %>% group_by(continent) %>% summarise(value = mean(death_rate,na.rm = TRUE)) 

#data <- merge(data_1,data_2)

write.csv(data_1,row.names=FALSE, file = "data1.csv")
write.csv(data_2,row.names=FALSE, file = "data2.csv")
