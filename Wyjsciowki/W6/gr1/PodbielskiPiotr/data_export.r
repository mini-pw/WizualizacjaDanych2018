library(SmarterPoland)
library(dplyr)
my_countries <- countries %>% 
  mutate(birth_rate = birth.rate,
         death_rate = death.rate) %>% 
  select(birth_rate, death_rate, continent)
write.table(my_countries, file = "data_countries.tsv", sep = "\t")