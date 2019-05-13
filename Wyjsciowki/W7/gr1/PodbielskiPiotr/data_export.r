library(SmarterPoland)
library(dplyr)
my_countries <- countries %>% 
  mutate(birth_rate = birth.rate,
         death_rate = death.rate) %>% 
  group_by(continent) %>%
  summarize(birth_rate = mean(birth_rate, na.rm=TRUE),
            death_rate = mean(death_rate, na.rm=TRUE))
write.table(my_countries, file = "data_countries.tsv", sep = "\t")