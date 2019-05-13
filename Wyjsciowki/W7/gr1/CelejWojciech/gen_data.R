library(dplyr)
library(SmarterPoland)


data <- countries %>%
  select(-population) %>% 
  na.omit() %>% 
  group_by(continent) %>% 
  summarise(mean_birth_rate = mean(birth.rate),
            mean_death_rate = mean(death.rate))

write.table(data, file = "data.tsv", row.names = FALSE, sep = "\t", col.names = TRUE, quote = FALSE)
