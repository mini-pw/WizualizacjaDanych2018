library(SmarterPoland)
library(dplyr)

df <- SmarterPoland::countries %>% rename(birthRate = birth.rate, deathRate = death.rate)
#write.table(df, "countries.tsv", sep = "\t", row.names = FALSE)
df %>% group_by(continent) %>% summarise(mean(na.omit(birthRate)))
df %>% group_by(continent) %>% summarise(mean(na.omit(deathRate)))
