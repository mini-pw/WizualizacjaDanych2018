install.packages("BetaBit")
install.packages("dplyr")

library(BetaBit)
library(dplyr)

BetaBit::proton()

head(employees)
employees %>% 
  filter(name == "John" & surname == "Insecure")

head(top1000passwords)
for (pass in top1000passwords){
  proton(action = "login", login="johnins", password=pass)
}

# logs %>%
#   group_by(login) %>%
#   summarise(n = length(login)) %>% 
#   arrange(desc(n))

# logs %>% filter(login == "slap") %>% 
#   group_by(login) %>%
#   summarise(n = length(login)) %>% 
#   arrange(desc(n))

head(logs)
logs %>%
  group_by(host) %>%
  filter(login == "slap") %>% 
  summarise(n = length(host)) %>% 
  arrange(desc(n))

proton(action = "server", host="194.29.178.16")

head(bash_history)
install.packages("stringr")
library(stringr)

unique(word(bash_history))
proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")
