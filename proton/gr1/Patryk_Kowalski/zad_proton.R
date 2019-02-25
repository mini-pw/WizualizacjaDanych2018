#Patryk Kowalski
#270628
library(BetaBit)
proton()

library(dplyr)
library(tidyverse)
login <- employees %>% 
  filter(name == 'John', surname == "Insecure") %>% 
  select(login)
proton(action = "login", login = login)

passVector <- sapply(top1000passwords, function(i) proton(action = "login", login = login, password = i))
passId <- passVector %>% match( "Success! User is logged in!",.)
password <- top1000passwords[passId]

login <- employees %>% 
  filter(surname == "Pietraszko") %>% 
  select(login)

host <- logs %>%
  filter(login == 'slap') %>% 
  group_by(host) %>% 
  summarise(number = n()) %>% 
  filter(number == max(number)) %>% 
  select(host)

proton(action = "server", host=(as.character(host[[1]])))

bash <- tibble(id = 1:length(bash_history), complex_command = bash_history)
bash %<>% 
  rowwise() %>% 
  mutate(commands = strsplit(complex_command, " ")[[1]][1]) 
commands <- tibble(id = 1:length(unique(bash$commands)), commands = unique(bash$commands)) %>%
  mutate(length = nchar(commands))
password <- commands[which.max(commands$length),"commands"]

proton(action = "login", login = login, password = as.character(password))

