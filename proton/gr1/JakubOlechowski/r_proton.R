install.packages("BetaBit")
library(BetaBit)
proton()

# Problem 1 - finding John Insecure's login

library(dplyr)

filtered.rows <- employees %>% 
  filter(name == 'John', surname == 'Insecure')

johnlogin <- filtered.rows %>% 
  top_n(1, login) %>% 
  select(login) %>% 
  as.character()

proton(action = 'login', login = johnlogin)

# Problem 2 - finding John Insecure's password

for (pass in top1000passwords) {
  res <- proton(action = 'login', login = johnlogin, password = pass)
  if (!grepl('.*incorrect.*', res)) {
    break
  }
}

# Problem 3 - finding most often logged server

pietraszko.login <- employees %>% 
  filter(name == 'Slawomir', surname == 'Pietraszko') %>% 
  top_n(1, login) %>% 
  select(login) %>% 
  as.character()

often.visited.host <- logs %>% 
  filter(login == pietraszko.login) %>% 
  group_by(host) %>% 
  summarise(count = length(host)) %>% 
  filter(count == max(count)) %>% 
  top_n(1, count)

proton(action = 'server', host = as.character(often.visited.host[['host']]))

# Problem 4 - finding the Pietraszko's password

tmp <- bash_history[!grepl('^[a-z]+ .*', bash_history)] %>% 
  as.factor()
print(levels(tmp)) # I clearly see a somewhat different string in index 1.

probably.password <- levels(tmp)[1]

proton(action = 'login', login = pietraszko.login, password = probably.password)
