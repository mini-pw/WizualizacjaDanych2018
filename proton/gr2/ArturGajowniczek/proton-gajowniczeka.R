#install.packages("BetaBit")
library(BetaBit)
proton()

library(dplyr)
df_employees <- data.frame(employees)

# Task 1
john_login <- df_employees %>%
  filter(name == 'John' & surname == 'Insecure') %>%
  select(login) %>%
  first()

proton(action = "login", login=john_login)

# Task 2
for(pass in top1000passwords) {
  msg <- proton(action = "login", login=john_login, password=pass)
  if(msg == "Success! User is logged in!") {
    password <- pass
    break
  }
}

# Task 3
df_logs <- data.frame(logs)

pietraszko_login <- df_employees %>%
  filter(surname == "Pietraszko") %>%
  select(login) %>%
  first()

host <- df_logs %>%
  filter(login == pietraszko_login) %>%
  group_by(host) %>%
  summarise(n=n()) %>%
  top_n(n=1) %>%
  select(host)

host <- toString(host$host)
proton(action = "server", host=host)

# Task 4
df_history <- data.frame(bash_history)

possible_passes <- df_history %>%
  extract(bash_history, c("command"), regex = "^(\\w+)$", remove = FALSE) %>%
  na.omit %>%
  select(command)

for(pass in possible_passes$command) {
  msg <- proton(action = "login", login=pietraszko_login, password=pass)
  if(msg == "Success! User is logged in!") {
    password <- pass
    break
  }
}
