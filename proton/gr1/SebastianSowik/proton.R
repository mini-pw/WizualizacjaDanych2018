install.packages("BetaBit")
library(BetaBit)
proton()

library(dplyr)

# Problem 1

insecure_login = employees %>% 
  filter(surname == "Insecure" & name == "John") %>% 
  select(login) %>% 
  first() 

proton(action="login", login = insecure_login)

# Problem 2

head(top1000passwords)

successfull_login_message <- 'Success! User is logged in!'

login_successful <- FALSE

for(p in top1000passwords){
  response <- proton(action="login", login=insecure_login, password=p)
  if (response == successfull_login_message) {
    password <- p
    break;
  }
}

# Problem 3

head(logs)

pietraszko_login <- employees %>% 
  filter(surname == "Pietraszko" & name == "Slawomir") %>% 
  select(login) %>% 
  first()

pietraszko_favorite_host <- logs %>% 
  group_by(login, host) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n), .by_group=TRUE) %>% 
  filter(login==pietraszko_login) %>% 
  top_n(1) %>% 
  ungroup() %>% 
  select(host) %>% 
  first() %>% 
  as.character()


proton(action="server", host=pietraszko_favorite_host)

# Problem 4

head(bash_history)

pass <- bash_history[grepl("^[[:alnum:]]{6}[[:alnum:]]+$", x=bash_history)] 

proton(action="login", login=pietraszkos_login, password=pass)
