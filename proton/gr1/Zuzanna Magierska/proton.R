install.packages("BetaBit")
library(BetaBit)
install.packages("dplyr")
library(dplyr)

proton()

# Problem 1
insecure_login <- employees %>% 
  filter(name == "John" & surname == "Insecure") %>% 
  select("login")

proton(action = "login", login=insecure_login)

# Problem 2
for (password in top1000passwords){
  if ("Success! User is logged in!" == proton(action = "login", login=insecure_login, password=password)){
    break
  }
}

# Problem 3
pietraszko_login <- employees %>% 
  filter(name == "Slawomir" & surname == "Pietraszko") %>% 
  select("login")

pietraszko_host <- logs %>% 
  filter(login == pietraszko_login$login) %>% 
  count(host) %>% 
  top_n(1, n) %>% 
  select("host") %>% 
  first() %>% 
  as.character()

proton(action = "server", host=pietraszko_host)

# Problem 4
commands <- unique(gsub(" (.*)", "", bash_history))

proton(action = "login", login=pietraszko_login, password="DHbb7QXppuHnaXGN")
