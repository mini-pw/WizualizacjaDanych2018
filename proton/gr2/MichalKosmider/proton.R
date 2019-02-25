# install.packages("BetaBit")
library(BetaBit)
proton()

johns_login = employees[employees$name == 'John' & employees$surname == 'Insecure', 'login']
proton(action = "login", login=johns_login)

for (password in top1000passwords) {
  response <- proton(action = "login", login=johns_login, password=password)
  if (response == 'Success! User is logged in!') {
    johns_password <- password
    print(paste('Password is ', johns_password))
    break
  }
}

# install.packages("dplyr")
library(dplyr)

pietraszko_login <- employees[employees$surname == 'Pietraszko', 'login']

pietraszko_host <- as.character(logs %>% 
  filter(login == pietraszko_login) %>%
  group_by(host) %>% 
  tally() %>% 
  top_n(n=1, wt=n) %>% 
  select(host) %>% 
  first())

proton(action='server', host=pietraszko_host)

# install.packages("tidyr")
library('tidyr')

words <- data.frame(bash_history) %>% separate(col = 1, into = 'pass', extra = 'drop') %>% unique()
pietraszko_password <- 'DHbb7QXppuHnaXGN'

proton(action = "login", login=pietraszko_login, password=pietraszko_password)
