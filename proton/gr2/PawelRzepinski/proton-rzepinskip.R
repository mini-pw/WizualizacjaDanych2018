install.packages("BetaBit")
library(BetaBit)
proton()

library(dplyr)
library(stringr)

# Step 1
weakspot_login <- employees %>% filter(name == "John" & surname == "Insecure") %>% select(login) %>% first()

proton(action = "login", login=weakspot_login)

# Step 2
for (pass in top1000passwords) {
  response <- proton(action = "login", login=weakspot_login, password=pass)
  if (response == "Success! User is logged in!") {
    weakspot_pass <- pass
    break
  }
}

# Step 3
pietraszko_login <- employees %>% filter(surname == "Pietraszko") %>% select(login) %>% first()
pietraszko_popular_host <- logs %>% 
  filter(login == pietraszko_login) %>% 
  count(host, sort=TRUE) %>% top_n(1) %>% select(host) %>% first()

proton(action = "server", host="194.29.178.16")

#Step 4
bash_history_df <- as.data.frame(bash_history)
bash_history_df %>% mutate(command = str_extract(bash_history, "([^ ]+)")) %>% distinct(command)

proton(action = "login", login=pietraszko_login, password="DHbb7QXppuHnaXGN")
