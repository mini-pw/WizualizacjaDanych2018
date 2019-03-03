install.packages("BetaBit")
library(BetaBit)
install.packages("dplyr")
library(dplyr)

proton()

john <- employees %>% 
  filter(name == "John" & surname == "Insecure")

proton(action = "login", login = john$login)

for(pass in top1000passwords) {
  loginResult = proton(action = "login", login = john$login, password = pass)
  
  if(loginResult == "Success! User is logged in!") {
    break;
  }
}

pietraszko <- employees %>% 
  filter(name == "Slawomir" & surname == "Pietraszko")

pietraszakoHosts <- logs %>% 
  filter(login == pietraszko$login) %>% 
  group_by(host) %>% 
  summarise(timesLogged = length(host)) %>% 
  arrange(desc(timesLogged))

pietraszakoMainHost <- toString(pietraszakoHosts$host[[1]])

proton(action = "server", host=pietraszakoMainHost)

commands <- strsplit(bash_history, ' ')

for(command in commands) {
  if(length(command) < 2 & nchar(command[1]) > 6)
    pietraszkoPass <- command[1]
}

proton(action = "login", login = pietraszko$login, password = pietraszkoPass)
