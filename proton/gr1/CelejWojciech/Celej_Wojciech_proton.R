library(BetaBit)
library(dplyr)
proton()

# Problem 1

employees
employees %>% 
  filter(name == "John", surname == "Insecure") %>% 
  select(login) %>% 
  as.character() -> johnsLogin

proton(action = "login", login = johnsLogin)  

# Problem 2

for (passwd in top1000passwords) {
  proton(action = "login", login = johnsLogin, password = passwd)
}

# Problem 3

employees %>% 
  filter(name == "Slawomir", surname == "Pietraszko") %>% 
  select(login) %>% 
    as.character() -> pietraszkoLogin

logs %>% 
  filter(login == pietraszkoLogin) %>% 
  group_by(login, host) %>% 
  summarise(number = n()) -> temp

host1 <- temp[which.max(temp$number), "host"][[1]] %>% as.character()

proton(action = "server", host = host1)

# Problem 4  

sapply(bash_history, function (x) strsplit(x, split = "\\s")[[1]][1]) %>% 
  as.data.frame() -> temp

colnames(temp)[1] <- "command"
temp %>% 
  group_by(command) %>% 
  summarise(count = n()) -> temp

pietraszkoPasswd <- temp[which.min(temp$count), "command"][[1]] %>% as.character()

proton(action = "login", login = pietraszkoLogin, password = pietraszkoPasswd)
