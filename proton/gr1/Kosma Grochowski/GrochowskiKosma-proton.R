install.packages("proton")
library(proton)
proton()
install.packages("dplyr")
library(dplyr)
emps <-data.frame(employees)
targetRow <- employees %>% 
  filter(surname == "Insecure")
login <- targetRow['login']
paste(login, 's', sep=" ") 

proton(action = "login", login=login)

passwords <- data.frame(top1000passwords)

for(i in 1:nrow(passwords)){
  password <-toString( passwords[i,1])
  if(!grepl("incorrect", proton(action = "login", login=login, password=password)))
  {
    print(password)
    break
  }
}

logs <- data.frame(logs)
goalRow <- employees %>% 
  filter(surname == "Pietraszko")
goalLogin <- toString(goalRow['login'])
maxHost <-  logs  %>% 
  filter(login == goalLogin) %>% 
  group_by(host) %>% 
  summarise(n = length(host))%>% 
  filter(n == max(n))
maxHost <-  (maxHost["host"])[[1]]
maxHost = toString(maxHost)
proton(action="server", host = maxHost)
history <- data.frame(bash_history)
install.packages("stringr")

library(stringr)
history$bash_first <- word(history$bash_history,1)
goalPwd <- history %>% group_by(bash_first)%>% 
  summarise(n = length(bash_first))%>% 
  filter(n == min(n))
goalPwd <-  toString(goalPwd["bash_first"])[[1]]
proton(action = "login", login=goalLogin, password=goalPwd)

