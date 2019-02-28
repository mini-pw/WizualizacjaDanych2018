install.packages("BetaBit")
library(BetaBit)
proton()
head(employees)
install.packages("dplyr")
library(dplyr)
record<-filter(employees,name=="John" & surname=="Insecure")
login_john<-select(record, login)
login_john
proton(action="login", login=login_john)
head(top1000passwords)
for (pass in top1000passwords){
  proton(action="login", login=login_john, password=pass)
}

head(logs)
popular_login<-logs %>% group_by(login) %>% summarise(n = length(login)) %>%slice(which.max(n)) %>% select(login)
popular_login
common_host<-logs %>% filter(login=="slap") %>% group_by(host)%>% summarise(n = length(host))%>%slice(which.max(n))%>% select(host)
common_host
proton(action = "server", host="194.29.178.16")

head(bash_history)
install.packages("stringr")
library(stringr)
separated<-word(bash_history)
unique(separated)
proton(action = "login", login=popular_login, password="DHbb7QXppuHnaXGN")
