#install.packages("proton")
library(proton)
proton()
head(employees)
library(dplyr)
employees %>% filter(name == "John" & surname == "Insecure")
proton(action = "login", login = "johnins")
top1000passwords

for ( i in 1:length(top1000passwords))
{
  if (proton(action = "login", login = "johnins", password = top1000passwords[i]) == "Success! User is logged in!")
  {
    pass = top1000passwords[i]
    break
  }
}

head(logs)
logs %>% group_by(host) %>% summarize(n = n()) %>% arrange(desc(n)) 
proton(action = "server", host = "194.29.178.91")
proton(action = "server", host = "193.0.96.13.15")
proton(action = "server", host = "194.29.178.16")
logs %>% filter(host == "194.29.178.16") %>% group_by(login) %>% summarise(n = n())

bash_history
head(bash_history)

tab <- bash_history %>% as.data.frame() %>%
  apply(MARGIN = 1, function(x) { strsplit(x, " ")[[1]][1]}) %>% as.data.frame() 
colnames(tab) <- c("pass")
tab %>% group_by(pass) %>% summarise(n = n())
logs %>% filter(host == "194.29.178.16") %>% group_by(login) %>% summarise(n = n())
proton(action = "login", login = "slap", password = "DHbb7QXppuHnaXGN")
