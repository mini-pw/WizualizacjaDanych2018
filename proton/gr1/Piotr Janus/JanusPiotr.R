install.packages("BetaBit")
library(BetaBit)
proton()

#Problem 1
john_login <- employees[employees$name == "John" & employees$surname  == "Insecure",'login']
proton(action = "login", login = john_login)


#Problem 2
lapply(top1000passwords, function(pass) proton(action = 'login', login = john_login, password = pass))

#Problem 3
library(dplyr)

logi <- logs %>% group_by(host) %>%  summarise(count = n()) %>%  arrange(desc(count))
head(logi)

proton(action = 'server', host =  "194.29.178.16")

#Problem 4
pietr_pass <- data.frame(table(gsub(" .*$","", bash_history)))
pietr_pass <- as.character(pietr_pass[pietr_pass$Freq<2,1])
pietr_login <- employees[employees$name == "Slawomir" & employees$surname  == "Pietraszko",'login']
proton(action = "login", login = pietr_login, password = pietr_pass)
