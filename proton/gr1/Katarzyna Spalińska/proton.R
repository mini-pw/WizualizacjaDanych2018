library(dplyr)
library(BetaBit)
proton()

------

john_login <- employees[which(employees$surname=="Insecure"), 3]
pietraszko_login <- employees[which(employees$surname=="Pietraszko"), 3]

proton(action = "login", login=john_login)

------

for (element in top1000passwords) {
  result_msg <- proton(action = "login", login=john_login, password=element)
  if (identical(all.equal(result_msg, "Success! User is logged in!"), TRUE)) {
    weak_pwd <- element
    break
  }
}

------
  
pietraszko_hosts <- logs %>% filter(login==pietraszko_login) %>% group_by(host) %>% summarise(Count = n())
pietraszko_fav_host <- pietraszko_hosts[which(pietraszko_hosts$Count == max(pietraszko_hosts$Count)),1]
proton(action = "server", host="194.29.178.16")

------

commands <- gsub( " .*$", "", bash_history )
bash_history_df <- as.data.frame(commands)

bash_history_df %>% group_by(commands) %>% summarise()

proton(action = "login", login=pietraszko_login, password="DHbb7QXppuHnaXGN")



