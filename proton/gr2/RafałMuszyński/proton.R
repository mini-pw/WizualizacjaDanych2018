# install.packages('BetaBit')
library(BetaBit)
proton()
insecure.login <- employees[which(employees$surname == 'Insecure' & employees$name == 'John'),]$login
proton(action = 'login', login = insecure.login)

for( password in top1000passwords){
  msg <- proton(action = 'login', login = insecure.login, password = password)
  if( msg == 'Success! User is logged in!'){
    insecure.password <- password
    break
  }
}

# install.packages('dplyr')
library(dplyr)
pietraszko.login <- employees[which(employees$surname == 'Pietraszko' & employees$name == 'Slawomir'),]$login
hostsWithCounts <- logs %>% 
  filter(login == pietraszko.login) %>%
  count(host, sort = TRUE)

usualHost <- as.character(hostsWithCounts$host[1])
proton(action = 'server', host = usualHost)

library(stringr)
possiblePasswords <- unique(word(bash_history))
for( password in possiblePasswords){
  msg <- proton(action = 'login', login = pietraszko.login, password = password)
  if( msg == 'Success! User is logged in!'){
    pietraszko.password <- password
    break
  }
}