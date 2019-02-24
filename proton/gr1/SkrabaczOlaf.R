install.packages("BetaBit")
library(BetaBit)
library(dplyr)
library(magrittr)
library(stringi)
BetaBit::proton()
employees %<>% as_tibble()
employees
employees %>% as_tibble() %>%
  filter(name=="John", surname=="Insecure")
proton(action="login", login="johnins")
d <- sapply(top1000passwords,
       function(x) proton(action="login", login="johnins", password=x))

logs %<>% as_tibble
(logs$login %>% unique)[(grepl("s", logs$login %>% unique))]
proton()
logs_pit
logs_pit <-  logs %>%filter(login=="slap")
logs_pit %>% group_by(host) %>% summarise(count = n()) %>% arrange(desc(count))
proton(action="server",  host = "194.29.178.16", hint=TRUE)

pass <- stri_split(bash_history, fixed=" ", simplify=TRUE)[,1]
pass %>% unique()

proton(action="login", login="slap", password="DHbb7QXppuHnaXGN")

