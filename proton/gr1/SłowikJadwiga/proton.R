install.packages("BetaBit")
install.packages("stringi")
library(BetaBit)
library(dplyr)
library(stringi)

proton()

get_login <- function(employee_name, employee_surname) {
  employees %>%
    filter(name == employee_name, surname == employee_surname) %>% 
    select(login) %>% 
    simplify2array(.)
}

john_insecure_login <- get_login("John", "Insecure") # johnins
proton(action = "login", login=john_insecure_login)

john_insecure_password <- sapply(top1000passwords, function(x) proton(action = "login", login = john_insecure_login, password = x)) %>% 
  .[stri_detect(., regex='Success! User is logged in!')] %>% 
  names(.) %>% 
  stri_extract_first(., regex=c("[^.]+")) # q1w2e3r4t5

pietraszko_login <- get_login("Slawomir", "Pietraszko") # slap
most_frequent_host <- logs %>%
  filter(login == pietraszko_login) %>% 
  group_by(host) %>% 
  summarize(cnt=n()) %>% 
  arrange(desc(cnt)) %>% 
  top_n(1) %>% 
  select(host) %>% 
  simplify2array(.) %>%
  as.character(.) # 194.29.178.16
proton(action = "server", host=most_frequent_host)

bash_history %>% 
  stri_extract_first(., regex="[^ ]*") %>% 
  data.frame(keyword=.) %>% 
  distinct(.)

pietraszko_password <- "DHbb7QXppuHnaXGN"
proton(action="login", login=pietraszko_login, password=pietraszko_password)
