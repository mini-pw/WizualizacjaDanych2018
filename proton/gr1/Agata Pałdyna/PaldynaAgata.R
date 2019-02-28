#-----------------------------------------------

# Laboratoria nr 1
# 18.02.2019

#-----------------------------------------------

# Gra

#-----------------------------------------------

## Instalacja i wczytanie pakietów

install.packages("BetaBit")
BetaBit::proton()

library(BetaBit)
library(dplyr)
library(stringi)


## Problem 1

JI_login <- BetaBit::employees %>%
  filter(name == "John" & surname == "Insecure") %>%
  select(login)

JI_login

BetaBit::proton(action = "login", login = JI_login)


## Problem 2

pass_vect <- BetaBit::top1000passwords
pass_vect

sapply(pass_vect, function(x){
  BetaBit::proton(action = "login", login = JI_login, password = x)
  } )


## Problem 3

BetaBit::proton(action = "server", host = "XYZ")

logs_dataset <- logs
head(logs_dataset)

Pietraszko_login <- BetaBit::employees %>%
  filter(surname == "Pietraszko") %>%
  select(login)

Pietraszko_login$login

logs_dataset %>%
  filter(login == Pietraszko_login$login) %>%
  count(host)

BetaBit::proton(action = "server", host = "194.29.178.16")


## Problem 4

bash_hist <- bash_history

Pietraszko_pass <- tail(unique(stri_split(bash_hist, fixed = " ", simplify = TRUE)[,1]), n=1)
Pietraszko_pass

proton(action = "login", login = Pietraszko_login$login, password = Pietraszko_pass)
