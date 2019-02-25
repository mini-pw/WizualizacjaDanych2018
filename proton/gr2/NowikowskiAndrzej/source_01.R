# install.packages("BetaBit")
# install.packages("dplyr")
library(BetaBit)
library(dplyr)
library(stringi)
proton()

# part 1
login = employees %>% filter(name=='John', surname=='Insecure') %>% select(login)
login = login[[1]]

proton(action = "login", login=login)

# part 2

# passwords = as.data.frame(top1000passwords);
# colnames(passwords) <- 'pwd'

# x= passwords %>% 
#  mutate(result = proton(action = "login", login=login, password=pwd)) %>% 
#  filter(!grepl('incorrect', result))

for (pass in top1000passwords) {
  if (grepl('incorrect', proton(action="login", login=login, password=pass))){
    pwd = pass
  }
}


# part 3

pietraszko_login = employees %>% filter(surname=="Pietraszko") %>% select(login)
pietraszko_login = pietraszko_login[[1]]

logs %>% 
  filter(login==pietraszko_login) %>% 
  group_by(host) %>% 
  summarize(count = length(host)) %>% 
  arrange(count)


proton(action = "server", host="194.29.178.16")

# part 4


# as.data.frame(bash_history) %>% 
#  mutate(command = stri_split_fixed(as.character(bash_history), ' ')[1])


# as.data.frame(bash_history) %>% 
#  mutate(astext = as.character(bash_history)) %>% 
#  mutate(command = unlist(strsplit(as.character(bash_history), ' '))[1])


# unlist(strsplit("cat /var/log/dpkg.log", " "))[1]

# history <- as.data.frame(bash_history, stringsAsFactors=FALSE)
# str(history)
# history %>% 
#  mutate(command = unlist(strsplit(bash_history, " "))[[1]]) %>% 
#  group_by(command) %>% 
#  summarize(count = length(command))

stri_split_fixed(bash_history, ' ', simplify=TRUE)[,1] %>% unique()

proton(action="login", login=pietraszko_login, password="DHbb7QXppuHnaXGN")