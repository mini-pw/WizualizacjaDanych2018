# Init section
install.packages("BetaBit")
library(BetaBit)
install.packages("dplyr")
library(dplyr)

# First task
proton()
JI_login <- filter(employees, name == "John", surname == "Insecure")$login
proton(action = "login", login = JI_login)

# Second task
for (pass in  top1000passwords) {
  ret_string <- proton(action = "login", login = JI_login, password = pass)
  if (ret_string == "Success! User is logged in!") {
    break
  }
}

# Third task
SP_login <- filter(employees, name == "Slawomir", surname == "Pietraszko")$login
host_count_map <- data.frame(filter(logs, login == SP_login) %>% count(host))
host_count <- host_count_map$n
host_idx <- which(host_count == max(host_count))
host_name <- toString(host_count_map[1, host_idx])
proton(action = "server", host = host_name)

# Fourth task
bash_history_df <- as.data.frame(bash_history)
n <- length(bash_history)
commands <- data.frame(cmd = character(n), stringsAsFactors=FALSE) 

i <- 1
for (cmd in bash_history) {
  cmd_first <- strsplit(cmd, " ")[[1]][1]
  commands$cmd[i] <- cmd_first
  i <- i + 1
}
commands %>% group_by(cmd) %>% group_keys()
# 4th one (DHbb7QXppuHnaXGN) looks like a password
proton(action = "login", login = SP_login, password = "DHbb7QXppuHnaXGN")