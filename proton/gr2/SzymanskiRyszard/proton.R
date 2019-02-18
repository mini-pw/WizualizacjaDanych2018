library(BetaBit)
library(data.table)
library(magrittr)

proton()

# Problem 1
employees_dt <- as.data.table(employees)
result_login <- employees_dt[name == "John" & surname == "Insecure", login]

proton(action = "login", login = result_login)

# Problem 2
for (password in top1000passwords) {
  login_responce <- proton(action = "login", login = result_login, password = password)
  if (login_responce == "Success! User is logged in!") {
    result_password <-  password
    break
  }
}

# Problem 3
logs_dt <- data.table(logs)
pietraszko_login <- employees_dt[surname == "Pietraszko", login]
result_host <- logs_dt[login == pietraszko_login, .N, by = host][N == max(N), host] %>% 
  as.character()

proton(action = "server", host = result_host)

# Problem 4
commands <- sapply(bash_history, function(bash_line) {
  command <- strsplit(bash_line, " ")[[1]][1]
}) %>% unique()

# After analyzing the vector values it turned
# out that the last value is the password
# that we were looking for
result_password <- commands[length(commands)]
proton(action = "login", login = pietraszko_login, password = result_password)

