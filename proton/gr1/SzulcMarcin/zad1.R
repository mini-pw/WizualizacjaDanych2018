library(BetaBit)
proton()

employees[employees$name=="John" & employees$surname == "Insecure", ]
# John Insecure's login == johnins

proton(action="login", login="johnins")

for (pass in top1000passwords) {
  proton(action="login", login="johnins", password=pass)
}

employees[employees$name=="Slawomir" & employees$surname == "Pietraszko", ]
# Slawomir Pietraszko's login == slap

tail(names(sort(table(logs[logs$login=="slap", ]$host))), 1)
# the most common ip: 194.29.178.16

proton(action="server", host="194.29.178.16")

# get unique values from first column (only strings before space)
unique(lapply(bash_history, {function(x) strsplit(x, split="\\s")[[1]][1]}))
# password == DHbb7QXppuHnaXGN

proton(action="login", login="slap", password="DHbb7QXppuHnaXGN")
