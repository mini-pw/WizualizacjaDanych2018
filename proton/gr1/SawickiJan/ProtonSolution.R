install.packages("BetaBit")
BetaBit::proton()
BetaBit::employees

# Problem 1

BetaBit::employees[grep("John", BetaBit::employees$name),]
#johnins
BetaBit::proton(action = "login", login="johnins")

# Problem 2

for (password1 in BetaBit::top1000passwords){
  BetaBit::proton(action = "login", login="johnins", password=password1)
}

# Problem 3

BetaBit::employees[grep("Pietraszko", BetaBit::employees$surname),]

BetaBit::logs[grep("slap", BetaBit::logs$login),]

BetaBit::logs[grep("pietraszko", BetaBit::logs$login),]

BetaBit::employees

slap_logs=BetaBit::logs[grep("slap", BetaBit::logs$login),]

slap_logs[order(slap_logs$host),]

tail(names(sort(table(slap_logs$host))), 1)

BetaBit::proton(action = "server", host="194.29.178.16")

# Problem 4

for (command1 in BetaBit::bash_history){
  command = strsplit(command1, "[ ]")[[1]][1]
  
  BetaBit::proton(action = "login", login="slap", password=command)
}
