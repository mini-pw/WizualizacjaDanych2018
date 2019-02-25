library('BetaBit')
library('data.table')
library('stringi')
proton()


# Konwertowanie na data.table
employees_DT <- data.table(employees)


# Znalezienie loginu
JI_login <- employees_DT[
    name == "John" & surname == "Insecure", .(login)
]


# Logowanie
proton(action="login", login=JI_login)


# Łamanie hasła
sapply(
    top1000passwords,
    function(pass) proton(
        action="login",
        login=JI_login,
        password=pass
    )
)

proton(action="login", login=JI_login, password=top1000passwords)


# Serwer z którego Pietraszko loguje się najczęściej
logs_DT <- data.table(logs)
Pietraszko_login <- unlist(employees_DT[surname == "Pietraszko", .(login)])

Pietraszko_hosts <- logs_DT[login == Pietraszko_login, .(host)]
Pietraszko_hosts_frequency <- data.table(table(Pietraszko_hosts))
Pietraszko_top_host <- unlist(Pietraszko_hosts_frequency[N == max(N), .(Pietraszko_hosts)])

proton(action="server", host="194.29.178.16")


# Łamanie hasła Pietraszko
komendy <- unname(sapply(bash_history, function(line) unlist(stri_split_fixed(line, " "))[1]))
table(komendy)

proton(action="login", login=Pietraszko_login, password="DHbb7QXppuHnaXGN")
