library(ggthemes)
library(waffle)
library(plotrix)
library(ggthreed)

live_matches = data.frame(Result = c("Win", "Draw", "Loss"), Value = c(30, 7, 1))
live_matches$Result = factor(live_matches$Result, levels = live_matches$Result)

live_scorers = data.frame(Player = c("Mohamed Salah",
                                     "Sadio Mane",
                                     "Roberto Firmino",
                                     "Xerdan Shaqiri",
                                     "James Milner",
                                     "Virgil van Dijk",
                                     "Georginio Wijnaldum",
                                     "Divock Origi",
                                     "Daniel Sturridge",
                                     "Naby Keita",
                                     "Fabinho",
                                     "Jordan Henderson",
                                     "Trent Alexander-Arnold",
                                     "Joel Matip",
                                     "Dejan Lovren"),
                          Value = c(22, 22, 12, 6, 5, 4, 3, 3, 2, 2, 1, 1, 1, 1, 1))
live_scorers$Player = factor(live_scorers$Player, levels = live_scorers$Player)

first_ok <- ggplot(data = live_matches, aes(x = Result, y = Value)) +
  geom_bar(stat = "identity") +
  theme_hc()

first_bad <- waffle(live_matches$Value, rows = 5) +
  scale_fill_manual(name = "Result: ", labels = c("Win", "Draw", "Loss", ""),
                    values = c("#414046", "#7CB5EC", "#90EC7D", "#FFFFFF")) +
  theme(legend.text = element_text(size = 15), legend.title = element_text(size=17),
        legend.position = "right")

second_ok <- ggplot(data = live_scorers, aes(x = Player, y = Value)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expand_scale(add = c(0, 8))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4)) +
  theme_hc()

second_bad <- function() {
  pie3D(live_scorers$Value, labels = live_scorers$Player, explode=0.4, labelcex = 1)
}
