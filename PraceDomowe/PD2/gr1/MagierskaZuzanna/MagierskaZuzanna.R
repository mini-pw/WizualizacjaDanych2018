library(dplyr)
library(ggplot2)

console <- c("Nintendo Switch", "PlayStation 4", "Nintendo 3DS", "PS Vita", "Xbox One")
manufacturer <- c("Nintendo", "Sony Interactive Entertainment", "Nintendo", 
                  "Sony Interactive Entertainment", "Microsoft")
consoles_sold <- c(3482388, 1695227, 566420, 181728, 15339)
df<- data.frame(console, manufacturer, consoles_sold) %>% 
  mutate(consoles_sold_mln = consoles_sold / 1000000)  
  
plot <- ggplot(df, aes(x=reorder(console, -consoles_sold), y=consoles_sold_mln, fill=manufacturer)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = consoles_sold_mln), vjust = -0.3) +
  ggtitle("Sprzeda¿ konsoli w Japonii w 2018 r.") +
  labs(y="Liczba sprzedanych konsoli (mln)", x = "Konsola") + 
  guides(fill=guide_legend(title="Producent"))

plot
