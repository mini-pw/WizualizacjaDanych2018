#https://pl.wikipedia.org/wiki/Bitwa_pod_Chocimiem_(1673)
#https://es.wikipedia.org/wiki/Batalla_de_Chocim_(1673)
#Or?owski D. Chocim 1673, Warszawa, 2007 <- g??wnie to

library(dplyr)
library(ggplot2)

sily_kraje <- c('Rzeczpospolita Obojga Narodow', 'Imperium Osmanskie')
sily_wojska <- c(34000, 37000)
sily_dziala <- c(65, 30)
sily_straty <- c(1500, 30000) #zabici+ranni+schwytani
sily <-  data.frame(
  kraj = sily_kraje,
  liczebnosc_wojsk = sily_wojska,
  liczba_dzial = sily_dziala,
  straty = sily_straty
)

rp_wojska_nazwy <-                c('Husaria', 'Arkebuzeria', 'Pancerni', 'Dragonia', 'Jazda woloska','Piechota', 'Jazda petyhorska', 'Jazda kozacka', 'Rajtaria', 'Jazda tatarska')
rp_wojska_wartosci <- as.integer( c(1690+541,  342,           11055,       5828+1645,  1619,           7988+500+1841+649,       1980,  524,             481,             324)       *.9)

rp_wojska <- data.frame(
  jednostka = rp_wojska_nazwy,
  liczebnosc = rp_wojska_wartosci
)

io_wojska_nazwy <- c('Wojsko\nprowincjonalne Sylistrii', 'Wojsko prowincjonalne\npaszy Bosni Sulej-mana', 'Janczarzy', 'Oddzialy rumelijskie', 'Oddzialy hospodarow\nmoldawskiego i woloskiego')
io_wojska_wartosci <- c(9000, 6000, 5000, 9000, 8000)
io_wojska <- data.frame(
  jednostka = io_wojska_nazwy,
  liczebnosc = io_wojska_wartosci
)

# si?y - zestawienie
forces_both_plot <- ggplot(sily, aes(x="", y=liczebnosc_wojsk, fill=kraj))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
  theme_minimal() +
  ylab("liczebność wojsk") +
  xlab("") + 
  guides(fill=guide_legend(title="Panstwo")) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
forces_both_plot

liczba_dzial_zestawienie_wykres <- ggplot(sily, aes(x="", y=liczba_dzial, fill=kraj))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ylab("liczba dzial") +
  xlab("") + 
  guides(fill=guide_legend(title="Panstwo")) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
liczba_dzial_zestawienie_wykres

wojska_plot <- function(dataframe) {
  order <- dataframe %>% 
    arrange(desc(liczebnosc)) %>% 
    pull(jednostka) %>% 
    as.character()
  plot <- dataframe %>%
    mutate(jednostka=factor(jednostka, levels=order)) %>% 
    ggplot(aes(x=jednostka, y=liczebnosc, fill=jednostka)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=liczebnosc),
              colour ="Black",
              fontface="bold", 
              size = 6, hjust =0.5,
              vjust = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, vjust=0.5, size=12),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          legend.position = "None")
  return(plot)
}

# si?y - RP
wojska_rp_plot <- wojska_plot(rp_wojska)
wojska_rp_plot

# si?y - IO
wojska_io_plot <- wojska_plot(io_wojska)
wojska_io_plot

# straty - zestawienie
loss_both_plot <- sily %>% 
  ggplot(aes(x="", y=straty, fill=kraj))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  xlab("") + 
  guides(fill=guide_legend(title="Panstwo")) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
loss_both_plot

# straty - RP
rp_kraj <- c('Rzeczpospolita Obojga Narodow', 'Rzeczpospolita Obojga Narodow')
rp_liczebnosc <- c(34000-1500, 1500)
rp_typ <- c('sily', 'straty')
rp_straty <-  data.frame(
  kraj = rp_kraj,
  liczebnosc = rp_liczebnosc,
  typ = rp_typ
)
loss_rp_plot <- rp_straty %>% 
  ggplot() + 
  geom_bar(aes(x=kraj, y=liczebnosc, fill=typ), stat='identity')  +
  xlab("") +
  ylab("liczebność") +
  theme_minimal() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
loss_rp_plot

# straty - IO
io_kraj <- c('Imperium Osmanskie', 'Imperium Osmanskie')
io_liczebnosc <- c(37000-30000, 30000)
io_typ <- c('siły', 'straty')
io_straty <-  data.frame(
  kraj = io_kraj,
  liczebnosc = io_liczebnosc,
  typ = io_typ
)
loss_io_plot <- io_straty %>% 
  ggplot() + 
  geom_bar(aes(x=kraj, y=liczebnosc, fill=typ), stat='identity') +
  xlab("") +
  ylab("liczebność") +
  theme_minimal() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
loss_io_plot
