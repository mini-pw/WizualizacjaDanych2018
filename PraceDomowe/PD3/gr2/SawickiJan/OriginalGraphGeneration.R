# Source
# https://github.com/mini-pw/WizualizacjaDanych2018/pull/179/files?file-filters%5B%5D=.html#diff-457ed911f033e454c21423e68c848469

library(ggplot2)
library(dplyr)
library(forcats)

Dzielnice <- c('Bemowo', 'Białołęka', 'Bielany', 'Mokotów', 'Ochota', 'Praga-Południe', 'Praga-Północ', 'Rembertów', 'Śródmieście',  'Targówek', 'Ursus', 'Ursynów', 'Wawer', 'Wesoła', 'Wilanów', 'Włochy', 'Wola', 'Żoliborz')
LiczbaInterwencji <- c(177, 295, 623, 578, 532, 838, 564, 88, 1446, 431, 162, 195, 294, 55, 157, 185, 900, 400)

dane <- data.frame(Dzielnice, LiczbaInterwencji)

dane <- dane %>% mutate(Dzielnica = factor(Dzielnice,levels=rev(unique(Dzielnice))))

OriginalPlot = dane %>%
  mutate(Dzielnica = fct_reorder(Dzielnica, LiczbaInterwencji)) %>%
  ggplot(aes(x= Dzielnica, y= LiczbaInterwencji, fill = LiczbaInterwencji)) + 
  scale_x_discrete() +
  scale_y_continuous() +  
  geom_bar(stat='identity') + 
  labs(title='Liczba interwencji Warszawskiej Straży Miejskiej',
       x="", y="") + 
  coord_flip() +
  theme_bw() +
  scale_fill_gradient(name = "Liczba interwencji", low="springgreen3", high="red")


svg(filename = "OriginalPlot.svg")
OriginalPlot
dev.off()
