#modyfikowany wykres nale¿a³ do Kolegi Tymona Czarnoty



library(ggplot2)
library(dplyr)
Dzielnice <- c('Bemowo', 'Bia³o³êka', 'Bielany', 'Mokotów', 'Ochota', 'Praga-Po³udnie', 'Praga-Pó³noc', 'Rembertów', 'Œródmieœcie',  'Targówek', 'Ursus', 'Ursynów', 'Wawer', 'Weso³a', 'Wilanów', 'W³ochy', 'Wola', '¯oliborz')
LiczbaInterwencji <- c(177, 295, 623, 578, 532, 838, 564, 88, 1446, 431, 162, 195, 294, 55, 157, 185, 900, 400)
dane <- data.frame(Dzielnice, LiczbaInterwencji)
dane <- dane %>% mutate(Dzielnica = factor(Dzielnice,levels=rev(unique(Dzielnice))))
dane$Dzielnica <- factor(dane$Dzielnica, levels = dane$Dzielnica[order(dane$LiczbaInterwencji)])
ggplot(data=dane, aes(x=Dzielnica, y=LiczbaInterwencji)) + 
  scale_x_discrete() +
  scale_y_continuous() + 
  geom_bar(stat='identity', aes(fill = Dzielnica)) + 
  ggtitle('Liczba interwencji Stra¿y Mijeskiej m.st. Warszawy \n w styczniu 2019 w podziale na dzielnice') + 
  xlab('Dzielnice') +
  ylab('Liczba interwencji')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label =LiczbaInterwencji, y = LiczbaInterwencji), size = 3)
dane1<-dane[order(LiczbaInterwencji),]
head(dane1)
write.table(dane1[.2:3],file="dopd6.csv",quote = F, row.names = F, col.names = T,sep = ',',fileEncoding="UTF-8")
