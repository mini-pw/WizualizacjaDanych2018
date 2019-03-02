x <- c("Estonia","Malta","Austria","Polska","Dania","Litwa","Bu³garia","Szwecja","Niemcy","Francja",
       "W³ochy","Portugalia","Hiszpania","Cypr","Wlk. Brytania","Grecja")
y <- c(14.2,19.9,26,12.7,23.6,12,9.6,22.2,24.0,23.7,18.6,12.9,17.9,19.7,20.2,10.5)
dane<- data.frame(x,y)
dane

library(ggplot2)
(ggplot(dane, aes(x=x, fill=x)) + geom_bar(aes(weight = y))
  + ggtitle("Wysokoœæ przeciêtnego dochodu do dyspozycji dla krajów UE w 2017r.")
  + labs(y="Wysokoœæ dochodu w tys", x = "Kraj")
  + theme(axis.text.x=element_text(angle=25,hjust=1,vjust=0.5,size = 15)))


#https://www.forbes.pl/gospodarka/najbogatsze-i-najbiedniejsze-regiony-ue-najnowsze-dane-eurostat/nkghh1t