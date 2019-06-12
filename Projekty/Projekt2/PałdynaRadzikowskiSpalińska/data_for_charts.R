#---------------------------------------------------------------------------------------------------------------------------
#
# WD - Projekt 2
#
#---------------------------------------------------------------------------------------------------------------------------
#
# Dane do wykresow i wykresy
#
#---------------------------------------------------------------------------------------------------------------------------

### Dane

## 1) "elastyczne sondaze"
partie <- c("PiS", "Nowoczesna", "PO", "Kukiz'15", "PSL", "SLD", "Razem", "Wolnosc", "Kongres Nowej Prawicy", "Prawica RP", "Nie wiem")
procenty <- c(30.1, 20.0, 15.2, 8.6, 5.4, 5.2, 4.0, 2.8, 0.9, 0.0, 7.9)
df1 <- data.frame(partie, procenty)


## 2) "jak po metaamfetaminie"
kraje <- c("Myanmar", "Laos", "Wietnam", "Kambodza", "Tajlandia\n(najtaniej)", "Tajlandia\n(najdrozej)", "Malezja", "Chiny")
ceny <- c(2, 2, 2.5, 4, 3, 10, 11, 25)
df2 <- data.frame(kraje, ceny)


## 3) "niech sie kreci"
Race <- c("White", "Black", "Asian", "Mixed Race", "NS (Not Stated)", "Other")
convictions <- c(6743, 885, 711, 322, 553, 110)
percents <- 100 * convictions / sum(convictions)
df3 <- data.frame(Race, convictions)


## 4) "kobiety giganty"
woman <- data.frame(Country = c("Lativa", "Australia", "Scotland", "Peru", "South Africa", "India"),
                    Height = c(1.68, 1.65, 1.65, 1.65, 1.58, 1.52))


## 5) Nie znam sie
nazwa = c("Swiatowe Dni Mlodziezy", "Nie ma takiego wydarzenia, nie wydarzylo się nic waznego", "Realizacja programu Rodzina 500+", "Wizyta papieza w Polsce", 
  "Wybory 2015 - parlamentarne/prezydenckie", "Rzady PIS, nowa sytuacja polityczna", "Szczyt NATO",
  "Konflikt wokol‚ TK", "Euro 2016, sukcesy polskich piłkarzy", "Inne rzadowe przedsiewziecia, reformy", "1050 rocznica Chrztu Polski",
  "Protesty spoleczne", "Reforma edukacji", "Brexit", "Wydarzenia z dziedziny gospodarki", 
  "Obnizenie wieku emerytalnego","Inne wydarzenia w Polsce", "Inne wydarzenia na swiecie", "Intronizacja Chrystusa na krola Polski", "Wybory prezydenckie w USA",
  "Smierc gornikow w kopalni miedzi", "Reformy podatkowe", "Ekshumacje ofiar katastrofy smolenskiej",
  "Wydarzenia sportowe")

procenty = c(14,8.5,10.3,3.4,3.1,2.9,2.7,2.1,1.6,1.2,1.1,1,1,0.9,0.9,0.8,0.7,0.6,0.4,0.3,0.3,0.2,0.2,0.2)

porownanie_procenty <- c(42.5, sum(procenty))
porownanie_opis <- c("Nie wiem, nie zastanawialem sie, nie interesuje sie", "Inne odpowiedzi")

wydarzenia_df <- data.frame(Wydarzenie=nazwa,Glosy=procenty) 
porownanie_df <- data.frame(Wydarzenie=porownanie_opis,Glosy=porownanie_procenty) 


## 6) Kinowe hity
movies <- data.frame(Title = c("Avengers: \nKoniec Gry","Avengers: \nWojna bez konca","Szybcy i wsciekli \n8",
                               "Gwiezdne Wojny: \nPrzebudzenie Mocy","Jurassic World","Harry Potter \ni Insygnia smierci II","Kapitan Marvel"),
                     Income = c(1209, 640.5, 541.9, 529, 525.5, 483.20, 456.7))


## 7) 
d_zus<-read.csv2('https://raw.githubusercontent.com/radziq1302/CloudApp/master/danezus.csv',sep = ' ', header = F,colClasses=c("numeric",'character',rep("numeric",5)))
d_zus<-transform(d_zus, newcol=as.character(paste(d_zus$V1, d_zus$V2, d_zus$V3)))
d_zus1<-data.frame(d_zus, stringsAsFactors=FALSE)
d_zus1$newcol[]<-as.character(d_zus1$newcol)
d_zus1$newcol[] <- lapply(d_zus1$newcol, as.character)
#d_zus1$newcol %>% dplyr::mutate_if(is.factor, as.character) -> d_zus1$newcol
d_zus$newcol[nrow(d_zus)]='> 4000'

etykietki<-d_zus$newcol[-length(d_zus$newcol)]
etykietki<-etykietki[-length(etykietki)]
etykietki<-as.character(etykietki)
etykietki<-c(etykietki, '>4000')
etykietki
d_zus<-d_zus[-nrow(d_zus),]

## 8) 
odpowiedzi<-c('zdecydowanie tak','raczej tak','trudno powiedziec','raczej nie','zdecydowanie nie')
wartosci<-c(28,25,17,16,14)
tvpis<-data.frame(value=wartosci, resp=as.character(odpowiedzi), fac<-rep(1,5))
tvpisbis<-data.frame(Opinie=c(rep('zdecydowanie tak',28),rep('raczej tak',25),rep('trudno powiedzie?',17),rep('raczej nie',16),rep('zdecydowanie nie',14)), fac<-rep(1,100))
tvpisbis$Opinie = factor(tvpisbis$Opinie,levels(tvpisbis$Opinie)[rev(c(5,2,3,1,4))])

## 9)
dmar<-read.csv('https://raw.githubusercontent.com/radziq1302/CloudApp/master/wdm.csv', header = F)
colnames(dmar)<-c('rok','liczba')
dmar$liczba<-round(dmar$liczba)
dmar$srednia<-c(1,rep(0,4),1,rep(0,4),1,rep(0,4),1,rep(0,4),1,rep(0,4),rep(1,21))

#---------------------------------------------------------------------------------------------------------------------------

### Wykresy


## 1) "elastyczne sondaze"
plot_1 <- function() {
  
  ggplot(df1, aes(x = reorder(partie, -procenty), y = procenty, fill = partie)) +
    geom_bar(stat = "identity") +
    scale_fill_manual("legend", values = c("PiS" = "navyblue", "Nowoczesna" = "dodgerblue", 
                                           "PO" = "darkorange1", "Kukiz'15" = "black", 
                                           "PSL" = "green3", "SLD" = "red", 
                                           "Razem" = "hotpink4", "Wolnosc" = "goldenrod1", 
                                           "Kongres Nowej Prawicy" = "cornflowerblue", 
                                           "Prawica RP" = "firebrick4", "Nie wiem" = "plum")) +
    ylim(0, 32) +
    geom_label(aes(label = paste0(sprintf("%0.1f", round(df1$procenty, digits = 1)), "%")), fill = "white", vjust = -0.15) +
    ggtitle("Wyniki sondazu") +
    xlab("") + ylab("Wynik procentowy") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold", vjust = 2),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          legend.position = "none")
  
}


## 2) "jak po metaamfetaminie"
plot_2 <- function () {
  
  ggplot(df2, aes(x = reorder(kraje, -ceny), y = ceny)) +
    geom_bar(stat = "identity", fill = "hotpink4") +
    ylim(0, 26) +
    geom_label(aes(label = ceny), vjust = -0.15) +
    ggtitle("Cena jednej tabletki metaamfetaminy w wybranych krajach Azji") +
    xlab("") + ylab("Cena [USD]") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold", vjust = 2),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          legend.position = "none")
}


## 3) "niech sie kreci"
plot_3 <- function() {
  
  plot_ly(df3, labels = ~Race, values = ~convictions, type = "pie", textposition = "outside") %>%
    layout(title = "Convictions in England and Wales for class B drug supply",
           font = list(size = 16),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           legend = list(x = 100, y = 0.4),
           margin = 0)
  
}


## 4) "kobiety giganty"
plot_4 <- function() {
  
  ggplot(woman) + 
    geom_bar(aes(x = reorder(Country, -Height), y=Height),stat="identity",fill='darkred',width = 0.3) +
    labs(title = "Average female height", x = "Country", y = "Height") +
    geom_label(aes(x = reorder(Country, -Height), y=Height, label = Height), 
               label.padding = unit(0.1, "lines"), vjust = -0.2, label.size = 0.05) +
    scale_y_continuous(breaks = seq(0, max(woman[["Height"]]+0.02), by = 0.1)) + 
    theme_bw()
  
}


## 5) Nie znam sie
plot_5 <- function() {
  plot_2 <- ggplot(wydarzenia_df) + 
    geom_bar(aes(x = reorder(Wydarzenie, Glosy), y=Glosy),stat="identity",fill='darkred',
             width = 0.8, position = position_dodge(width = 1.6)) +
    geom_label(aes(x = reorder(Wydarzenie, Glosy), y=Glosy,
                   label = paste0(wydarzenia_df$Glosy, "%")), 
               fill = "white", label.size = 0.08, vjust = 3, nudge_x =2.3,
               nudge_y = 0.6, label.padding = unit(0.15, "lines")) +
    scale_y_continuous(expand = c(0, 0.2), limits = c(-0.01,15)) +
    labs(x = "", y = "Procent odpowiedzi") +
    coord_flip() +
    theme_bw()
  
  plot_1 <- ggplot(porownanie_df) + 
    geom_bar(aes(x = reorder(Wydarzenie, Glosy), y=Glosy),stat="identity",fill='darkred') +
    geom_label(aes(x = reorder(Wydarzenie, Glosy), y=Glosy,
                   label = paste0(porownanie_df$Glosy, "%")), 
               fill = "white", label.size = 0.1, vjust = 3, nudge_x =0.4,
               nudge_y = 3.8, label.padding = unit(0.15, "lines")) +
    scale_y_continuous(expand = c(0, 0.2), limits = c(-0.7,100)) +
    labs(x = "", y = "Procent odpowiedzi") +
    coord_flip() +
    theme_bw()
  
  g2 <- ggplotGrob(plot_1)
  g3 <- ggplotGrob(plot_2)
  g <- rbind(g2, g3, size = "first")
  g$widths <- unit.pmax(g2$widths, g3$widths)
  
  
  title_text <- textGrob("Ktore z wydarzen mijajacego roku mozna Pana(i) zdaniem \nuznac za najwazniejsze dla Polski?", 
                         gp=gpar(fontsize=18, fontface="bold",lineheight=1),
                         just=c("left"), x=unit(0.05, "npc"), y=unit(0.05, "npc"))
  subtitle_text <- textGrob("Dane w procentach, pytanie mialo charakter otwarty", 
                            gp=gpar(fontsize=13), just=c("left"), x=unit(0.05, "npc"))
  grid.arrange(title_text, subtitle_text, g, ncol = 1, heights = c(0.4, 1.1, 14))
}


## 6) Kinowe hity
plot_6 <- function() {
  
  ggplot(movies) + 
    geom_bar(aes(x = reorder(Title, -Income), y=Income),stat="identity",fill='darkred') +
    labs(title = "Kinowe rekordy otwarcia (w mln $)", x = "Tytul", y = "Zysk") +
    theme_bw()
  
}


## 7) 
plot_7 <- function() {
  ggplot(data = tvpisbis[order(tvpisbis$Opinie),], aes(fac))+#, aes(x = fac)) +
    geom_bar(aes(fill=Opinie), width = 0.5)+scale_fill_manual(values=c("#14E9E4","#A4FBF9","#E7E3E7","#F07EEB","#B60FAE"))+ theme_bw()+theme(axis.title.x=element_blank(),
                                                                                                                                axis.text.x=element_blank(),
                                                                                                                                axis.ticks.x=element_blank(),legend.text=element_text(size=12))+
    
    labs(title="Czy domalowanie teczowej auroli do wizerunku Matki Boskiej to profanacja?", subtitle="wersja poprawiona")+ylab("wartosc procentowa")+xlab("")+
    xlim(0.65,1.7)
  
}

## 8) 
plot_8 <- function() {
  ggplot(data = d_zus,aes(x = reorder(d_zus$newcol, d_zus$V1), y=d_zus$V4))+geom_bar(stat="identity",fill = "steelblue") + theme_bw()+
    theme(axis.text.x=element_text(size=12,angle=45,hjust=1))+
    scale_x_discrete(labels=etykietki)+labs(title="Jak wysokie emerytury maja Polacy", subtitle="wersja poprawiona")+ylab("wartosc procentowa")+xlab("wysokosc emerytury [zl]")                                                                                                                       
  
}

## 9)
options(scipen=10000)
plot_9 <- function() {
  ggplot(dmar[11:nrow(dmar),], aes(x=rok, y=liczba))+geom_bar(stat = 'identity', width = 0.9, aes(fill=as.factor(srednia))) +theme_bw()+
    theme(axis.text.x=element_text(size=12,angle=45,hjust=1),axis.text.y=element_text(size=12), legend.position = 'bottom')+scale_fill_manual(values=c("#ECECEC","#870B63"))+
    #geom_text(size = 3,aes(label=liczba), position=position_dodge(width=0.9),hjust=-0.25)+ 
    scale_fill_discrete(name = 'zrodlo danych', labels = c('brak danych', 'dane GUS'))+
    labs(title="Liczba zawartych malzenstw", subtitle="wersja poprawiona")+
    ylab("liczba malzenstw")+xlab("rok")
}
