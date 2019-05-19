library(dplyr)
library(ggplot2)
library(scales)
library(rmarkdown)

data<- read.csv("/run/media/miesiaca/Elements SE/STUDIA/Wizualizacja/dashbord/PL_dat.csv", sep = ';')
colnames(data)
suma_strzelonych_goli <- sum(data["home_team_goal"]) + sum(data["away_team_goal"])
#team <- 'Arsenal'
#seas<-'2014/2015'
strzelonych_goli<-sum(data%>%filter(home_team==team, season ==seas)%>%select(home_team_goal))+
  sum(data%>%filter(away_team==team, season ==seas)%>%select(away_team_goal))
#team_count<- nrow(distinct(data, away_team))
suma<- cumsum(data%>%filter(home_team==team, season ==seas)%>%arrange(date)%>%select(home_team_goal))%>%rename(suma = home_team_goal)
cbind(data%>%filter(home_team==team, season ==seas)%>%arrange(date), suma)

#NA WYJEŹDZIE KTO NAJWIECEJ ZDOBYŁ GOLI
wykres1<-data%>%filter(season == seas)%>%group_by(away_team)%>%
  summarise(gole_wyjazd = sum(away_team_goal))%>%
  arrange(-gole_wyjazd)

ggplot(wykres1,aes(x=away_team, y = gole_wyjazd, fill = gole_wyjazd))+
  geom_bar(stat = 'identity')+ 
  scale_x_discrete(name ="Drużyny",limits = wykres1$away_team)+ 
  scale_y_continuous(name = "łączna ilość goli zyskanych na wyjeździe") +
  scale_fill_continuous(name = "Suma goli") + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


#U SIEBIE KTO NAJWIECEJ ZDOBYŁ GOLI
wykres2<-data%>%filter(season == seas)%>%group_by(home_team)%>%
  summarise(gole_wyjazd = sum(home_team_goal))%>%arrange(-gole_wyjazd)

ggplot(wykres2, aes(x=home_team, y = gole_wyjazd, fill = gole_wyjazd))+geom_bar(stat = 'identity')+ 
  scale_x_discrete(name ="Drużyny",limits = wykres2$home_team)+ 
  scale_y_continuous(name = "łączna ilość goli zyskanych u siebie") +
  scale_fill_continuous(name = "Suma goli") + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


#U SIEBIE KTO NAJWIECEJ STRACIŁ
wykres3<-data%>%filter(season == seas)%>%group_by(home_team)%>%
  summarise(gole_wyjazd = sum(away_team_goal))%>%arrange(-gole_wyjazd)

ggplot(wykres3, aes(x=home_team, y = gole_wyjazd, fill = gole_wyjazd))+geom_bar(stat = 'identity')+ 
  scale_x_discrete(name ="Drużyny",limits = wykres3$home_team)+ 
  scale_y_continuous(name = "łączna ilość goli straconych u siebie") +
  scale_fill_continuous(low = "black", high = "red", name = "Suma goli") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


#NA WYJEŹDZIE KTO NAJWIECEJ STRACIŁ
wykres4<-data%>%filter(season == seas)%>%group_by(away_team)%>%
  summarise(gole_wyjazd = sum(home_team_goal))%>%arrange(-gole_wyjazd)

ggplot(wykres4, aes(x=away_team, y = gole_wyjazd, fill = gole_wyjazd))+geom_bar(stat = 'identity')+ 
  scale_x_discrete(name ="Drużyny",limits = wykres4$away_team)+ 
  scale_y_continuous(name = "łączna ilość goli straconych na wyjeździe") +
  scale_fill_continuous(low = "black", high = "red", name = "Suma goli") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


data<- data%>%
  mutate(
    home_team_pkt = if_else(home_team_goal>away_team_goal, as.integer(3), 
                            if_else(home_team_goal==away_team_goal,as.integer(1),as.integer(0))),
    away_team_pkt = if_else(home_team_goal>away_team_goal, as.integer(0), 
                            if_else(home_team_goal==away_team_goal,as.integer(1),as.integer(3))))





#NA WYJEŹDZIE KTO NAJWIECEJ ZDOBYŁ PUNKTÓW
wykres5<-data%>%filter(season == seas)%>%group_by(away_team)%>%
  summarise(pkt_wyjazd = sum(away_team_pkt))%>%arrange(-pkt_wyjazd)

ggplot(wykres5, aes(x=away_team, y = pkt_wyjazd, fill = pkt_wyjazd))+geom_bar(stat = 'identity')+ 
  scale_x_discrete(name ="Drużyny",limits = wykres5$away_team)+ 
  scale_y_continuous(name = "ilość punktów zyskanych na wyjeździe") +
  scale_fill_continuous(name = "Suma punktów") + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


#U SIEBIE KTO NAJWIECEJ ZDOBYŁ PUNKTÓW
wykres6<-data%>%filter(season == seas)%>%group_by(home_team)%>%
  summarise(pkt_wyjazd = sum(home_team_pkt))%>%arrange(-pkt_wyjazd)

ggplot(wykres6, aes(x=home_team, y = pkt_wyjazd, fill = pkt_wyjazd))+geom_bar(stat = 'identity')+ 
  scale_x_discrete(name ="Drużyny",limits = wykres6$home_team)+ 
  scale_y_continuous(name = "ilość punktów zyskanych u siebie") +
  scale_fill_continuous(name = "Suma punktów") + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


#ŁĄCZNA ILOŚĆ PUNKTÓW
data2<- inner_join(wykres5, wykres6, by = c("away_team"="home_team"))
data2$suma<-data2$pkt_wyjazd.x+data2$pkt_wyjazd.y
data2<-data2%>%arrange(-suma)

ggplot(data2, aes(x=away_team, y = suma, fill = suma))+geom_bar(stat = 'identity')+ 
  scale_x_discrete(name ="Drużyny",limits = data2$away_team)+ 
  scale_y_continuous(name = "łączna ilość punktów") +
  scale_fill_continuous(name = "Suma punktów") + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))

#38 kolejek
data<- data%>%
  mutate(which_stage = if_else(stage<=19, 'First','Second'))
#data$which_stage<-if_else(data$stage<=19, 'First','Second')

#u siebie ile punktów w podziale na półrocza
wykres7<-data%>%filter(season==seas)%>%group_by(home_team,which_stage)%>%summarise(suma_pkt_half = sum(home_team_pkt))
ggplot(wykres7, aes(fill = which_stage, y = suma_pkt_half, x = home_team))+
  geom_bar(position = 'dodge', stat = "identity")+ 
  scale_x_discrete(name ="Drużyny",limits = data2$away_team)+ 
  scale_y_continuous(name = "Ilość punktów") +
  scale_fill_discrete(name = "Półrocze") + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


#ile punktów w podziale na półrocza


wykres8<-data%>%filter(season==seas)%>%group_by(away_team,which_stage)%>%summarise(suma_pkt_half = sum(away_team_pkt))
pom1<-data%>%filter(season==seas)%>%group_by(home_team,which_stage)%>%summarise(suma_pkt_half = sum(home_team_pkt))
wykres8$suma_pkt_half_all<-wykres8$suma_pkt_half+pom1$suma_pkt_half

ggplot(wykres8, aes(fill = which_stage, y = suma_pkt_half_all, x = away_team))+
  geom_bar(position = 'dodge', stat = "identity")+ 
  scale_x_discrete(name ="Drużyny",limits = data2$away_team)+ 
  scale_y_continuous(name = "Ilość punktów") +
  scale_fill_discrete(name = "Półrocze") + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


#różnica miejsc w zależności od kolejki
miejsce_kolejki<-distinct(data%>%filter(season==seas), away_team)
miejsce_kolejki$stage_1 = 0
data%>%filter(season==seas, stage==1)
miejsce_kolejki$stage_1<- ifelse()



pom2<-rbind(select(data, season, stage, team_pkt = home_team_pkt, 
             team_name = home_team), 
      select(data, season, stage, team_pkt = away_team_pkt, 
             team_name = away_team))%>%filter(season==seas, stage==1)

pom3<-rbind(select(data, season, stage, team_pkt = home_team_pkt, 
                   team_name = home_team), 
            select(data, season, stage, team_pkt = away_team_pkt, 
                   team_name = away_team))

seas='2014/2015'
pom2$cumsum<-pom2$team_pkt
number<-1
sum(dat%>%filter(home_team=='Aston Villa', season=='2015/2016')%>%select(home_team_pkt))+
  sum(dat%>%filter(away_team=='Aston Villa', season=='2015/2016')%>%select(away_team_pkt))
#pom2$stage_1#<-0
for(number in 2:38){
  pom4<-pom3%>%filter(season==seas, stage==number)
  pom4<-pom4[order(pom4$team_name),]
  pom2$cumsum<-pom2$cumsum+pom4$team_pkt
  pom2[paste0("stage_",number)]<-0

  
  ord<-order(-pom2$cumsum)
  pom2<-pom2[ord,] 
  pom2[1,paste0("stage_",number)]<-1
  for( i in 2:nrow(pom2)){
   if (pom2$cumsum[i-1]==pom2$cumsum[i]){
     pom2[i,paste0("stage_",number)]<-pom2[i-1,paste0("stage_",number)]
   }
    else{
      pom2[i,paste0("stage_",number)]<-(pom2[i-1,paste0("stage_",number)]+1)
    }
  }
  pom2<-pom2[order(pom2$team_name),]

}

pom5<-pom2
pom5$season<-NULL
pom5$stage<-NULL
pom5$team_pkt<-NULL
pom5$cumsum<-NULL


n <- pom5$team_name
# transpose all but the first column (name)
pom5 <- as.data.frame(t(pom5[,-1]))
colnames(pom5) <- n
row.names(pom5)<-1:38
pom5$index<-1:38
getwd()

#write.csv(pom5, "MiesjceVsKolejka14_15.csv")
team = 'Leicester City'
ggplot(pom5, aes(x =1:38,  y = `Leicester City`, group=1))+geom_line()+geom_point() +
  scale_x_discrete(name = "Kolejka",labels = as.character(1:38), breaks = 1:38)+ 
  scale_y_reverse(name = "Miejsce w tabeli",labels = as.character(1:14), breaks = 1:14)+
  ggtitle(team)+
  theme(
    plot.title=element_text(family='', face='bold', size=15)
  )

pom5$`Leicester City`