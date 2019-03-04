#Przedstawic gestosc rozkladu populacji tylko dla Europy i Afryki, gdzie kontynent jest zaznaczony jako wypelnienie (fill). 
#Pokazac ten sam wykres z uzyciem czterech roznych palet kolorow. 
#Wynikowy wykres pokazac na jednym rysunku (z uzyciem np. grid.arrange


print(countries)

countriesA<- countries %>% 
  filter(continent=="Africa" | continent =="Europe")


pureRB<-ggplot(countriesA, aes(x = population ,fill=continent)) +
  geom_density() +
  scale_fill_manual(values=c("red","blue"))
  
pureYB<-ggplot(countriesA, aes(x = population ,fill=continent)) +
  geom_density() +
  scale_fill_manual(values=c("yellow","blue"))

pureBB<-ggplot(countriesA, aes(x = population ,fill=continent)) +
  geom_density() +
  scale_fill_manual(values=c("black","blue"))

pureGB<-ggplot(countriesA, aes(x = population ,fill=continent)) +
  geom_density() +
  scale_fill_manual(values=c("green","blue"))



