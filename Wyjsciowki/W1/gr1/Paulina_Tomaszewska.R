install.packages("SmarterPoland")
library(SmarterPoland)
install.packages("ggthemes")
library(ggthemes)


p<-ggplot(countries, aes(x=death.rate, fill=continent))+geom_density(alpha=0.2)#+facet_wrap(~continent)
p1<-p+theme_bw()
p2<-p+theme_excel()
p3<-p+theme_economist()
p4<-p+theme_grey()

library(patchwork)
(p1+p2)/(p3+p4)

