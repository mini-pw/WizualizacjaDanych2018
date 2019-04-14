# skrypt generujacy bazowy wykres

library(dplyr)
library(ggplot2)

x <- c("Estonia","Malta","Austria","Polska","Dania","Litwa","Bułgaria","Szwecja","Niemcy","Francja",
       "Włochy","Portugalia","Hiszpania","Cypr","Wlk. Brytania","Grecja")
y <- as.numeric(c(14.2,19.9,26,12.7,23.6,12,9.6,22.2,24.0,23.7,18.6,12.9,17.9,19.7,20.2,10.5))
dane<- data.frame(x,y)

dane <- data.frame(x=x,y=y, stringsAsFactors = FALSE)
orders <- dane %>% arrange(desc(y))
orders$x <- factor(orders$x, levels = orders$x)

p <- ggplot(data = orders, aes(x = x, y = y)) +
  # geom_bar(stat = 'identity',fill = 'darkgreen', width = 0.9, position = position_dodge())+
  geom_col(fill = 'darkgreen', width = 0.9)+
  scale_y_discrete(limits = seq(0,max(y)+2,4), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(y)+2)))+
  geom_text(aes(label=formatC(orders$y, digits = 1, format = 'f')),
            colour ="White", fontface="bold", 
            size = 4, hjust =0.5,
            vjust = 1.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(face='bold', hjust = 0.5, margin = margin(0,0,20,0)),
        axis.text.x = element_text(angle = 90, face='bold',size=12, hjust = 1, vjust=0.25),
        axis.text.y = element_text(face="bold", size=12),
        axis.title.y = element_text(margin = margin(0,10,0,0)), 
        panel.border = element_blank(),
        axis.line = element_line(size = 0.7),
        axis.ticks.x = element_blank())+
  ggtitle("Wysokość przeciętnego dochodu do dyspozycji dla krajów UE w 2017r.") +
  labs(y="Wysokość dochodu w tys", x=' ') 

svg("base_plot.svg", height = 7.5, width = 8)
p
dev.off()
