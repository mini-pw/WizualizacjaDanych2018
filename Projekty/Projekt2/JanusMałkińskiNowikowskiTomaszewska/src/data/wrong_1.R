require(latticeExtra)
require(ggplot2)

dane<-read.csv2("http://www.ibspan.waw.pl/~pgrzeg/stat_lab/samochody.csv")


dane
#CYLINDRY
#PRZYSP
#ROK
x<-dane$cylindry
y<-dane$rok+1900
z<-dane$przysp

d<-data.frame(x,y,z)
set<-d %>% group_by(x,y) %>% summarise(acc=mean(z))
set



dim_module <- function(input, output, session)
{
  output$bad_3D_barplot <- renderPlot({
    
    #bad_3D_barplot<-
      cloud(acc~factor(x)+factor(y), set, panel.3d.cloud=panel.3dbars, col.facet='blue', 
                          xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1, distance=1),xlab=list("number of cylinders", rot=30),
                          ylab=list("production year", rot=-40), zlab=list("acceleration", rot=92), 
                          par.settings = list(axis.line = list(col = "transparent")))
  })

  output$good_3D_barplot <- renderPlot({
    
    ggplot(set,aes(x = y, y = acc)) + 
      geom_bar(stat='identity') + theme_bw()+ labs(x = "production year", y="mean acceleration")+
      facet_wrap(~x)
  })
  
}
