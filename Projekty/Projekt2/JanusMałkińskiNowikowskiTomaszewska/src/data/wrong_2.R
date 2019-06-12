require(SmarterPoland)

s<-filter(countries, continent=="Oceania")



polar_module <- function(input, output, session)
{
  output$bad_polar <- renderPlot({
    ggplot(s, aes(country, death.rate, fill = country)) +
      geom_bar(width = 1, stat = "identity", color = "white") + 
      scale_y_continuous(breaks = 0:nlevels(s$death.rate)) +
      theme_gray() +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank()) + coord_polar() +ggtitle('Death rate in Oceanian countries')
  })
  
  output$good_polar <- renderPlot({
    ggplot(s, aes(country, death.rate, fill = country)) +
      geom_bar(width = 1, stat = "identity",color = "white" ) + 
     theme_bw()+
      theme(axis.text.x = element_blank())+
      ggtitle('Death rate in Oceanian countries')
  })

}
