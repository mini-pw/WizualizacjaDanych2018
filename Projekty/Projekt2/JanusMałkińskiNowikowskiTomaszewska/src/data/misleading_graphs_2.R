require(ggplot2)
require(dplyr)
require(plotrix)
require(SmarterPoland)
require(ggsci)
require(tidyr)

description <- 'How to trick your audience?'
Sys.setlocale("LC_TIME", "C")
thb <- read.csv('data/thb.csv')
thb <- thb %>% select(date = 2, rate = 3) %>% mutate(currency = 'thb', 
                                                     date = as.Date(date))
gbp <- read.csv('data/gbp.csv')
gbp <- gbp %>% select(date = 2, rate = 3) %>% mutate(currency = 'gbp', 
                                                     date = as.Date(date))
dates <- gbp$date
dates <- dates[order(dates)] %>%  unique() %>% format('%d %b')
currency_rate <- rbind(thb,gbp) %>% mutate(currency = as.factor(currency),
                                           date = factor(format(date,'%d %b'), levels = dates))

yaxis_module <- function(input, output, session)
{
  output$thb_plot <- renderPlot({
    
    ggplot(thb, aes(x = date, y = rate))+
      geom_line(size = 1)+
      geom_point(size = 3)+
      scale_fill_jco() +
      ggtitle("Currency rate for THB to PLN") +
      labs(x = '', y= '')+
      theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  })
  output$gbp_plot <- renderPlot({
    
    ggplot(gbp, aes(x = date, y = rate, color = ))+
      geom_line(size = 1)+
      geom_point(size = 3)+
      scale_color_aaas() +
      ggtitle("Currency rate for GBP to PLN") +
      labs(x = '', y= '')+
      theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$all_currency <- renderPlot({
    
    ggplot(currency_rate, aes(x = date, y = rate, group = currency, color = currency))+
      geom_line(size = 1)+
      geom_point(size = 3)+
      scale_color_aaas() +
      ggtitle("Currency rate to PLN for last month") +
      labs(x = '', y= '')+
      theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'lightgray'),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
}

