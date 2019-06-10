require(ggplot2)
require(dplyr)
require(plotrix)
require(SmarterPoland)
require(ggsci)

description <- 'How to trick your audience?'

birth <-  countries %>% group_by(continent) %>% 
                        summarise(birth.rate = mean(birth.rate, na.rm=T)) %>% 
                        mutate(continent = factor(continent, levels = continent[order(desc(birth.rate))]))
baseline_module <- function(input, output,session)
{
  
  real_diff <- reactive({
    val1 <- birth %>%  filter(continent == input$continent_1) %>% select(birth.rate)
    val2 <- birth %>%  filter(continent == input$continent_2) %>% select(birth.rate)
    if(val1>val2)
    {
      return(round(val1/val2,2))
    }
    else
    {
      return(round(val2/val1,2))
    }
  })
  curr_diff <- reactive({
    min <- input$axis_range_slider
    val1 <- birth %>%  filter(continent == input$continent_1) %>% select(birth.rate) - min
    val2 <- birth %>%  filter(continent == input$continent_2) %>% select(birth.rate) - min
    
    if(val1>val2)
    {
      return(round(val1/val2,2))
    }
    else
    {
      return(round(val2/val1,2))
    }
  })
  
  output$real_diff_box <- renderValueBox({
    valueBox(
      real_diff(), "Real difference",
      color = "green")
  })
  
  output$curr_diff_box <- renderValueBox({
    valueBox(
      curr_diff(), "Current difference",
      color = "orange")
  })
  
  output$baseline <- renderPlot({
    
    birth <-  countries %>% group_by(continent) %>% 
      summarise(birth.rate = mean(birth.rate, na.rm=T)) %>% 
      mutate(continent = factor(continent, levels = continent[order(desc(birth.rate))]))
    baseline_plot <- ggplot(birth, aes(x = continent, y = birth.rate)) +
      geom_col() +
      scale_y_continuous(expand = c(0,0))+
      coord_cartesian(ylim = c(input$axis_range_slider, 35)) +
      scale_fill_jco() +
      ggtitle("Mean birth rate") +
      guides(fill = FALSE) +
      labs(x = '', y= '')
    theme(
      panel.background = element_blank(),
      panel.grid.major.y = element_line(color = 'lightgray'),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
    baseline_plot
  })
}

