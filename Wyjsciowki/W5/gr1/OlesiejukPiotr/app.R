library(shiny)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_option",
                         label = "Select options names:",
                         choices = colnames(airquality)[c(1,3,4)],
                         selected = "Ozone"))
    ,
    
    mainPanel(
      h2("Barplot"),
      plotOutput("option_plot", height = 600)
    )
  )
)


server <- function(input, output) {
  
  
  options_colors <- c(Ozone = "blue", Wind = "black" ,Temp = "red")
  options <- colnames(airquality)[c(1,3,4)]
  
  output[["option_plot"]] <- renderPlot({
    
    
    df <- data.frame(Month = airquality[["Month"]],
                    variable = airquality[[input[["chosen_option"]]]])
    
    table <- airquality %>% group_by(Month) %>% summarise(mean_ozone = mean(Ozone[!is.na(Ozone)]),
                                                          mean_wind = mean(Wind),
                                                          mean_temp = mean(Temp))
    
    p <- ggplot(df, aes(x = Month, y = mean(variable[!is.na(variable)]))) +
      ylab(input[["chosen_option"]]) +
      xlab("Month") +
      geom_col() +
      theme_bw()
    
    p
  })
  
  
}

shinyApp(ui = ui, server = server)