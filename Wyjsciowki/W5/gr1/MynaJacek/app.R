library(shiny)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_variable",
                         label = "Select variable:",
                         choices = c("Ozone", "Wind", "Temp"),
                         selected = "Ozone", 
                         multiple = FALSE)
    ),
    
    mainPanel(
      h2("My plot"),
      plotOutput("my_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  output[["my_plot"]] <- renderPlot({
    
    p <- data.frame(Month = airquality[["Month"]], 
               variable = airquality[[input[["chosen_variable"]]]]) %>%
    group_by(Month) %>% 
    summarize(Mean = mean(variable, na.rm=TRUE)) %>%
    ggplot(aes(x = Month, y = Mean)) +
      geom_col() +
      theme_bw() +
      labs(y = paste0("Mean value of ", input[["chosen_variable"]], " by Month"))
    
    p
  })
  
}

shinyApp(ui = ui, server = server)