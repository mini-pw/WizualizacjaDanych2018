library(shiny)
library(SmarterPoland)
library(dplyr)

colnames = colnames(airquality)

ui <- fluidPage(
  
  titlePanel("Wyjściówka 5"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_option",
                         label = "Wybierz kolumnę danych:",
                         choices = colnames[! colnames %in% c("Day","Month")],
                         selected = colnames(airquality))
    ),
    
    mainPanel(
      h2("Wykres wybranych danych 'airquality'"),
      plotOutput("month_plot", height = 600)
      
    )
  )
)

server <- function(input, output) {
  
  chosen_data <- reactive({
    
    df <- data.frame(Month = airquality[["Month"]],
                     variable = airquality[[input[["chosen_option"]]]])
    df %>% group_by(Month) %>%  summarise(mean = mean(variable,na.rm = TRUE))
  
  })
  
  
  output[["month_plot"]] <- renderPlot({
    p <- ggplot(chosen_data(), aes_string(x = "Month", y = "mean")) +
      ylab(input[["chosen_option"]]) + 
      geom_line(color='darkblue') +
      theme_bw()
    
    p
  })
  
  
}

shinyApp(ui = ui, server = server)

