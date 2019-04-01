library(shiny)
library(tidyverse)

ui <- fluidPage(
  
  titlePanel("Wyjsciowka 5"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_variable",
                         label = "Select y variable:",
                         choices = unique(c('Ozone', 'Solar.R', 'Wind')),
                         selected = unique(c('Ozone')))
    ),
    
    mainPanel(
      h2("wykresik"),
      plotOutput("air_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  data <- airquality %>%
    na.omit() %>%
    group_by(Month) %>%
    summarise(
      Ozone = mean(Ozone),
      Solar.R = mean(Solar.R),
      Wind = mean(Wind))
  output[["air_plot"]] <- renderPlot({
    p <- ggplot(data, aes(x = Month)) +
      geom_point(aes_string(y = input$chosen_variable)) +
      theme_bw()
    
    p
  })
  

}

shinyApp(ui = ui, server = server)
