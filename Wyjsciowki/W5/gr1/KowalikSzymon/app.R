library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Air Quality"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_input",
                         label = "Select data:",
                         choices = colnames(airquality)[1:4])
    ),
    
    mainPanel(
      plotOutput("air_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  air_filtered <- reactive({
    airquality %>% 
      group_by(Month) %>% 
      summarise_all(funs(mean(., na.rm=TRUE)))
      #select(Month, input[["chosen_input"]])
  })
  
  output[["air_plot"]] <- renderPlot({
    p <- ggplot(air_filtered(), aes(x = Month, y = get(input[["chosen_input"]]))) +
      ylab(input[["chosen_input"]]) +
      geom_col() +
      theme_bw()
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
