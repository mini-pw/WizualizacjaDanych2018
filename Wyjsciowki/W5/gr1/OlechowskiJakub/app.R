library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Air quality by features"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_quality",
                         label = "Select feature name:",
                         choices = names(airquality)[1:4])
    ),
    
    mainPanel(
      h2("Plot"),
      plotOutput("air_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  output[["air_plot"]] <- renderPlot({
    
    df <- airquality %>% 
      na.omit() %>% 
      group_by(Month) %>% 
      summarise_all(mean)
    
    p <- ggplot(df, aes_string(x = "Month", y = input[['chosen_quality']])) +
      geom_bar(stat = 'identity') +
      theme_bw()
    
    p
  })
}

shinyApp(ui = ui, server = server)
