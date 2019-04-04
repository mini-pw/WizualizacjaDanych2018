library(shiny)
library(dplyr)
data(airquality)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      inputId = "Var",
      selectInput(inputId = "selected_column", label = "Variable:", 
                  choices = c("Ozone", "Solar.R", "Wind", "Temp"),
                  selected = "Temp")
    ),
    
    
    mainPanel(
      h2("plot"),
      plotOutput("air_plot", height = 600)
    )
  ))


server <- function(input, output) {
  
  
  output[["air_plot"]] <- renderPlot({
    #browser()
    df <- airquality[c("Month", input[["selected_column"]])]
    colnames(df)[2] <- "variable"
    p <- df %>% na.omit %>% group_by(Month) %>% summarise(mean=mean(variable)) %>% 
      ggplot(aes(x = Month, y =mean)) +
      geom_bar(stat='identity') +
      theme_bw()
    
    p
  })
  
}

shinyApp(ui = ui, server = server)