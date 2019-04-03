library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_variable",
                         label = "Select variable:",
                         choices = names(airquality)[1:4],
                         selected = "Ozone"
                  )
    ),
    
    mainPanel(
      h2("Plot"),
      plotOutput("ozone_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  

  airquality_r <- reactive({
    
    aggregate(airquality, list(airquality$Month), mean, na.rm=TRUE) %>%
      select(input[["chosen_variable"]], Month) 
  })
  
  output[["ozone_plot"]] <- renderPlot({
    p <- ggplot(airquality_r(), aes_string(x = "Month", y = input[["chosen_variable"]])) +
      geom_point() +
      theme_bw()
    
    p
  })
}

shinyApp(ui = ui, server = server)