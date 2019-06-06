#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
   
   titlePanel("Wyjsciowka 8"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Liczba losowanych wartosci:",
                     min = 1,
                     max = 10,
                     value = 5)
      ),
      
      mainPanel(
        d3Output("distPlot")
      )
   )
)

server <- function(input, output) {
   
  output$distPlot <- renderD3({
      
     generated_data <- round(runif(input$bins, 0, 1 ),  digits = 2)
    
     r2d3(data = generated_data,
            script = "r2d3-example.js")
     })
}

shinyApp(ui = ui, server = server)

