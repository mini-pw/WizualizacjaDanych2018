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
   
   # Application title
   titlePanel("Panel dowodzenia"),
   d3Output('plot'),
   sliderInput('numb',label='Wybierz jakąś liczbę', value = 4, min =2, max = 10)
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$plot <- renderD3({
    r2d3(
      data = round(runif(input$numb),2),
      script = "r2d3-example.js")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

