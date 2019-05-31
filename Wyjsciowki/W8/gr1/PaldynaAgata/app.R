library(shiny)
library(r2d3)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Rysowanie wykresow słupkowych z wektora n losowanych liczb z przedziału 0-1"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("n",
                     "Number of random numbers:",
                     10)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         d3Output("barPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$value <- renderText({input$n})
   
  output$barPlot <- renderD3({
     r2d3(
       data = round(runif(input$n, 0, 1), 2),
       script = "r2d3-example.js"
     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

