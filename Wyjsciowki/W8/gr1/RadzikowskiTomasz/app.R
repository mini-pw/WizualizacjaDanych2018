#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(r2d3)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("wybierzmy sobie slupki"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 6,
                     value = 3)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         d3Output("distPlot")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderD3({
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      dane<-c (0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
      selected<-runif(input$bins, 0.0, 6.0)
      
      r2d3(
        data = sample(dane,input$bins),
        script = "./r2d3-example.js"
      )      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

