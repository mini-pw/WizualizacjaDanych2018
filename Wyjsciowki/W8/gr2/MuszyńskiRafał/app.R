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
  titlePanel("W8 Muszyński Rafał"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 30,
                  value = 15)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # plotOutput("distPlot")
      d3Output("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <-reactive({
    round(runif(n = input[['bins']],0,1), digits = 3);
  })
  
  output$distPlot <- renderD3({
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    r2d3(data = data(),
         script = "./r2d3-example.js")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

