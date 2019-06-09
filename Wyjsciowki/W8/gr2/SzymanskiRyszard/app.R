library(magrittr)
library(r2d3)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("D3 showcase"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 10,
                     value = 3)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         d3Output("barPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$barPlot <- renderD3({
    data = runif(input$bins) %>% round(2)
    r2d3(data = data,
         script = "./r2d3-example.js")
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)

