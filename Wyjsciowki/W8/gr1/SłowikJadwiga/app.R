library(shiny)
library(r2d3)

#r2d3(
#  data = c (0.3, 0.6, 0.8, 0.95, 0.40, 0.20, 0.40, 0.4, 0.5),
#  script = "./r2d3-example.js")

ui <- fluidPage(
  
  titlePanel("Wyjściówka"),
  
  sidebarLayout(
    numericInput(inputId = "chosen_n",
                 label = "Input n",
                 value="8"),
    d3Output("plot")
  )
)

server <- function(input, output) {
  
  n_r <- reactive({
    round(runif(input[["chosen_n"]], 0, 1), 3)
  })
  
  output[["plot"]] <- renderD3({
    r2d3(
      data = n_r(),
      script = "./r2d3-example.js")
  })
  
}

shinyApp(ui = ui, server = server)

# .libPaths()
#g2r