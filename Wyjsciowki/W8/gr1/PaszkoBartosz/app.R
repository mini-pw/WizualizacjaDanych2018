library(r2d3)
library(shiny)
library(SmarterPoland)
library(dplyr)


ui <- fluidPage(
  titlePanel("Praca Domowa 4"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "n_input",
                  label = "Liczba słupków:",
                  value = 10
      )
    ),
    
    mainPanel(
      h2("Plot"),
      d3Output("bar_plot") ##, height = 600)
    )
  )
)

server <- function(input, output) {
  output[["bar_plot"]] <- renderD3({
    n <- input[['n_input']]
    dat <- round(runif(n), 3)
    # print(dat)
    r2d3(
      data = dat,
      script = "./r2d3-example.js"
    )
  })
  
}

shinyApp(ui = ui, server = server)