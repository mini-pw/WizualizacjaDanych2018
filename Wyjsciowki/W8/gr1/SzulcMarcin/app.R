data = data.frame(Population=sample(1:20,10),Households = sample(1:20,10), year=sample(c(2000,2010),10,replace=T))

ui <- fluidPage(
  numericInput("obs", "Observations:", 10, min = 1, max = 100),
  d3Output("d3")
)

server <- function(input,output){
  output$d3 <- renderD3({
    r2d3(data = round(runif(input[['obs']]), 2),
         script = "r2d3-example.js")
  })
}
shinyApp(ui=ui, server=server)