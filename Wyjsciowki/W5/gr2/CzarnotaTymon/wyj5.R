library(shiny)
library(SmarterPoland)
library(dplyr)

#View(ChickWeight)

ui <- fluidPage(
  titlePanel("Kurczaki są super"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_diet",
                         label = "Wybierz dietę kurczaka:",
                         choices = unique(ChickWeight[["Diet"]]),
                         selected = unique(ChickWeight[["Diet"]]))
    ),
    mainPanel(
      h2("Obejrzyj kurczaki"),
      plotOutput("chicken_plot",height = 600),
      sliderInput(inputId = "Time","Wybierz czas",min=0,max =21, value = 5)
    )
  )
)

server <- function(input, output) {
  
  chicken_r <- reactive({
    filter(ChickWeight, Diet %in% input[["chosen_diet"]]) 
  })
  
  chicken_t <- reactive(
    {
     filter(ChickWeight, Time > input[["Time"]]) 
    }
  )
  
  output[["chicken_plot"]] <- renderPlot({
    p <- ggplot(inner_join(chicken_r(),chicken_t()), aes(x = Time, y = weight, color = Diet)) +
      geom_point() +
      theme_bw()
    
    p
  })
  
}
shinyApp(ui = ui, server = server)