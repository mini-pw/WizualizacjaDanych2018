library(shiny)
library(SmarterPoland)
library(dplyr)
chickens <- ChickWeight

ui <- fluidPage(
  
  titlePanel("Chickens' diets"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "choose_diet",
                         label = "Select diet:",
                         choices = unique(chickens[["Diet"]]),
                         selected = unique(chickens[["Diet"]])),
      
      sliderInput(inputId = "choose_time", 
                  label = "Select time:",
                  value = c(min(chickens["Time"]),max(chickens["Time"])),
                  min = min(chickens["Time"]),
                  max = max(chickens["Time"]))
    ),
    
    mainPanel(
      h2("Chickens"),
      plotOutput("chicken_plot", height = 600, brush = "chicken_brush")
    )
  )
)


server <- function(input, output) {
  
  chickens_r <- reactive({
    filter(chickens, Diet %in% input[["choose_diet"]] & Time %in% seq(input[["choose_time"]][1],input[["choose_time"]][2]))
  })
  
  chickens_b <- reactive({
    
    filter(chickens_r(), Time > input[["chicken_brush"]][["xmin"]],
           Time < input[["chicken_brush"]][["xmax"]], 
           weight > input[["chicken_brush"]][["ymin"]],
           weight < input[["chicken_brush"]][["ymax"]]) 
  })
  
  
  
  output[["chicken_plot"]] <- renderPlot({
    p <- ggplot(chickens_r(), aes(x = Time, y = weight, color = Diet, group=Chick)) +
      geom_point() +
      geom_line(alpha = 0.2) +
      theme_bw()
  
    p
  })

  
  
}

shinyApp(ui = ui, server = server)