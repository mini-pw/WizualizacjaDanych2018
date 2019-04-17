library(shiny)
library(SmarterPoland)
library(dplyr)


ui <- fluidPage(
  
  titlePanel("Chickens"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_diet",
                         label = "Select diet:",
                         choices = unique(ChickWeight[["Diet"]]),
                         selected = unique(ChickWeight[["Diet"]])),
      sliderInput("time_slider",
                  label = 'Select time range',
                  min = min(ChickWeight$Time),
                  max = max(ChickWeight$Time),
                  value = c(min(ChickWeight$Time), max(ChickWeight$Time))
                  )
    ),
    
    mainPanel(
      h2("Chickens"),
      plotOutput("chicken_plot", height = 600, brush = "country_brush"),
      h3('Chickens')
    )
  )
)

server <- function(input, output) {
  chickens <- reactive({
    ChickWeight %>% filter(Time >= input[['time_slider']][1], 
                            Time <= input[['time_slider']][2],
                            Diet %in% input[['chosen_diet']])
  })
  
  output[["chicken_plot"]] <- renderPlot({
    
    p <- chickens() %>% 
        ggplot(aes(x = Time, y = weight, color = Diet)) +
        geom_point() +
        theme_bw()
    p
  })
}

shinyApp(ui = ui, server = server)
