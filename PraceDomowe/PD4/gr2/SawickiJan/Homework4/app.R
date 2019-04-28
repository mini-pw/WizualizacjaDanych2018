# Source
# https://github.com/mini-pw/WizualizacjaDanych2018/pull/179/files?file-filters%5B%5D=.html#diff-457ed911f033e454c21423e68c848469

library("ggplot2")
library(dplyr)
library(forcats)
library(shiny)
library("shinyWidgets")


District <-
  c(
    'Bemowo',
    'Bialoleka',
    'Bielany',
    'Mokotow',
    'Ochota',
    'Praga-Poludnie',
    'Praga-Polnoc',
    'Rembertow',
    'Srodmiescie',
    'Targowek',
    'Ursus',
    'Ursynow',
    'Wawer',
    'Wesola',
    'Wilanow',
    'Wlochy',
    'Wola',
    'Zoliborz'
  )

InterventionCount <-
  c(177,
    295,
    623,
    578,
    532,
    838,
    564,
    88,
    1446,
    431,
    162,
    195,
    294,
    55,
    157,
    185,
    900,
    400)

Data = data.frame(District, InterventionCount)

Data = Data %>% mutate(District = factor(District, levels = rev(unique(District))))



ui <- fluidPage(
  titlePanel("Interventions of Warsaw City Guard"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "chosen_district",
        label = "Select districts names:",
        choices = unique(Data[["District"]]),
        selected = unique(Data[["District"]])
      ),
      sliderInput(
        "slider",
        label = "Select Intervention count range:",
        min = min(Data$InterventionCount),
        max = max(Data$InterventionCount),
        value = c(min(Data$InterventionCount), max(Data$InterventionCount))
      )
    ),
    
    mainPanel(
      plotOutput("plot", height = 600, click = "click"),
      verbatimTextOutput("plot_value"),
      textOutput("selected_var")
    )
  )
)

server <- function(input, output) {
  Data_r <- reactive({
    filter(
      Data,
      District %in% input[["chosen_district"]],
      InterventionCount >= input$slider[1],
      InterventionCount <= input$slider[2]
    )
  })
  
  output[["plot"]] <- renderPlot({
    ggplot(data = Data_r(),
           aes(x = District, y = InterventionCount, fill = InterventionCount)) +
      scale_x_discrete() +
      scale_y_continuous() +
      geom_bar(stat = 'identity') +
      labs(x = "", y = "") +
      coord_flip()
  })
}

shinyApp(ui = ui, server = server)
