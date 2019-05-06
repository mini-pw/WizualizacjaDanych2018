library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_diet",
                         label = "Select diet:",
                         choices = unique(chickens[["Diet"]]),
                         selected = unique(chickens[["Diet"]])),
      
      sliderInput(inputId = "chosen_time", 
                  label = "Select time:",
                 value = chickens[["time"]],
                 min = 0,
                  max = 21)
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("chicken_plot", height = 600, brush = "chicken_brush"),
      textOutput("chicken_text"),
      h2("Table"),
      tableOutput("chickens_table")
    )
  )
)


server <- function(input, output) {
  
  diet_colors <- c()
  
  chickens_r <- reactive({
    
    filter(chickens, Diet %in% input[["chosen_diet"]])
    filter(chickens, Time %in% seq(0,input[["chosen_time"]]))
  })
  
  chickens_b <- reactive({
    
    filter(chickens_r(), Time > input[["chicken_brush"]][["xmin"]],
           Time < input[["chicken_brush"]][["xmax"]], 
           weight > input[["chicken_brush"]][["ymin"]],
           weight < input[["chicken_brush"]][["ymax"]]) 
  })
  
  
  
  output[["chicken_plot"]] <- renderPlot({
    p <- ggplot(chickens_r(), aes(x = Time, y = weight, color = Diet)) +
      geom_point() +
      #scale_color_manual(values = diet_colors[input[["chosen_diet"]]]) +
      theme_bw()
    
    p
  })
  
  output[["chickens_table"]] <- renderTable({
    validate(
      need(input[["chicken_brush"]], "Select at least one chicken")
    )
    chickens_b()
  })
  
  output[["chickens_text"]] <- renderText({
    validate(
      need(input[["chicken_brush"]], "Select at least one chicken")
    )
    
    chickens_b() %>% 
      nrow %>% 
      paste0("You have selected ", ., " chickens")
  })
  
  
}

shinyApp(ui = ui, server = server)
