library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_feature",
                   label = "Select feature name:",
                   choices = c("Ozone", "Solar.R", "Wind", "Temp"))
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("plot", height = 600),
      h2("Table"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  
  my_data <- airquality %>% 
    group_by(Month) %>% 
    summarize(Ozone = mean(Ozone, na.rm = TRUE),
              Wind = mean(Wind, na.rm = TRUE),
              Temp = mean(Temp, na.rm = TRUE),
              Solar.R = mean(Solar.R, na.rm = TRUE)) %>% 
    mutate(Month = month.abb[Month])
  
  data_b <- reactive({
    my_data %>% select(Month, input[["chosen_feature"]])
  })
  
  output[["plot"]] <- renderPlot({
    p <- ggplot(data_b(), aes(x = Month, y = get(input[["chosen_feature"]]))) +
      geom_point() +
      theme_bw() + 
      labs(y = input[["chosen_feature"]])
    
    p
  })
  
  output[["table"]] <- renderTable({
    data_b()
  })
  
}

shinyApp(ui = ui, server = server)
