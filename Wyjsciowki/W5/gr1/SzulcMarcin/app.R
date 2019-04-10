library(shiny)
library(dplyr)


ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_column",
                         label = "Select column:",
                         choices = c("Ozone", "Solar.R", "Wind"),
                         selected = "Ozone")
    ),
    mainPanel(
      h2("Airquality plot"),
      plotOutput("month_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  
  data_r <- reactive({
    
    airquality %>%
      group_by (Month) %>%
      na.omit() %>%
      summarize_at(.vars = input[["chosen_column"]], .funs = mean)
    
  })

  
  output[["month_plot"]] <- renderPlot({
    p <- ggplot(data_r(), aes_string(x = "Month", y = input[["chosen_column"]])) +
      geom_bar(stat="identity")
    p
  })
  
  
}
shinyApp(ui = ui, server = server)
