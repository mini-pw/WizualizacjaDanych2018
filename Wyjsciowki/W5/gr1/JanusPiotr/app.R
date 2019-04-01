head(airquality)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_column",
                         label = "Select variable:",
                         choices = c("Ozone","Solar.R", "Wind","Temp"),
                         selected = 'Ozone')
    ),
    mainPanel(
      h2("Barplot"),
      plotOutput("air_plot", height = 600)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  air_r <- reactive({
    
    data_to_plot <- airquality[, c(input[["chosen_column"]],'Month')]
    names(data_to_plot) <- c('val', 'Month')
    data_to_plot <- data_to_plot %>% na.omit() %>%  group_by(Month) %>%  summarize(out = mean(val))
  })

  
  output[["air_plot"]] <- renderPlot({
    p <- ggplot(air_r(), aes(x = Month, y = out)) +
      geom_col() +
      theme_minimal()+
      ylab(input[['chosen_column']])
    p
  })
  
}


shinyApp(ui = ui, server = server)
