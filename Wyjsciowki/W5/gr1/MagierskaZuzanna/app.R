library(shiny)
library(dplyr)

colnames <- colnames(airquality)
colnames_1 <- colnames[! colnames %in% c("Day", "Month")]

data <- na.omit(airquality)
data <- data %>% group_by(Month) %>% summarise(
  Ozone = mean(Ozone),
  Solar.R = mean(Solar.R),
  Wind = mean(Wind),
  Temp = mean(Temp)
) 

ui <- fluidPage(
  
  titlePanel("Air Quality App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_column",
                         label = "Select column name:",
                        choices = unique(colnames_1),
                         selected = first(colnames_1))
    ),
    
    mainPanel(
      h2("Plot"),
      plotOutput("air_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  output[["air_plot"]] <- renderPlot({
    
    p <- ggplot(data, aes_string(x = "Month", y = input[["chosen_column"]])) +
      geom_line() +
      theme_bw()
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
