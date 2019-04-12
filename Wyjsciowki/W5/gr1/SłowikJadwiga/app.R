library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Wyjściówka"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_value",
                         label = "Select Y axis",
                         choices = c("Ozone", "Wind", "Temp", "Solar.R"),
                         selected = c("Ozone")),
      selectInput(inputId = "chosen_type",
                  "Select plot type",
                  choices = c("barplot", "line"),
                  selected = c("barplot"))
    ),
    mainPanel(
      h2("Plot"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  d <- reactive({
    airquality %>%
      select("Month", y=input[["chosen_value"]]) %>% 
      group_by(Month) %>% 
      summarise(avg = mean(y, na.rm = TRUE))
  })
  
  output[["plot"]] <- renderPlot({
    p <- ggplot(d(), aes(x = Month, y = avg)) + 
      scale_y_continuous(input[["chosen_value"]])
    if(input[["chosen_type"]] == "line") {
      p + geom_line()
    } else {
      p + geom_col()
    }
  })
  
}

shinyApp(ui = ui, server = server)
