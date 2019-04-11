library(dplyr)
library(shiny)
library(SmarterPoland)
library(dplyr)
ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_column",
                         label = "Select column:",
                         choices = colnames(aq),
                         selected = unique(countries[["continent"]]))
    ),
    
    mainPanel(
      plotOutput("aq_plot")
    )
  )
)

server <- function(input, output) {
  

  aq_data <- reactive({
    aq <- airquality[c("Month", input[[ "chosen_column"]])]
    colnames(aq)[2] <- "column2"
    
    aq[is.na(aq)] <- 0
    aq
    aq %>% 
      group_by(Month) %>% 
      summarise(column2 = mean(column2))
  })
 
 
  #browser()
  output[["aq_plot"]] <- renderPlot({
    p <- ggplot(aq_data(), aes(x = Month, y = column2)) +
      geom_bar(stat = "identity")
    #browser()
    p
  })

}

shinyApp(ui = ui, server = server)
