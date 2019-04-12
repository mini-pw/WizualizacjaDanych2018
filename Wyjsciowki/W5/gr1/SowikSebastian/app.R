library(shiny)
library(SmarterPoland)
library(dplyr)

head(airquality)
airquality_colnames <- colnames(airquality)[1:4]
airquality_means <- airquality %>% group_by(Month) %>% summarise_all(funs(mean(., na.rm=TRUE))) 

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_feature",
                         label = "Select continent names:",
                         choices = unique(airquality_colnames),
                         selected = unique(airquality_colnames[1]))
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("countries_plot", height = 600, brush = "country_brush")#brush zaznaczanie obszaru
    )
  )
)

server <- function(input, output) {
  
  
  output[["countries_plot"]] <- renderPlot({
    
    validate(
      need(input[["chosen_feature"]], "Select at least feature")
    )
    feature_name <- input[["chosen_feature"]]
    p <- ggplot(airquality_means, aes_string(x = 'Month', y = feature_name )) +
      geom_bar(stat='identity') +
      theme_bw()
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
