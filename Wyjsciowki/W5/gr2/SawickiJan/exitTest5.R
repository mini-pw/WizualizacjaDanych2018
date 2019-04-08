library(shiny)
library(SmarterPoland)
library(dplyr)
head(ChickWeight)
ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_continent",
                         label = "Select continent names:",
                         choices = unique(ChickWeight[["Diet"]]),
                         selected = unique(ChickWeight[["Diet"]]))
    ),
    
    mainPanel(
      h2("Scatterplot"),
      sliderInput(inputId = "country_brush", 
                  min = as.numeric(min(ChickWeight$Time)), 
                  max = as.numeric(max(ChickWeight$Time)),
                  value = c(min(ChickWeight$Time),max(ChickWeight$Time)), 
                  label="SUPER SLIDER"),
      plotOutput("countries_plot", height = 600),
      h2("Table"),
      tableOutput("countries_table"),
      textOutput("countries_text")
    )
  )
)

server <- function(input, output) {
  
  continent_colors <- c(Asia = "red", Europe = "green", Africa = "orange", Americas = "black", 
                        Oceania = "blue")
  
  countries_r <- reactive({
    
    filter(ChickWeight, Diet %in% input[["chosen_continent"]], as.numeric(Time) > as.numeric(input[["country_brush"]][1]),
           as.numeric(Time) < as.numeric(input[["country_brush"]][2])) 
  })
  
  countries_b <- reactive({
    validate(
      need(input[["country_brush"]], "Select at least one country")
    )
    
    filter(countries_r(), as.numeric(Time) > as.numeric(input[["country_brush"]][1]),
           as.numeric(Time) < as.numeric(input[["country_brush"]][2])) 
  })
  
  output[["countries_plot"]] <- renderPlot({
    p <- ggplot(countries_r(), aes(x = Time, y = weight, color = Diet)) +
      geom_point() +
      # scale_color_manual(values = continent_colors[input[["chosen_continent"]]]) +
      theme_bw()
    
    p
  })
  
  output[["countries_table"]] <- renderTable({
    countries_b()
  })
  
  output[["countries_text"]] <- renderText({
    text = paste0("Number of countries is ", nrow(countries_b()))
    text
  })
  
}

shinyApp(ui = ui, server = server)
