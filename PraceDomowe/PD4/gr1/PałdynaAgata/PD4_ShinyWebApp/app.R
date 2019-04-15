library(shiny)
library(ggplot2)
library(dplyr)
library(scales)


official <- c(6.6, 5.6, 5.6, 5.3, 4.6, 4.3, 4.0, 4.2, 5.7, 5.8,
              5.7, 5.3, 4.7, 4.6, 5.0, 7.8, 9.8, 9.1, 8.3, 8.0,
              6.6, 5.7, 4.9, 4.8, 4.4, 4.0) / 100
real <- c(11.8, 10.2, 9.8, 9.4, 8.4, 7.7, 7.1, 7.3, 9.5, 10.0,
          9.9, 9.3, 8.4, 8.4, 9.2, 14.2, 16.7, 16.2, 15.2, 14.5,
          12.7, 11.3, 9.9, 9.4, 8.2, 8.1) / 100

unemployment <- data.frame(
  Year = c(c(1994:2019), c(1994:2019)),
  Rate = c(official, real),
  Type = c(rep(times = 26, "official"), rep(times = 26, "real"))
)

minYear <- min(unemployment[["Year"]])
maxYear <- max(unemployment[["Year"]])
unemploymentTypes <- unique(unemployment[["Type"]])


ui <- fluidPage(
   
   titlePanel("Unemployment rate in USA"),
   
   sidebarLayout(
      sidebarPanel(
         checkboxGroupInput(inputId = "chosen_type",
                           label = "Select type of data:",
                           choices = unemploymentTypes,
                           selected = unemploymentTypes),
        
         sliderInput("years",
                     "Select years range:",
                     min = minYear,
                     max = maxYear,
                     value = c(minYear, maxYear),
                     sep = "",
                     step = 1)
      ),
      
      mainPanel(
         plotOutput("unemploymentPlot")
      )
   )
)


server <- function(input, output) {
  
  unemployment_r <- reactive({
    filter(unemployment, Type %in% input[["chosen_type"]]) 
  })
   
   output$unemploymentPlot <- renderPlot({
     
     ggplot(data = unemployment_r(), aes(x = Year, y = Rate, group = Type)) +
       geom_line(aes(color = Type)) +
       geom_point(aes(color = Type)) +
       xlim (input[["years"]]) + 
       #ylim(c(0, 18)) +
       scale_y_continuous(labels = percent) +
       theme_minimal() +
       theme(axis.text = element_text(size = 14),
             axis.title = element_text(size = 16, face = "bold"),
             legend.title = element_text(size = 16, face = "bold"),
             legend.text = element_text(size = 14),
             legend.position = "right")
   })
}


shinyApp(ui = ui, server = server)
