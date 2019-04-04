#----------------------------------------------------
#
# Wyjściówka nr 5
# 01.04.2019
#
#----------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)


airquality_col_names <- colnames(airquality)[1:4]


ui <- fluidPage(
   
   titlePanel("Wyjściówka nr 5 - Agata Pałdyna"),
   
   sidebarLayout(
     sidebarPanel(
       selectInput(inputId = "choosen_column",
                    label = "Wybierz kolumnę do wykresu:",
                    choices = airquality_col_names),
                    selected = "Ozone"),
     
     mainPanel(
       h2("Wykres"),
       plotOutput("data_plot", height = 600)
     )
  )
)


server <- function(input, output) {
  column_colors <- c(Ozone = "darkred", Solar.R = "orange", Wind = "burlywood4", Temp = "deepskyblue")
  
  airquality_r <- reactive({
    airquality %>% 
      group_by(Month) %>%
      summarise(Mean = mean(eval(parse(text = input[["choosen_column"]])), na.rm = TRUE)) %>%
      select(Month, Mean)
  })
  
  output[["data_plot"]] <- renderPlot({
    p <- ggplot(airquality_r(), aes(x = Month, y = Mean)) + 
      geom_bar(stat = "identity", fill = column_colors[input[["choosen_column"]]]) +
      ggtitle(paste0("Średnia wartość kolumny ", input[["choosen_column"]], " w zależności od miesiąca")) +
      xlab("Miesiąc") + ylab("Średnia wartość") +
      theme(plot.title = element_text(size = 15, face = "bold"),
            axis.text = element_text(size = 13),
            axis.title = element_text(size = 14, face = "bold"),
            legend.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 13))
    
    p
  })
}


shinyApp(ui = ui, server = server)

