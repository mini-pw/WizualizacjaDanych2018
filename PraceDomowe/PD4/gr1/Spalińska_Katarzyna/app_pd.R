library(shiny)
library(SmarterPoland)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)
library(shinyWidgets)

#colnames = colnames(airquality)

hours_in_congestion = c(272, 254, 246, 237, 237, 227, 226, 
                        223, 218, 210, 202, 200, 199, 195, 
                        195, 190, 186, 181, 180, 173)

cities = c("Bogota", "Rzym", "Dublin", "Paryż",
           "Rostów nad Donem", "Londyn", "Mediolan", "Bordeaux",
           "Meksyk", "Moskwa", "Belo Horizonte", "Sankt Petersburg", 
           "Rio de Janeiro", "Florencja", "Bruskela", " Belfast",
           "Neapol", "Guadalajara", "Niżny Nowogród", "Warszawa")

countries = c("Kolumbia","Włochy","Irlandia","Francja","Rosja","Wlk.Brytania","Włochy","Francja","Meksyk",
              "Rosja","Brazylia","Rosja","Brazylia","Włochy","Belgia","Wlk.Brytania","Włochy","Meksyk","Rosja","Polska")

continents = c("Ameryka Południowa", "Europa", "Europa", "Europa", "Europa", "Europa", "Europa","Europa", "Ameryka Północna",
               "Europa", "Ameryka Południowa", "Europa", "Ameryka Południowa", "Europa","Europa", "Europa", "Europa", "Ameryka Południowa", "Europa", "Europa" )

traffic_data <- data.frame("city" = cities, "hours_in_congestion" = hours_in_congestion, "country" = countries, "continent" = continents, stringsAsFactors = FALSE)

mycolors = c("lightsalmon4","darkgreen","palegreen3","khaki2","orange","pink2","violetred4","darkblue","dodgerblue3","lightblue")


ui <- fluidPage(
  
  titlePanel("Praca domowa 4"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput("chosen_option_1",
                  "Wybierz kraje", 
                  choices=unique(traffic_data[["country"]]), 
                  selected=unique(traffic_data[["country"]]), 
                  options = list(`actions-box` = TRUE),
                  multiple = T),
      
      h4("Lub"),
      
      radioButtons('chosen_option_2', 'Wybierz cały kontynent', 
                   choices=c(c("Wszystkie"), unique(traffic_data[["continent"]])), 
                   selected = "Wszystkie"),
      
      radioButtons('sort', 'Sortowanie', 
                   choices=c("Rosnąco", "Malejąco")), 
                   selected="Malejąco"
    ),
    
    mainPanel(
      h2("Liczba godzin spędzonych w korkach w najbardziej zatłoczonych miastach świata"),
      plotOutput("cities_plot", height = 600)
      
    )
  )
)

server <- function(input, output) {
  
  countries_colors = c(Polska = "#8073ac", Włochy = "#f46d43",Belgia = "#fdae61",
                       Irlandia = "#fee08b",Rosja = "#d53e4f",Meksyk = "#e6f598",Kolumbia = "#abdda4",
                       Wlk.Brytania = "#66c2a5", Brazylia = "#3288bd",Francja = "#542788")
  
  
  chosen_countries <- reactive({
    
    if (input$sort == "Malejąco")
    {
      permut <- order(traffic_data$hours_in_congestion, decreasing = FALSE)
    }
    else
    {
      permut <- order(traffic_data$hours_in_congestion, decreasing = TRUE)
    }
    
    traffic_data$city <- factor(traffic_data$city, levels = traffic_data$city[permut])
    
    if (input$chosen_option_2 == "Wszystkie") {
      filter(traffic_data, country %in% input[["chosen_option_1"]]) 
      #print("1")
    }
    else {
      filter(traffic_data, continent %in% input[["chosen_option_2"]]) 
      #print("2")
    }
    
    
  })
  
  output[["cities_plot"]] <- renderPlot({
    
    p <- ggplot(chosen_countries(), aes(x = city, y = hours_in_congestion, fill = country)) +
      geom_bar(stat = "identity") +
      labs(y = "Liczba godzin spędzonych w korkach [h]", x = "", fill = "Kraj") +
      geom_text(aes(label = hours_in_congestion), hjust = 1.2) +
      scale_y_continuous(expand = c(0.01, 0)) +
      scale_fill_manual(values = countries_colors) +
      coord_flip()
    p
  })
  
  
}

shinyApp(ui = ui, server = server)

