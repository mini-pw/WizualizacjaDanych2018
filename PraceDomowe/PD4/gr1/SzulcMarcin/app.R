library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(shiny)
library(RColorBrewer)

countries <- c('Estonia', 'The Netherlands', 'Estonia', 'Romania',
               'The Netherlands', 'Poland', 'Slovakia',
               'Spain', 'Slovakia', 'Poland', 'Hungary',
               'Switzerland', 'Hungary', 'Romania', 'Switzerland', 
               'Bulgaria', 'Spain', 'Bulgaria', 'Kosovo',
               'Albania', 'Macedonia', 'Macedonia')

cities <- c('Tallin', 'Amsterdam', 'Tartu', 'Pitesti', 
            'Sint Maartensbrug', 'Warsaw', 'Trencin', 
            'Aviles', 'Bratislava', 'Cracow', 'Sajoszentpeter',
            'Bern', 'Budapest', 'Bucarest', 'Lugano', 
            'Vidin', 'Madrid', 'Sofia', 'Dramjak', 
            'Korce', 'Tetovo', 'Skopje')

days <- c(4, 4, 3, 32, 22, 86, 35, 127, 23, 164, 76, 2, 46,
          41, 9, 166, 20, 71, 67, 61, 293, 162)

annual_concentration <- c(15.7, 22.5, 16.8, 33.9, 27.6, 41.6,
                          29.4, 45.8, 29.1, 56.7, 35.9, 19.3,
                          33.5, 35.5, 19.9, 61.1, 24.0, 40.0,
                          33.7, 40.2, 97.3, 84.1)

smog_data <- data.frame(countries, cities, days, annual_concentration)

smog_data$cities <- paste(cities, " (", countries, ")", sep="")

cities_order_days <- smog_data %>%
  arrange(days) %>% 
  pull(cities)

cities_order_concentration <- smog_data %>%
  arrange(annual_concentration) %>% 
  pull(cities)

# spliting dataset into 2 ordered datasets (order by 'days' and by 'annual_concentration' to speedup rendering)
smog_data$cities_days_order <- factor(smog_data$cities, levels = cities_order_days)
smog_data$cities_concentration_order <- factor(smog_data$cities, levels = cities_order_concentration)

smog_data_days <-  smog_data[order(-smog_data$days), ]
smog_data_days <-  smog_data_days[, c("countries", "days", "cities", "cities_days_order")]

smog_data_concentration <-  smog_data[order(-smog_data$annual_concentration),]
smog_data_concentration <-  smog_data_concentration[,  c("annual_concentration", "countries", "cities", "cities_concentration_order")]

# fixing colors
dd <- union(smog_data_days$countries, smog_data_concentration$countries)
dd.col <- brewer.pal(name = "Paired", n = 12)
names(dd.col) <- dd


get_days_plot <- function(data){
  return(ggplot(data = data, aes(x = cities_days_order, y = days, fill = countries)) + 
    scale_y_continuous(limits = c(0, 310)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    geom_text(aes(label = days), size=3, hjust = -0.3) +
    geom_abline(aes(slope = 0, intercept = 35,  col = "EU limit of days"), lty = 2) + 
    theme(axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          aspect.ratio = 1,
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_line(size = 0.3, colour = "#F0F0F0"),
          panel.grid.major = element_line(size = 0.3, colour = "#F0F0F0"),
          panel.background = element_blank(),
          plot.title = element_text(family = 'Helvetica', 
                                    color = 'black', 
                                    face = 'bold', 
                                    size = 16, 
                                    hjust = 0.5),
          plot.margin = unit(c(0,0,0.5,0), "cm")) + 
    scale_fill_manual(values = dd.col) + 
    labs(title=expression("Number of days exeeding concentration level above 50μg/"*m^3)))

}

get_concentration_plot <- function(data){
  return(ggplot(data = data, aes(x = cities_concentration_order, y = annual_concentration, fill = countries)) + 
           scale_y_continuous(limits = c(0, 103)) + 
           geom_bar(stat = "identity") + 
           coord_flip() + 
           geom_text(aes(label = annual_concentration), size=3, hjust = -0.3) +
           geom_abline(aes(slope = 0, intercept = 35,  col = "EU annual concentration limit"), lty = 2) + 
           theme(axis.ticks.x = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.title.y = element_blank(),
                 axis.title.x = element_blank(),
                 aspect.ratio = 1,
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor = element_line(size = 0.3, colour = "#F0F0F0"),
                 panel.grid.major = element_line(size = 0.3, colour = "#F0F0F0"),
                 panel.background = element_blank(),
                 plot.title = element_text(family = 'Helvetica', 
                                           color = 'black', 
                                           face = 'bold', 
                                           size = 16, 
                                           hjust = 0.5),
                 plot.margin = unit(c(0,0,0.5,0), "cm")) + 
           scale_fill_manual(values = dd.col) + 
           labs(title=expression('PM'[10]~"annual concentration")))
  
}

ui <- fluidPage(
  
  titlePanel("Smog in EU cities"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "column",
                  label = "Choose a data:",
                  choices = c("Number of days exeeding EU limit", "PM10 annual concentration")),
      
      numericInput(inputId = "obs",
                   label = "Number of cities to view:",
                   value = 7)
    ),
    
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  datasetColumn <- reactive({
    switch(input$column,
           "Number of days exeeding EU limit" = "cities_days_order",
           "PM10 annual concentration" = "cities_concentration_order")
  })
  

  output$plot <- renderPlot({
    
    data_and_plot = switch(input$column,
           "Number of days exeeding EU limit" = list(data = smog_data_days, plot_fun = get_days_plot),
           "PM10 annual concentration" = list(data = smog_data_concentration, plot_fun = get_concentration_plot))
    
    data  <- head(data_and_plot$data, n = input$obs)
    
    data_and_plot$plot_fun(data)
  }, width = 700, height = 400)
}

shinyApp(ui, server)
