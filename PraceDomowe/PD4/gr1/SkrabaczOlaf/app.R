#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(plotly)

jornada <- seq(10, 24)
barcelona <- c(21, 24, 24, 25, 28, 31, 34, 37, 40, 43, 46, 49, 50, 51, 54)
atletico <- c(19, 20, 23, 24, 25, 28, 31, 34, 35, 38, 41, 44, 44, 44, 47)
real <- c(14, 17, 20, 20, 23, 26, 29, 30, 30, 33, 36, 39, 42, 45, 45)
 
puntos.data <- data.frame(jornada, barcelona, atletico, real)

puntos.long <- melt(puntos.data, id = "jornada", measure = c("barcelona", "atletico", "real"))
puntos.long

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Praca Domowa 4 - Olaf Skrabacz"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("jornada",
                     "Choose jornadas",
                     min = 10,
                     max = 24,
                     value = c(10, 24)),
         checkboxGroupInput("teams",
                            h3("Wybierz drużyny"),
                            choices=list("Atletico" = "atletico",
                                         "Barcelona" = "barcelona",
                                         "Real" = "real"),
                            selected=c("atletico","barcelona", "real"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("pointsPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$pointsPlot <- renderPlotly({
  
     data_chart <- puntos.long %>% filter(variable %in% input$teams &
            jornada >= input$jornada[1] & jornada <= input$jornada[2])
     my_plot <- ggplot(data = data_chart, aes(jornada, value, colour = variable, label = value)) + # Skąd i co rysować
       geom_point() + # Rysuj punkty
       geom_line(alpha=0.4) + # Rysuj linie
       scale_x_continuous(breaks = seq(input$jornada[1], input$jornada[2], by=1),
                          limits=c(input$jornada[1],input$jornada[2])) + # Oś X podpisana co 1
       #scale_y_continuous(breaks = seq(0, 60, by=10), limits=c(0,60)) +
       labs(title="Points for top 3 teams in La Liga (after 24th match day)", x="Match day", y="Points", color="Team") + # Zmiana oznaczeń osi i legendy
       scale_color_manual(labels = c("FC Barcelona", "Atletico Madrid", "Real Madrid"), values=c("purple", "deepskyblue1", "black")) + # Zmiana oznaczeń drużyn
       theme_bw() # Wybór stylu```
     my_plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

