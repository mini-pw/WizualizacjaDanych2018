#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)


x <- c("Estonia","Malta","Austria","Polska","Dania","Litwa","Bułgaria","Szwecja","Niemcy","Francja",
       "Włochy","Portugalia","Hiszpania","Cypr","Wlk. Brytania","Grecja")
y <- as.numeric(c(14.2,19.9,26,12.7,23.6,12,9.6,22.2,24.0,23.7,18.6,12.9,17.9,19.7,20.2,10.5))
dane<- data.frame(x,y)

dane <- data.frame(x=x,y=y, stringsAsFactors = FALSE)
orders <- dane %>% arrange(desc(y))
orders$x <- factor(orders$x, levels = orders$x)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Sidebar with a slider input for number of bins 
   dashboardPage(
     dashboardHeader(title = "Praca domowa 4"),
     dashboardSidebar(sliderInput("range_slider", label = h3("Zakres wartości"), min = 0, max = 30, value = c(10, 20)),
                      selectInput("color", label = h3("Kolor wykresu"), selected = 'darkgreen', choices = c("Ciemny zielony"='darkgreen', 
                                                                                                            "Ciemny niebieski"='darkblue', 
                                                                                                            "Fioletowy"='purple',
                                                                                                            "Czerwony"='red')),
                      selectInput("czcionka", h3("Rozmiar tekstu"),
                                  choices = c(8,10,12,14,16,18),  selected = 12)),
     dashboardBody(
       fluidRow(
         column(width = 8, box(
           title = "Wykres", width = 12, status = "primary",
           plotOutput("barPlot")
         )),
         column(width = 4, box(
           title = "Top 5 na wykresie", width = 12, status = "info" , dataTableOutput("table")))
         )
       )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  current_val <- reactive({
    orders %>%  filter(y>input$range_slider[1] , y<input$range_slider[2])
  })

    output$table <- renderDataTable({
      validate(
        need(dim(current_val())[1] != 0, 'Wygląda na to, że nie ma danych do wyświetlenia')
      )
      datatable(head(current_val() %>% select(x,y), n=5),selection = 'none',colnames=c("Kraj", "Dochód (tys)"), options = list(dom = 't'))
    })
  
   output$barPlot <- renderPlot({
     validate(
       need(dim(current_val())[1] != 0, 'Wygląda na to, że nie ma danych do wyświetlenia')
     )
     data <- current_val()
     font_size <- as.numeric(input$czcionka)
     g <- ggplot(data = data, aes(x = x, y = y)) +
       geom_col(fill = input$color, width = 0.9)+
       scale_y_discrete(limits = seq(0,max(data$y)+2,4), expand = c(0,0))+
       coord_cartesian(ylim = c(0, round(max(data$y)+2)))+
       geom_text(aes(label=formatC(current_val()$y, digits = 1, format = 'f')),
                 colour ="White", fontface="bold", 
                 size = font_size/3, hjust =0.5,
                 vjust = 1.5) +
       theme_bw() +
       theme(panel.grid.major.x = element_blank(),
             plot.title = element_text(face='bold',size = font_size, hjust = 0.5, margin = margin(0,0,20,0)),
             axis.text.x = element_text(angle = 90, face='bold',size=font_size, hjust = 1, vjust=0.25),
             axis.text.y = element_text(face="bold", size=font_size),
             axis.title.y = element_text(size=font_size, margin = margin(0,10,0,0)),
             panel.border = element_blank(),
             axis.line = element_line(size = 0.7),
             axis.ticks.x = element_blank())+
       ggtitle("Wysokość przeciętnego dochodu do dyspozycji dla krajów UE w 2017r.") +
       labs(y="Wysokość dochodu w tys", x=' ') 

     g$elementId <-NULL
     g
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

