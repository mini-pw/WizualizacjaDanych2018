library(shiny)
library(dplyr)
library(ggplot2)

x <- c("Estonia","Malta","Austria","Polska","Dania","Litwa","Bułgaria","Szwecja","Niemcy","Francja",
       "Włochy","Portugalia","Hiszpania","Cypr","Wlk. Brytania","Grecja")
y <- as.numeric(c(14.2,19.9,26,12.7,23.6,12,9.6,22.2,24.0,23.7,18.6,12.9,17.9,19.7,20.2,10.5))
dane<- data.frame(x,y)
orders <- dane %>%arrange(desc(y))%>%pull(x)

ui <- fluidPage(
  
  titlePanel("Wizualizacja danych -- praca domowa nr 4"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput(inputId="bar_values_visibility", label="Widoczne wartości słupków", value = FALSE),
      selectInput(inputId="bar_color", label="Kolor słupka", choices=c("Niebieski", "Czerwony", "Zielony", "Liliowy", "Różowy"))
    ),
    mainPanel(
      h2("Wykres"),
      plotOutput("plot", width=800, height=500)
    )
  )
)

server <- function(input, output) {
  
  color_r <- reactive({
    switch(input[["bar_color"]],
      "Niebieski"={"#578CB5"},
      "Czerwony"={"#DC143C"},
      "Zielony"={"#2e8b57"},
      "Liliowy"={"#DD9ECD"},
      "Różowy"={"#FF00FF"})
    })
  
  output[["plot"]] <- renderPlot({
    base_plot <- ggplot(dane, aes(x=x, y=y, fill=factor(1))) +
      geom_col() +
      scale_x_discrete(limits=orders) +
      scale_fill_manual(values=c(color_r()))
    
    if(input[["bar_values_visibility"]] == TRUE) {
      base_plot <- base_plot  + geom_text(aes(label=formatC(dane$y, digits = 1, format = 'f')),
                                          colour ="White",
                                          fontface="bold", 
                                          size = 5,
                                          hjust = 1.2,
                                          vjust = 0.4,
                                          angle = 90)
    }
    
    base_plot +
      ggtitle("Wysokość przeciętnego dochodu do dyspozycji dla krajów UE w 2017r.") +
      labs(y="Wysokość dochodu w tys", x=' ') + 
      scale_y_continuous(breaks = seq(0, 28, 3)) +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size = 15),
            legend.position = "none")
  })
  
}

shinyApp(ui = ui, server = server)
