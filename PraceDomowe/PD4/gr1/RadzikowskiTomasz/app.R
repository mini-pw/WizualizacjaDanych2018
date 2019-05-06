#https://tomix.shinyapps.io/PracaDomowa4/



library(ggplot2)
library(shiny)
library(dplyr)
ceny<-c(3.50, 8.50, 0.5, 1.60, 0.4, 1.75, 0.85, 2.85, 2.5, 3.6, 0.6, 2.10, 1, 1.75, 2.20, 4.3, 9, 9.5)

warzywa <- data.frame(warzywo=c("pietruszka", "ziemniaki","kapusta biala", 'kapusta czerwona', 'kapusta kiszona', 'cebula', 'marchew','por','ogorek'),
                      cena2018=ceny[seq(1, length(ceny), 2)], cena2019=ceny[seq(2, length(ceny), 2)])
ui <- fluidPage(
  titlePanel("Ceny wybranych warzyw w roku 2018 i 2019"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_vege", 
                         label = "wybierz dane:",
                         choices = unique(warzywa[["warzywo"]]),
                         selected = warzywa[["warzywo"]][1:4]),
      width = 4
    ,

      selectInput(inputId = "year", 
                  label = "Rok:", 
                  choices = c("2018", "2019"))

    ),

    
    mainPanel(
      h2("Oto Twoj wykres"),
      br(),
      br(),
      p("Dzieki temu narzedziu mozesz porownac ceny warzyw."),
      plotOutput("wykres", height = 725)
      
  
  )
)
)

server <- function(input, output) {
  chosen <- reactiveValues(yaxis = "cena2018")
  
  observeEvent(input[["year"]], {
        if(input[["year"]] == "2018") {
      chosen[["yaxis"]] <- "cena2018"
      
      chosen[["color"]] <- "navyblue"
    }
    else if (input[["year"]] == "2019") {
      chosen[["yaxis"]]  <- "cena2019"
      
      chosen[["color"]] <- "orange"
    }
    
  })
  
  vegetypes <- reactive({
    filter(warzywa, warzywo %in% input[["chosen_vege"]]) 
  })
  
  output[["wykres"]] <- renderPlot({
    p <- ggplot(data = vegetypes(), aes(x = vegetypes()$warzywo)) +
      geom_bar(aes_string(y = chosen[["yaxis"]]), stat = "identity", fill = chosen[["color"]]) + #scale_y_discrete()+
      geom_text(aes_string(y = chosen[["yaxis"]], label = chosen[["yaxis"]]), vjust=1.6, color="white", size=6.5) +
      theme_minimal()+ylab("cena [PLN]")+xlab("warzywo")+theme(axis.text=element_text(size=14))
    
  p
  })
  
}

shinyApp(ui, server)
