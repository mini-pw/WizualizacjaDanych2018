library(shiny)
library(ggplot2)
library(dplyr)

console <- c("Nintendo Switch", "PlayStation 4", "Nintendo 3DS", "PS Vita", "Xbox One")
consoles_sold <- c(3482388, 1695227, 566420, 181728, 15339)
consoles_sold_pr <- round((consoles_sold/sum(consoles_sold))*100,digits = 1)
df<- data.frame(console, consoles_sold,consoles_sold_pr)
df


ui <- fluidPage(
  
  titlePanel("Sprzedaz konsoli w Japonii w 2018r"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_konsoles",
                         label = "Select konsoles names:",
                         choices = unique(df[["console"]]),
                         selected = unique(df[["console"]]))
    ),
  
    mainPanel(
    h2("Wykres slupkowy"),
    plotOutput("konsoles_plot", height = 600, brush = "konsoles_brush"),
    h2("Table"),
    tableOutput("konsoles_table")
    
    )
  )
)

server <- function(input, output) {
  
  selected_konsoles <- reactive(
    filter(df, console %in% input[["chosen_konsoles"]]) 
  )
  cbp2 <- c("Nintendo Switch" ="#E69F00","PlayStation 4" = "#D55E00","Nintendo 3DS"=  "#56B4E9",
            "PS Vita" = "#009E73","Xbox One" = "#F0E442")


  
  output[["konsoles_plot"]] <- renderPlot({
    ggplot(selected_konsoles(), aes(x=console,y =consoles_sold_pr))+ 
      geom_text(aes(label=paste0(consoles_sold_pr,"%")), vjust=-0.3, size=8)+
      geom_bar(stat = "identity", fill= cbp2[input[["chosen_konsoles"]]]) + 
      scale_x_discrete(limits = console)+
      ggtitle("Sprzedaz konsoli w Japonii w 2018 r.")+
      labs(y="Procent sprzedanych konsoli", x = "Konsola", fill='Rodzaje konsoli')+ 
      theme(axis.text.x=element_text(size = 15),axis.text.y=element_text(size = 15),
            legend.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 13))
    
  })
  
  output[["konsoles_table"]] <- renderTable({
    validate(
      need(input[["chosen_konsoles"]], "Select at least one console")
    )
    selected_konsoles()
  })
  
}

shinyApp(ui = ui, server = server)

