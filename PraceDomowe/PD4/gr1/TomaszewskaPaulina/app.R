library(shiny)
library(dplyr)
library(ggplot2)

#dane
procenty <- c(29,20,25,11,0,15, 37,13,6,3,0,41,
              55,26,5,9,1,4, 39, 31, 6, 1, 5, 18)
zrodla <- c("Taxes that I pay", "Taxes that others pay", "Government's money","Taxes that companies pay", "Others", "I don't know")
partie <- factor(c("PiS", "Kukiz15", "PO", "Nowoczesna"))
df <- tibble(percent=procenty, answers=factor(rep(zrodla, 4), levels=zrodla),
             parties=rep(partie, each=6))




ui <- fluidPage(
  
  titlePanel("Source of money for 500+ programme"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_parties",
                         label = "Choose parties names:",
                         choices = unique(df[["parties"]]),
                         selected = unique(df[["parties"]]))
    ,
    
 
    
    selectInput(inputId = "plot_orientation",
                           label = "Choose plot orientation:",
                           choices = c("horizonal", "vertical"),
                           selected = c("horizontal"))

      ),

    
    mainPanel(
      h2("Barplot"),
      plotOutput("parties_plot", height = 600),
      h2("Table"),
      tableOutput("parties_table")
    )
  )
)

server <- function(input, output) {
  
  
  parties_r <- reactive({
    
    filter(df, parties %in% input[["chosen_parties"]]) 
  })
  

  
  output[["parties_plot"]] <- renderPlot({
    
    p<-ggplot(data=parties_r(), aes(x=parties, y=percent, fill=answers)) + 
      geom_bar(stat='identity', position='dodge', width=0.8) + 
      ggtitle("What do you think is the source of money for 500+ programme?") +
      scale_x_discrete(limits = levels(parties_r())) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      xlab('Political parties') + ylab('Percent of answers') +
      labs(fill = 'Answers') +
      theme(plot.title = element_text(hjust = 0.5))
      
    if(input[["plot_orientation"]] == "vertical") {
      p
      } 
    else {p+coord_flip()}
  
  })
  
  output[["parties_table"]] <- renderTable({
    parties_r()
  })
  
}

shinyApp(ui = ui, server = server)