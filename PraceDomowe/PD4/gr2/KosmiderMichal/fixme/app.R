library(ggplot2)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(gridExtra)

years = 2010:2017
consumption = c(39.9, 39.4, 42.5, 41.9, 44.3, 40.5, 42.3, 44.5)
sugar_price = c(2.73, 4.07, 3.96, 3.60, 2.50, 2.23, 2.87, 3.04)

sugar_in_Poland = data.frame(years, consumption, sugar_price)

ui <- fluidPage(
   
   titlePanel("Automatic FixMe"),
   
   sidebarLayout(
      sidebarPanel(
        materialSwitch("fixed", label = 'FixMe:', value = F, status = 'primary'),
        sliderInput("zoom", "Zoom:",
                    min = min(years) - 2000,
                    max = max(years) - 2000,
                    value = c(min(years) - 2000, max(years) - 2000))
      ),
      
      mainPanel(
         plotOutput("distPlot")
      )
   ),
   
   tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 950px; }"))
)

server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      if (input$fixed) {
        coords <- coord_cartesian(xlim = input$zoom + 2000)
        axis.x <- scale_x_continuous(breaks = years)
        
        p2 <- ggplot(sugar_in_Poland, aes(x = years, y = sugar_price)) +
          ggtitle("Sugar Price in Poland") +
          axis.x +
          coords +
          geom_point(size = 3) + geom_smooth(method = "loess", formula = y ~ x) +
          xlab("") +
          ylab("Sugar Price [PLN/kg]") + theme_minimal() 
        
        p1 <- ggplot(sugar_in_Poland, aes(x = years, y = consumption)) +
          ggtitle("Sugar Consumption in Poland") +
          axis.x +
          coords +
          geom_point(size = 3) + geom_smooth(method = "loess", formula = y ~ x) +
          xlab("") +
          ylab("Consuption p.p. [kg]") + theme_minimal() 
        
        p1 <- ggplotGrob(p1)
        p2 <- ggplotGrob(p2)
        
        p2$widths <- p1$widths
        
        grid.arrange(p1, p2, ncol=1, heights = c(1, 1))
      } else {
        min.x <- input$zoom[1] + 2000 
        max.x <- input$zoom[2] + 2000
        sugar_in_Poland %>% filter(min.x <= years & years <= max.x) %>% 
          ggplot(aes(x = years, 
                     y = sugar_price, 
                     fill = consumption)) +
            ggtitle("Sugar consumption and sugar prices in Poland (2010-2017)") +
            geom_col() + 
            xlab("") +
            ylab("Sugar price for kg (PLN)") +
            guides(fill=guide_legend(title="Consumption pp (kg)")) +
            theme_bw()
      }
   })
}

shinyApp(ui = ui, server = server)
