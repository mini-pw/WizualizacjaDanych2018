library(shiny)
library(SmarterPoland)
library(dplyr)

x <- c("Estonia","Malta","Austria","Polska","Dania","Litwa","Bułgaria","Szwecja","Niemcy","Francja",
       "Włochy","Portugalia","Hiszpania","Cypr","Wlk. Brytania","Grecja")
y <- as.numeric(c(14.2,19.9,26,12.7,23.6,12,9.6,22.2,24.0,23.7,18.6,12.9,17.9,19.7,20.2,10.5))
df = data.frame(country=x,avg_income=y)

ui <- fluidPage(
  
  titlePanel("Praca domowa 4"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_country",
                         label = "Wybierz kraj:",
                         choices = unique(df[["country"]]),
                         selected = unique(df[["country"]])),
      sliderInput(inputId = "chosen_avg_income_id",
                  label = "Wybierz dochód:",
                  min = min(df$avg_income), 
                  max = max(df$avg_income), 
                  step = 1,
                  value = c(min(df$avg_income), max(df$avg_income)))
    ),
    
    mainPanel(
      h2("Dochód krajów UE w 2017 roku"),
      plotOutput("countries_plot", height = 550)
    )
  )
)

server <- function(input, output) {
  
 
  df_r <- reactive({
    filter(df, country %in% input[["chosen_country"]], 
           avg_income >= input[["chosen_avg_income_id"]][1],
           avg_income <= input[["chosen_avg_income_id"]][2]) 
  })

  order_r <- reactive({ 
    filter(df, country %in% input[["chosen_country"]], 
                               avg_income >= input[["chosen_avg_income_id"]][1],
                               avg_income <= input[["chosen_avg_income_id"]][2])  %>% 
    arrange(avg_income) %>% 
    pull(country)})
  
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
  
  output[["countries_plot"]] <- renderPlot({
    p <- ggplot(df_r(), aes(x=country,y=avg_income)) + 
      geom_bar(stat='identity',fill='#e9cc7f') +
      geom_text(aes(label=specify_decimal(avg_income,1)),hjust=1.618) +
      scale_x_discrete(limits = order_r()) +
      labs(y='Dochód*', x=' ',caption='*Dochód wyrażony w tysiącach parytetu siły nabywczej',title='Wysokość przeciętnego dochodu do dyspozycji na osobę',subtitle='dla krajów UE za rok 2017') +
      coord_flip() +
      theme_bw() +
      theme(
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        legend.title = element_text(size=11,face='bold'),
        axis.title.x = element_text(size=11,face='bold'),
        axis.text = element_text(size=10,face='bold')
      )
    p
  })
 
  }
  
shinyApp(ui = ui, server = server)