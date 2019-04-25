library(ggplot2)
library(ggthemes)
library(extrafont)
library(shiny)
library(dplyr)

options(encoding = "UTF-8")

Name <- c("Eisenbichler M.", "Geiger K.", "Peier K.", "Kobayashi R.", "Stoch K.", 
          "Kraft S.", "Forfang J.", "Johansson R.", "Freitag R.", "Zajc T.", 
          "Huber D.", "Kubacki D.", "Aschenwald P.", "Hayboeck M.", "Ammann S.", 
          "Prevc P.", "Kobayashi J.", "Klimov E.", "Zyla P.", "Ito D.", "Sato Y.", 
          "Koudelka R.", "Jelar Z.", "Fettner M.", "Polasek V.", "Schuler A.", 
          "Boyd-Clowes M.", "Learoyd J.", "Stjernen A.", "Zografski V.")

Nationality <- as.factor(c("GER", "GER", "SUI", "JPN", "POL", "AUT", "NOR", "NOR", "GER", 
                           "SLO", "AUT", "POL", "AUT", "AUT", "SUI", "SLO", "JPN", "RUS", 
                           "POL", "JPN", "JPN", "CZE", "SLO", "AUT", "CZE", "SUI", "CAN", 
                           "FRA", "NOR", "BUL"))

Distance1 <- c(131.5, 131.0, 131.0, 133.5, 128.5, 130.0, 132.5, 128.0, 125.5, 127.0, 126.0,
           128.5, 120.0, 122.0, 122.5, 123.5, 116.0, 126.5, 128.5, 119.0, 120.0, 120.5, 
           118.5, 117.5, 120.0, 117.5, 117.0, 116.5, 124.5, 117.0)

Distance2 <- c(135.5, 130.5, 129.5, 126.5, 129.5, 126.5, 125.5, 129.0, 129.5, 124.0, 125.5, 
           125.5, 128.0, 125.5, 126.0, 122.0, 132.0, 121.0, 121.0, 126.0, 124.0, 120.5, 
           121.0, 122.5, 117.5, 119.0, 118.5, 116.5, 102.0, 0)

Punkty <- c(279.4, 267.3, 266.1, 262.0, 259.4, 256.1, 250.9, 248.9, 248.7, 245.5, 242.0, 
           240.2, 239.9, 233.7, 230.6, 230.5, 230.0, 229.1, 228.7, 225.7, 221.4, 220.1, 
           219.8, 219.0, 218.2, 212.6, 212.1, 205.9, 185.5, 106.1)

MS2019_scores <- data.frame(Name, Nationality, Distance1, Distance2, Punkty)

Nationalities <- as.factor(c("GER", "SUI", "JPN", "POL", "AUT", "NOR", "SLO", "RUS", "CZE", "CAN", "FRA", "BUL"))

Images <- c("http://icons.iconarchive.com/icons/wikipedia/flags/128/DE-Germany-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/CH-Switzerland-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/JP-Japan-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/PL-Poland-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/AT-Austria-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/NO-Norway-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/SI-Slovenia-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/RU-Russia-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/CZ-Czech-Republic-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/CA-Canada-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/RE-Reunion-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/BG-Bulgaria-Flag-icon.png"
)
Flags <- data.frame(Nationalities, Images)

data = merge(MS2019_scores, Flags, by.x = "Nationality", by.y = "Nationalities")

ui <- fluidPage(
  
  titlePanel("Wyniki mistrzostw świata w skokach narciarskich 2019 \nkonkurs indywidualny na skoczni dużej"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_country",
                         label = "Wybierz narodowość:",
                         choices = unique(data$Nationality),
                         selected = unique(data$Nationality)
                         ),

      selectInput("type", "Wybierz co ma przedstawiać wykres:",
                  c("Suma punktów",
                    "Dystans w 1. skoku",
                    "Dystans w 2. skoku")
                  )
    ),
    
    
    
  mainPanel(
      h2("Scatterplot"),
      plotOutput("plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  results_r <- reactive({
    filter(data, Nationality %in% input[["chosen_country"]]) %>% arrange(Punkty)  #TODO: posortować
  })
  
  label <- reactive({
    input[["type"]]
  })
  
  to_plot <- reactive(({
    if (input[["type"]] == "Dystans w 1. skoku") {
      "Distance1"
    } else if (input[["type"]] == "Dystans w 2. skoku") {
      "Distance2"
    } else {
      "Punkty"
    }
  }))
  
  aes_reactive <- reactive({
    if(to_plot() == "Distance1"){
      aes(x = reorder(Name, Distance1), y = Distance1)
    } else if (to_plot() == "Distance2"){
      aes(x = reorder(Name, Distance2), y = Distance2)
    } else {
      aes(x = reorder(Name, Punkty), y = Punkty)
    }
  })
  
  output[["plot"]] <- renderPlot({
    ggplot(data = results_r(), aes_reactive()) +
      geom_bar(stat = "identity", fill = "dodgerblue4",  width = 0.5) + 
      geom_text(aes_string(label = to_plot()), position=position_dodge(width=1), hjust = -0.3) +
      coord_flip() +
      #scale_y_continuous(expand = expand_scale(add = c(0, 0)), limits = c(0, 320)) +
      labs(x="", y=label()) +
      theme_classic() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 11),
            legend.background = element_rect(colour = "grey"),
            axis.text.y = element_text(hjust = 0))# + 
  })
  

  
}

shinyApp(ui = ui, server = server)