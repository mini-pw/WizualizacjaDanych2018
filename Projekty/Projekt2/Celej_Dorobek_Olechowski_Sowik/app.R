library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(waffle)
library(plotrix)
library(ggthreed)


live_matches = data.frame(Result = c("Win", "Draw", "Loss"), Value = c(30, 7, 1))
live_matches$Result = factor(live_matches$Result, levels = live_matches$Result)

live_scorers = data.frame(Player = c("Mohamed Salah",
                                     "Sadio Mane",
                                     "Roberto Firmino",
                                     "Xerdan Shaqiri",
                                     "James Milner",
                                     "Virgil van Dijk",
                                     "Georginio Wijnaldum",
                                     "Divock Origi",
                                     "Daniel Sturridge",
                                     "Naby Keita",
                                     "Fabinho",
                                     "Jordan Henderson",
                                     "Trent Alexander-Arnold",
                                     "Joel Matip",
                                     "Dejan Lovren"),
                          Value = c(22, 22, 12, 6, 5, 4, 3, 3, 2, 2, 1, 1, 1, 1, 1))
live_scorers$Player = factor(live_scorers$Player, levels = live_scorers$Player)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Nie rób tego w domu"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("See also", icon = icon("send",lib='glyphicon'), 
               href = "https://github.com/mini-pw/WizualizacjaDanych2018/blob/master/Prezentacje/P4.Rmd/")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              titlePanel("Waffle vs varplot: Liverpool matches in Premier League 2018/19"),
              fluidRow(column(2,                           
                              textInput("textInput1", "How many macthes Liverpool won?", value="0"),
                              verbatimTextOutput("textOutput1", placeholder = FALSE)),
                       box(title = "Waffle", status = "primary",
                          solidHeader = TRUE, collapsible = TRUE, 
                          plotOutput("plot1bad"),
                          width = 5),
                       box(title = "Barplot", status = "primary",
                          solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                          plotOutput("plot1good"), width = 5)),
              titlePanel("Exploded Pie vs barplot: Best Liverpool goalscorers in 2018/19"),
              fluidRow(column(2,                           
                              selectInput("selectInput2", label = "Who scored more: Salah vs Mane?", 
                                          choices = c("don't know", "Mane", "Salah", "the same"), 
                                          selected = NULL),
                              verbatimTextOutput("textOutput2", placeholder = FALSE)),
                       box(title = "Exploded Pie", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot2bad"), width = 5),
                       box(title = "Barplot", status = "primary", collapsed = TRUE,
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot2good"), width = 5)),
              fluidRow(box(title = "plot5", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot5"), width = 6),
                       box(title = "plot6", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot6"), width = 6)),
              fluidRow(box(title = "plot7", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot7"), width = 6),
                       box(title = "plot8", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot8"), width = 6))
      ),
      tabItem("about",
              "App developed for WD project. Bad plots on the left, collapsed (good) on the right."
      )
    )
  )
)
server <- function(input, output) {
  output$plot1good <- renderPlot({
    ggplot(data = live_matches, aes(x = Result, y = Value)) +
      geom_bar(stat = "identity") +
      theme_hc()
    })
  
  output$plot1bad <- renderPlot({
    waffle(live_matches$Value, rows = 5) +
      scale_fill_manual(name = "Result: ", labels = c("Win", "Draw", "Loss", ""),
                        values = c("#414046", "#7CB5EC", "#90EC7D", "#FFFFFF")) +
      theme(legend.text = element_text(size = 15), legend.title = element_text(size=17),
            legend.position = "right")
  })
  output$textOutput1 <- renderText({
    input$textInput1})
  
  output$plot2good <- renderPlot({
    ggplot(data = live_scorers, aes(x = Player, y = Value)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(expand = expand_scale(add = c(0, 8))) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.4)) +
      theme_hc()
  })
  output$plot2bad <- renderPlot({
    pie3D(live_scorers$Value, labels = live_scorers$Player, explode=0.4, labelcex = 1)
  })
  output$textOutput2 <- renderText({
    input$selectInput2})
  
  output$plot4 <- renderPlot(ggplot())
  output$plot5 <- renderPlot(ggplot())
  output$plot6 <- renderPlot(ggplot())
  output$plot7 <- renderPlot(ggplot())
  output$plot8 <- renderPlot(ggplot())
}

shinyApp(ui, server)