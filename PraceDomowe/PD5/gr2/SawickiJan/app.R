library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

source("./process.R")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Football App"),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    )
  )),
  dashboardBody(tabItems(
    tabItem(
      "dashboard",
      plotOutput("win_rate_plot"),
      selectInput(
        inputId = "chosen_season",
        label = "Choose season",
        choices = levels(as.factor(team_results$season))
      ),
      selectInput(
        inputId = "chosen_team",
        label = "Choose your team",
        choices = levels(as.factor(team_results$team))
      ),
      plotOutput("wins_plot")
    )
  ))
)

server <- function(input, output) {
  data_r <- reactive({
    filter(team_results, season == input[["chosen_season"]])
  })
  
  output[["wins_plot"]] <- renderPlot({
    ggplot(data = data_r(), aes(x = data_r()$team, y = data_r()$wins)) +
      geom_bar(stat = "identity")
  })
}

shinyApp(ui, server)
