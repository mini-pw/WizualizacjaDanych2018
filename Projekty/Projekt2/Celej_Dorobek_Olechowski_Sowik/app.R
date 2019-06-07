library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(DT)
library(waffle)


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
               href = "https://github.com/mini-pw/WizualizacjaDanych2018/blob/master/Prezentacje/P4.Rmd/"),
      radioButtons(
        "plot_type",
        "Jakie wykresy chcesz podziwiać",
        choices = c("złe", "dobre", "oba")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(box(title = "plot1", status = "primary",
                          solidHeader = TRUE, collapsible = TRUE, 
                          plotOutput("plot1"), width = 6),
                      box(title = "plot2", status = "primary",
                          solidHeader = TRUE, collapsible = TRUE, 
                          plotOutput("plot2"), width = 6)),
              fluidRow(box(title = "plot3", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot3"), width = 6),
                       box(title = "plot4", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot4"), width = 6)),
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
              "App developed for WD project"
      )
    )
  )
)
server <- function(input, output) {
  output$plot1 <- renderPlot(ggplot())
  output$plot2 <- renderPlot(ggplot())
  output$plot3 <- renderPlot(ggplot())
  output$plot4 <- renderPlot(ggplot())
  output$plot5 <- renderPlot(ggplot())
  output$plot6 <- renderPlot(ggplot())
  output$plot7 <- renderPlot(ggplot())
  output$plot8 <- renderPlot(ggplot())
}

shinyApp(ui, server)