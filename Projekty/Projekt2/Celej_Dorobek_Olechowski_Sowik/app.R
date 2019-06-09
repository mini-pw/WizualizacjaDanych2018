library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(DT)
library(waffle)
source('bar_plots_gone_wrong.R')

share_types <- c('A', 'B', 'C', 'D', 'E')
usa_un <- c(10, 15, 20, 25, 30)
usa <- usa_un/sum(usa_un)
uk_un <- c(8, 12, 12, 28, 40)
uk <- uk_un/sum(uk_un)

market_share <- data.frame(share_types, usa, uk)
market_share_narrow <-gather(market_share, "country", "share", usa, uk)
market <-market_share_narrow %>% mutate(pct=share*100)


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
              fluidRow(box(title = "Stacked barplot", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot5"), width = 6),
                       box(title = "plot6", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot6"), width = 6, collapsed = TRUE)),
              fluidRow(box(title = "Scale not from zero", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot7"), width = 6),
                       box(title = "Scale begins in zero", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot8"), width = 6, collapsed = TRUE)),
              fluidRow(box(title = "Gap in y scale", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot9"), width = 6),
                       box(title = "Zoomed facet", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot10"), width = 6, collapsed = TRUE)),
              fluidRow(box(title = "Spider plot", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot11"), width = 6),
                       box(title = "Barplot", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("plot12"), width = 6, collapsed = TRUE))
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
  output$plot5 <- renderPlot(market_stacked)
  output$plot6 <- renderPlot(market_dodge)
  output$plot7 <- renderPlot(scale_not_in_zero)
  output$plot8 <- renderPlot(scale_in_zero)
  output$plot9 <- renderPlot(gap_in_scale())
  output$plot10 <- renderPlot(zoomed_plot)
  output$plot11 <- renderPlot(plot_radar_chart())
  output$plot12 <- renderPlot(barplot_school_subjects)
}

shinyApp(ui, server)