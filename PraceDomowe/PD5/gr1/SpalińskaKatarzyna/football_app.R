library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(markdown)
source("analiza_football.R")


football <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App"
                  
                  #dropdownMenu(type = "notifications", badgeStatus = "warning",
                  #             notificationItem(icon = icon("exclamation-triangle"), status = "info",
                  #                              "This app is underdeveloped"
                  #             )
                  ),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("About", icon = icon("info-circle"), tabName = "about",
               badgeColor = "green"),
      menuItem("Compare All Teams", tabName = "dashboard_all", icon = icon("dashboard")),
      menuItem("Compare Team", tabName = "dashboard_team", icon = icon("dashboard"))
      #menuItem("See also", icon = icon("send",lib='glyphicon'), 
      #         href = "https://github.com/mini-pw/WizualizacjaDanych2018/")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard_all",
      fluidRow(
        column(width = 6,
          box(title = "Goals scored home and away", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, width = NULL,
              selectInput("season_all", "Choose season:",
                          unique(football$season)
              ),
              plotOutput("all_goals_home_away")
          ),
          infoBox(title = "Most goals scored home", value = textOutput("info_1"), subtitle = NULL,
                  icon = shiny::icon("bar-chart"), color = "aqua", width = NULL),
          infoBox(title = "Most goals scored away", value = textOutput("info_2"), subtitle = NULL,
                  icon = shiny::icon("bar-chart"), color = "aqua", width = NULL),
          infoBox(title = "Most goals scored in total", value = textOutput("info_3"), subtitle = NULL,
                  icon = shiny::icon("bar-chart"), color = "aqua", width = NULL)
        ),
        column(width = 6,
          box(title = "Teams with most victories per season", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, width = NULL,
              plotOutput("all_most_won_matches_1"),
              plotOutput("all_most_won_matches_2")
          )
          
        )
      )
      ),
      tabItem("dashboard_team",
              #infoBox(title = "Number of bar charts", value = 2, subtitle = NULL,
              #        icon = shiny::icon("bar-chart"), color = "aqua", width = 4),
              fluidRow( 
                column(width = 6,
                  box(title = "Choose options for analysis", status = "primary",
                      solidHeader = TRUE, width = NULL,
                      selectInput("team", "Choose Main Team:",
                                  unique(football$home_team)
                      ),
                      selectInput("season_team", "Choose season:",
                                  unique(football$season)
                      ),
                      selectInput("team_2", "Choose 2nd team:",
                                  unique(football$home_team)
                      )
                  ),
                  infoBox(title = "Info",
                          value =  "Charts refer to chosen Main Team", 
                          subtitle = "Additionally, chart below can compare progress between two teams. 
                                      To avoid comparison, choose the same team as both 'Main' and 'Second'.
                                      Progress is calculated based on winnings and losses in subsequent stages.", 
                          icon = shiny::icon("bar-chart"), color = "aqua", width = NULL),
                  box(title = "Progress among stages during the season", status = "primary",
                      solidHeader = TRUE, collapsible = TRUE,width = NULL,
                      plotOutput("team_progress"))
                ),
                column(width = 6,
                  box(title = "Matches lost and won with other teams in the past two seasons", status = "primary",
                      solidHeader = TRUE, collapsible = TRUE,width = NULL,
                      plotOutput("team_won_lost")),
                  box(title = "Number of goals per month during the season", status = "primary",
                      solidHeader = TRUE, collapsible = TRUE,width = NULL,
                      plotOutput("team_goals_in_season")
                  
                ))
              )
      ),
      tabItem("about",
              "About the app",
              includeMarkdown("./example.md")
      )
    )
  )
)

server <- function(input, output) {
    
  output[["team_goals_in_season"]] <- renderPlot(
    plot_1(input[["season_team"]],input[["team"]])
  )
  
  output[["all_goals_home_away"]] <- renderPlot(
    plot_2(input[["season_all"]])
  )
  
  output[["all_most_won_matches_1"]] <- renderPlot(
    plot_3("2014/2015")
  )
  
  output[["all_most_won_matches_2"]] <- renderPlot(
    plot_3("2015/2016")
  )
  
  output[["team_won_lost"]] <- renderPlot(
    plot_4(input[["team"]])
  )
  
  output[["team_progress"]] <- renderPlot(
    plot_5(input[["season_team"]],input[["team"]],input[["team_2"]])
  )
  
  output[["info_1"]] <- renderText(
    func_1("home",input[["season_all"]])
  )
  
  output[["info_2"]] <- renderText(
    func_1("away",input[["season_all"]])
  )
  
  output[["info_3"]] <- renderText(
    func_1("total",input[["season_all"]])
  )
  
}

shinyApp(ui, server)