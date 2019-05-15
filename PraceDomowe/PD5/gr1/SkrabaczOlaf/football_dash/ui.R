library(shinydashboard)
library(DT)
library(tibble)
data <- read.csv("PL_dat.csv", sep=";") %>% as_tibble

dashboardPage(
  dashboardHeader(title="Premier league data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall", tabName = "overall"),
      menuItem("One Team", tabName = "one_team")
    ),
    radioButtons("season_chooser", label = h2("Season"),
                 choices = list("2015/2016" = "2015/2016", "2014/2015" = "2014/2015"), 
                 selected = "2015/2016"),
    sliderInput("stage_chooser", h2("Stages"), 1, 38, c(1,38), step = 1)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overall",
              h2("Overall league stats"),
              fluidRow(
                box(plotOutput("points_plot")),
                box(plotOutput("pstwo_plot"))
              ),
              fluidRow(
                box(plotOutput("max_goals_scored_plot")),
                box(plotOutput("mean_goals_scored"))
              )
      ),
      
      tabItem(tabName = "one_team",
              h2("Specific team stats"),
              selectInput("choosen_team", "Choose team", data$home_team %>% unique()),
              fluidRow(
                box(plotOutput("points_in_time")),
                box(plotOutput("goal_balance_in_time"))
              ),
              fluidRow(
                valueBoxOutput("Goals scored home", width=3),
                valueBoxOutput("Goals lost home", width=3),
                valueBoxOutput("Goals scored away", width=3),
                valueBoxOutput("Goals lost away", width=3)
              ),
              fluidRow(
                DT::DTOutput("spec_table")
              )
      )
    )
  )
)