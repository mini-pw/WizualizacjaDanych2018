library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App",
                  
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("exclamation-triangle"), status = "info",
                                                "This app is underdeveloped"
                               )
                  )),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("See also", icon = icon("send",lib='glyphicon'), 
               href = "https://github.com/mini-pw/WizualizacjaDanych2018/")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              box(title = "Number of teams", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, 
                  plotOutput("n_goals_plot", height = "300px")
              ),
              infoBox(title = "Number of bar charts", value = 2, subtitle = NULL,
                      icon = shiny::icon("bar-chart"), color = "aqua", width = 4),
              plotOutput("n_matches_plot")
      ),
      tabItem("about",
              "About the app"
      )
    )
  )
)

server <- function(input, output) {
  
  output[["n_matches_plot"]] <- renderPlot(
    select(dat, season, home_team, away_team) %>% 
      melt(id.vars = "season") %>% 
      group_by(value) %>% 
      summarise(n = length(value)) %>% 
      arrange(desc(n)) %>% 
      ggplot(aes(x = value, y = n)) +
      geom_col()
  )
  
  output[["n_goals_plot"]] <- renderPlot(
    select(dat, season, home_team_goal, away_team_goal) %>% 
      melt(id.vars = "season") %>% 
      group_by(season, variable) %>% 
      summarise(total = sum(value)) %>% 
      ggplot(aes(x = season, y = total, fill = variable)) +
      geom_col(position = "dodge")
  )
}

shinyApp(ui, server)
