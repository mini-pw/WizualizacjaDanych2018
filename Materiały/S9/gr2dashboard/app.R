library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

source("data-processing.R")

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
              plotOutput("win_rate_plot"),
              selectInput(inputId = "chosen_team", label = "Choose your team", choices = levels(win_perc_dat[["team"]])),
              checkboxInput(inputId = "decreasing_checkbox", label = "Deacreasing?", value = TRUE),
              plotOutput("points_against_plot")
      ),
      tabItem("about",
              "About the app",
              includeMarkdown("example.md")
      )
    )
  )
)

server <- function(input, output) {
  
  
  points_against_dat <- reactive({
    
    
    # | away_team == input[["chosen_team"]]
    rbind(filter(point_dat, home_team == input[["chosen_team"]]) %>% 
            select(opponent = away_team, points = home_team_points),
          filter(point_dat, away_team == input[["chosen_team"]]) %>% 
            select(opponent = home_team, points = away_team_points)) %>% 
      group_by(opponent) %>% 
      summarise(mean_points = mean(points))
  })
  
  output[["points_against_plot"]] <- renderPlot({
    arrange_fun <- if(input[["decreasing_checkbox"]]) {
      desc
    } else {
      identity
    }
    
    team_order <- points_against_dat() %>% 
      arrange(arrange_fun(mean_points)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(points_against_dat(), opponent = factor(opponent, levels = team_order)) %>% 
      ggplot(aes(x = opponent, y = mean_points)) +
      geom_col() +
      scale_y_continuous(limits = c(0, 3))
  })
  
  output[["win_rate_plot"]] <- renderPlot({
    team_order2015 <- filter(win_perc_dat, season == "2015/2016") %>% 
      arrange(desc(win_perc)) %>% 
      pull(team) %>% 
      as.character()
    
    
    mutate(win_perc_dat, team = factor(team, levels = team_order2015)) %>% 
      ggplot(aes(x = team, y = win_perc, fill = season)) +
      geom_col(position = "dodge") +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
  })
  
}

shinyApp(ui, server)
