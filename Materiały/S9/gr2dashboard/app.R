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
              plotOutput("win_rate_plot")
      ),
      tabItem("about",
              "About the app",
              includeMarkdown("example.md")
      )
    )
  )
)

server <- function(input, output) {
  
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
