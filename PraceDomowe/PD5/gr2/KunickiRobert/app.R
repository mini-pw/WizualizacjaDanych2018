library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

source("data-processing.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("Aktualna tabela", icon = icon("send",lib='glyphicon'), 
               href = "https://www.livescore.com/soccer/england/premier-league/")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              plotOutput("win_rate_plot"),
              selectInput(inputId = "chosen_team", label = "Choose your team", choices = levels(win_perc_dat[["team"]])),
              checkboxInput(inputId = "decreasing_checkbox", label = "Deacreasing?", value = TRUE),
              plotOutput("points_against_plot"),
              plotOutput("goals_against_diff"),
              plotOutput("goals_against_scored"),
              plotOutput("goals_against_lost")
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
  
  goals_against_dat <- reactive({
      goals_dat %>% filter(team == input[["chosen_team"]])
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
      ggplot(aes(x = opponent, y = mean_points, fill = mean_points)) +
      geom_col() +
      scale_y_continuous(limits = c(0, 3))+
      geom_col(position = "dodge") +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5))+
      ggtitle("Średnia liczba punktów zdobyta przeciwko zespołowi") +
      xlab('Zespół') +
      ylab('Średnia liczba punktów ')  +
      labs(fill='Średnia liczba punktów')
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
  
  output[["goals_against_scored"]] <- renderPlot({
    
    arrange_fun <- if(input[["decreasing_checkbox"]]) {
      desc
    } else {
      identity
    }
    
    team_order <- points_against_dat() %>% 
      arrange(arrange_fun(mean_points)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(goals_against_dat(), team_against = factor(team_against, levels = team_order)) %>% 
      
      ggplot(aes(x = team_against, y = goals_scored_sum, fill=goals_scored_sum)) +
      geom_text(aes(label = goals_scored_sum), vjust = -0.2) +
      ggtitle("Bramki strzelone") +
      xlab('Zespół') +
      ylab('Bramki strzelone')  +
      labs(fill='Bramki strzelone') +
      geom_col(position = "dodge") +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
  })
  
  output[["goals_against_lost"]] <- renderPlot({
    
    arrange_fun <- if(input[["decreasing_checkbox"]]) {
      desc
    } else {
      identity
    }
    
    team_order <- points_against_dat() %>% 
      arrange(arrange_fun(mean_points)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(goals_against_dat(), team_against = factor(team_against, levels = team_order)) %>% 
      
      ggplot(aes(x = team_against, y = goals_lost_sum, fill=goals_lost_sum)) +
      geom_text(aes(label = goals_lost_sum), vjust = -0.2) +
      ggtitle("Bramki stracone") +
      xlab('Zespół') +
      ylab('Bramki stracone')  +
      labs(fill='Bramki stracone') +
      geom_col(position = "dodge") +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
    
  })
  
  output[["goals_against_diff"]] <- renderPlot({
    
    arrange_fun <- if(input[["decreasing_checkbox"]]) {
      desc
    } else {
      identity
    }
    
    team_order <- points_against_dat() %>% 
      arrange(arrange_fun(mean_points)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(goals_against_dat(), team_against = factor(team_against, levels = team_order)) %>% 
      ggplot(aes(x = team_against, y = goals_diff, fill=goals_diff)) +
      geom_text(aes(label = goals_diff), vjust = -0.2) +
      ggtitle("Różnica bramek zdobytych i straconych pomiędzy zespołami") +
      xlab('Zespół') +
      ylab('Różnica bramek')  +
      labs(fill='Różnica bramek') +
      geom_col(position = "dodge") +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
    
  })
  
}

shinyApp(ui, server)
