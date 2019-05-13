library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

source('data-processing.R')

ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(
    title = 'Football App',
    dropdownMenu(
      type = 'notifications',
      badgeStatus = 'warning',
      notificationItem(icon = icon('exclamation-triangle'), status = 'info', 'This app is underdeveloped')
    )
  ),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[['effective_user']], subtitle = a(href = '#', icon('circle', class = 'text-success'), 'Online')),
    sidebarMenu(
      id = 'tabs',
      menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem('dashboard',
              fluidRow(
                box(
                  title = 'Inputs',
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = 'olive',
                  status = 'primary',
                  selectInput(inputId = 'chosen_season', label = 'Choose season', choices = levels(dat[['season']])),
                  selectInput(inputId = 'chosen_team', label = 'Choose team', choices = levels(win_sum_dat[['team']]))
                )
              ),
              fluidRow(
                infoBoxOutput('points-info-box'),
                infoBoxOutput('scored-goals-info-box'),
                infoBoxOutput('lost-goals-info-box')
              ),
              fluidRow(
                box(
                  title = 'Summary of whole season',
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = 'olive',
                  status = 'primary',
                  tableOutput('points_summary_table')
                ),
                box(
                  title = 'Average points against specific team',
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = 'olive',
                  status = 'primary',
                  plotOutput('points_against_plot')
                )
              ),
              fluidRow(
                box(
                  title = 'Average scored goals against specific team',
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = 'olive',
                  status = 'primary',
                  plotOutput('goals_scored_against_plot')
                ),
                box(
                  title = 'Average lost goals against specific team',
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  background = 'olive',
                  status = 'primary',
                  plotOutput('goals_lost_against_plot')
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  score_against_dat <- reactive({
    rbind(
        filter(point_dat, home_team == input[['chosen_team']]) %>% 
          select(opponent = away_team, points = home_team_points, scored_goals = home_team_goal, lost_goals = away_team_goal),
        filter(point_dat, away_team == input[['chosen_team']]) %>% 
          select(opponent = home_team, points = away_team_points, scored_goals = away_team_goal, lost_goals = home_team_goal)
      ) %>% 
      group_by(opponent) %>% 
      summarise(mean_points = mean(points), mean_scored_goals = mean(scored_goals), mean_lost_goals = mean(lost_goals))
  })
  
  # Info boxes
  
  output[['points-info-box']] <- renderInfoBox({
    total_points <- win_sum_dat %>%
      filter(season == input[['chosen_season']]) %>%
      filter(team == input[['chosen_team']]) %>%
      select(points)
    infoBox('Total points', total_points, icon = icon('futbol'), color = 'navy', fill =TRUE)
  })
  
  output[['scored-goals-info-box']] <- renderInfoBox({
    scored_goals <- win_sum_dat %>%
      filter(season == input[['chosen_season']]) %>%
      filter(team == input[['chosen_team']]) %>%
      select(scored_goals)
    infoBox('Scored goals', scored_goals, icon = icon('plus'), color = 'olive', fill =TRUE)
  })
  
  output[['lost-goals-info-box']] <- renderInfoBox({
    lost_goals <- win_sum_dat %>%
      filter(season == input[['chosen_season']]) %>%
      filter(team == input[['chosen_team']]) %>%
      select(lost_goals)
    infoBox('Lost goals', lost_goals, icon = icon('minus'), color = 'maroon', fill =TRUE)
  })
  
  # Table
  
  output[['points_summary_table']] <- renderTable({
    win_sum_dat %>%
      filter(season == input[['chosen_season']]) %>%
      select(-season) %>%
      filter(!is.na(points)) %>%
      arrange(desc(points))
  })
  
  # Bar charts
  
  output[['points_against_plot']] <- renderPlot({
    team_order <- score_against_dat() %>% 
      arrange(desc(mean_points)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(score_against_dat(), opponent = factor(opponent, levels = team_order)) %>% 
      ggplot(aes(x = opponent, y = mean_points)) +
      geom_col() +
      scale_y_continuous(limits = c(0, 3)) +
      coord_flip()
  })
  
  output[['goals_scored_against_plot']] <- renderPlot({
    team_order <- score_against_dat() %>% 
      arrange(desc(mean_scored_goals)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(score_against_dat(), opponent = factor(opponent, levels = team_order)) %>% 
      ggplot(aes(x = opponent, y = mean_scored_goals)) +
      geom_col() +
      scale_y_continuous(limits = c(0, 6)) +
      coord_flip()
  })
  
  output[['goals_lost_against_plot']] <- renderPlot({
    team_order <- score_against_dat() %>% 
      arrange(desc(mean_lost_goals)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(score_against_dat(), opponent = factor(opponent, levels = team_order)) %>% 
      ggplot(aes(x = opponent, y = mean_lost_goals)) +
      geom_col() +
      scale_y_continuous(limits = c(0, 6)) +
      coord_flip()
  })
}

shinyApp(ui, server)
