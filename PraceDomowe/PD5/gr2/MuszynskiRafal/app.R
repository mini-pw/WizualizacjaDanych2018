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
                                                "This app is sick!!"
                               )
                  )),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Compare", tabName = "compare", icon = icon("balance-scale")),
      menuItem("See also", icon = icon("send",lib='glyphicon'), 
               href = "https://github.com/mini-pw/WizualizacjaDanych2018/")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              selectInput(inputId = "chosen_season", label = "Choose season", choices = levels(dat[["season"]])),
              selectInput(inputId = "chosen_team", label = "Choose your team", choices = levels(points_in_season[['team']])),
              plotOutput("points_plot"),
              " ",
              fluidRow(
                column(6,
                  plotOutput("position_plot")),
                column(6,
                  plotOutput("points_per_stage_plot")
                )
              )
      ),
      tabItem("compare",
              selectInput(inputId = "compare_season", label = "Choose season", choices = levels(dat[["season"]])),
              selectInput(inputId = "compare_team_a", label = "Choose your team", choices = levels(points_in_season[['team']])),
              plotOutput('fractions_plot'),
              selectInput(inputId = "compare_team_b", label = "Choose opponent", choices = levels(points_in_season[['team']])),
              plotOutput('points_comparision')
      )
    )
  )
)

server <- function(input, output) {
  
  output[['points_plot']] <- renderPlot({
    points_in_chosen_season  <- points_in_season %>% 
      filter(season == input[['chosen_season']])
    
    team_order <- points_in_chosen_season %>% 
      arrange(desc(season_points)) %>% 
      pull(team) %>% 
      as.character()
    
    points_in_chosen_season %>%
      mutate(team = factor(team, levels = team_order)) %>%
      mutate(selection = (team == input[['chosen_team']])) %>% 
      ggplot(aes(x = team, y = season_points, fill = selection)) +
      geom_col() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
      ggtitle(paste("Season", input[['chosen_season']] ,"final results")) +
      ylab("Points in season")
      # scale_fill_manual(values = c('blue','red'), guide = FALSE)
  })
  
  last_stage_r <- reactive({
    last_stage <- max( position_in_table %>% 
                         filter(season == input[['chosen_season']]) %>% 
                         pull(stage) )
  })
  
  final_positions_r <- reactive({
    position_in_table %>%
      filter(season == input[['chosen_season']] & stage == last_stage_r())
  })
  
  output[['position_plot']] <- renderPlot({
    
    position_in_table %>%
      filter(season == input[['chosen_season']]) %>%
      mutate(selection = (team == input[['chosen_team']])) %>% 
      ggplot(aes(x = stage, y = position, color = team, alpha = selection, label = team)) +
      geom_line() +
      scale_alpha_discrete(range = c(0.3,1), guide = FALSE) +
      theme_minimal() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Position in table through season") +
      theme(legend.position = "none") +
      annotate("text", x= last_stage_r(), y = final_positions_r()[['position']], label = final_positions_r()[['team']]) +
      xlim(c(0,last_stage_r() + 5))
    
  })
  
  output[['points_per_stage_plot']] <- renderPlot({
    cumulative_points %>% 
      filter(season == input[['chosen_season']]) %>% 
      mutate(selection = (team == input[['chosen_team']])) %>% 
      ggplot(aes(x = stage, y = cumul_points, color = team, alpha = selection)) +
      geom_line() +
      scale_alpha_discrete(range = c(0.3,1), guide = FALSE) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Points through season") +
      ylab('points')
  })
  
  output[['points_comparision']] <- renderPlot({
    compare_dat <- point_at_stage %>%
      filter(season == input[['compare_season']]) %>%
      group_by(team, points) %>%
      summarise(occurences = n()) %>%
      mutate(result = ifelse(points == 0, 'lost', ifelse(points == 1, 'tied', 'won'))) %>%
      filter( team %in% c(input[['compare_team_a']], input[['compare_team_b']]))
    
    compare_dat %>% 
      ggplot(aes(x = result, y = occurences, fill = team)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('Comparision of results in season')
    
  })
  
  output[['fractions_plot']] <- renderPlot({
    valid_teams <- point_at_stage %>%  filter(season == input[['compare_season']]) %>% pull(team)
    validate(
      need(input[["compare_team_a"]] %in% valid_teams, "That team did not play in that season")
    )
    get_ratios_plot(input[['compare_team_a']], input[['compare_season']])
  })
  
}

shinyApp(ui, server)
