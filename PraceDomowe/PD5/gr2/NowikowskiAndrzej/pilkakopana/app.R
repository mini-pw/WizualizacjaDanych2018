#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggthemes)


# utils
get_points <- function(own_goals, enemy_goals) {
  ifelse(own_goals == enemy_goals,
         1,
         ifelse(own_goals > enemy_goals, 3, 0))
}

get_color <- function(value,
                      red_breakpoint,
                      green_breakpoint) {
  ifelse(value < red_breakpoint,
         'red',
         ifelse(value > green_breakpoint, 'green', 'blue'))
}

get_points_color <- function(points) {
  get_color(points, 1, 2)
}

get_win_ratio_color <- function(win_ratio) {
  get_color(win_ratio, 0.4, 0.6)
}

get_goal_ratio_color <- function(goal_ratio) {
  get_color(goal_ratio, 0.9, 1.1)
}


source_url <-
  "https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv"
dat <- read.csv2(source_url)
# dat <- dat %>% select(-date)
overview_dat <- rbind(
  dat %>% mutate(
    team = home_team,
    enemy = away_team,
    own_goals = home_team_goal,
    enemy_goals = away_team_goal
  ),
  dat %>% mutate(
    team = away_team,
    enemy = home_team,
    own_goals = away_team_goal,
    enemy_goals = home_team_goal
  )
) %>% select(-c(home_team, away_team, home_team_goal, away_team_goal))

overview_dat <-
  overview_dat %>% mutate(score = get_points(own_goals, enemy_goals))
# str(dat)
teams <-
  unique(rbind(
    dat %>% mutate(team = away_team) %>% select(team),
    dat %>% mutate(team = home_team) %>% select(team)
  ))

seasons <- levels(unlist(unique(dat %>% select(season))))


ui <- dashboardPage(
  skin = "green",
  header = dashboardHeader(title = "Piłka kopana w Anglii"),
  sidebar = dashboardSidebar(sidebarMenu(
    menuItem(
      "Overview",
      tabName = "overview",
      icon = icon("dashboard"),
      menuSubItem(
        tabName = "overview",
        icon = NULL,
        selectInput(
          inputId = 'overview_season',
          label = "Select seasons",
          choices = unique(dat$season),
          # multiple = TRUE,
          selected = unique(dat$season)
        )
      ),
      menuSubItem(
        tabName = "overview",
        icon = NULL,
        sliderInput(
          inputId = "overview_stage_range",
          label = "Select stage range:",
          min = min(dat %>% select(stage)),
          max = max(dat %>% select(stage)),
          value = c(min(dat %>% select(stage)), max(dat %>% select(stage))),
          step = 1
        )
      )
    ),
    menuItem(
      "Team details",
      tabName = "teamdetails",
      icon = icon('info-circle'),
      menuSubItem(
        tabName = "teamdetails",
        icon = NULL,
        selectInput(
          inputId = "details_season",
          label = "Select season",
          choices = seasons
        )
      ),
      menuSubItem(
        tabName = "teamdetails",
        icon = NULL,
        selectInput(
          inputId = 'details_team',
          label = 'Select team:',
          choices = teams
        )
      ),
      menuSubItem(
        tabName = "teamdetails",
        icon = NULL,
        uiOutput('details_opponent_input')
      )
    ),
    menuItem(
      "See also",
      icon = icon("send", lib = 'glyphicon'),
      href = "https://github.com/mini-pw/WizualizacjaDanych2018/"
    )
  )),
  body = dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "overview",
            fluidRow(
              box(
                width = 12,
                title = "Total score",
                collapsible = T,
                status = 'primary',
                plotOutput('overview_total_score')
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Total goals",
                collapsible = T,
                status = 'primary',
                plotOutput('overview_total_goals')
              )
            )),
    
    # Second tab content
    tabItem(
      tabName = "teamdetails",
      h1("Team details"),
      fluidRow(
        column(
          width = 6,
          h3("Home matches"),
          valueBoxOutput('details_home_avg_points'),
          valueBoxOutput('details_home_avg_win_ratio'),
          valueBoxOutput('details_home_avg_goals_ratio')
        ),
        column(
          width = 6,
          h3("Away matches"),
          valueBoxOutput('details_away_avg_points'),
          valueBoxOutput('details_away_avg_win_ratio'),
          valueBoxOutput('details_away_avg_goals_ratio')
        )
      ),
      conditionalPanel(
        "input.details_team_enemy != 'None'",
        fluidRow(
          column(
            width = 6,
            uiOutput('home_vs'),
            valueBoxOutput('details_home_avg_points_vs'),
            valueBoxOutput('details_home_avg_win_ratio_vs'),
            valueBoxOutput('details_home_avg_goals_ratio_vs')
          ),
          column(
            width = 6,
            uiOutput('away_vs'),
            valueBoxOutput('details_away_avg_points_vs'),
            valueBoxOutput('details_away_avg_win_ratio_vs'),
            valueBoxOutput('details_away_avg_goals_ratio_vs')
          )
        ),
        fluidRow(
          column(
            width = 6,
            valueBoxOutput('details_home_wins_vs'),
            valueBoxOutput('details_home_draws_vs'),
            valueBoxOutput('details_home_loses_vs')
          ),
          column(
            width = 6,
            valueBoxOutput('details_away_wins_vs'),
            valueBoxOutput('details_away_draws_vs'),
            valueBoxOutput('details_away_loses_vs')
          )
        )
      ),
      h3("Team points"),
      fluidRow(column(12, plotOutput(
        'details_team_points'
      ))),
      h3("Last matches"),
      fluidRow(column(
        12, dataTableOutput('details_last_matches')
      ))
    )
  ))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # team details
  team_details_r <-
    reactive({
      df <-
        dat %>% filter(season == input$details_season) %>%  filter(home_team == input$details_team |
                                                                     away_team == input$details_team) %>% arrange(desc(date))
      # browser()
      validate(need(nrow(df) != 0, "No data"))
      return(df)
    })
  
  output$details_last_matches <-
    renderDataTable(team_details_r(),
                    options = list(pageLength = 5))
  
  
  
  
  avg_home_points_r <- reactive({
    round(
      team_details_r() %>% filter(home_team == input$details_team) %>% mutate(points = get_points(home_team_goal, away_team_goal)) %>% summarize(mean(points)),
      2
    )
  })
  
  avg_home_win_ratio_r <- reactive({
    won_matches <-
      nrow(
        team_details_r() %>% filter(home_team == input$details_team) %>% mutate(points = get_points(home_team_goal, away_team_goal)) %>% filter(points == 3)
      )
    all_matches <-
      nrow(team_details_r() %>% filter(home_team == input$details_team))
    round(won_matches / all_matches,
          2)
  })
  
  avg_home_goal_ratio_r <- reactive({
    goal_scored <-
      team_details_r() %>% filter(home_team == input$details_team) %>%  summarise(sum(home_team_goal))
    goal_against <-
      team_details_r() %>% filter(home_team == input$details_team) %>%  summarise(sum(away_team_goal))
    round(goal_scored / goal_against,
          2)
  })
  
  avg_away_points_r <- reactive({
    round(
      team_details_r() %>% filter(away_team == input$details_team) %>% mutate(points = get_points(away_team_goal, home_team_goal)) %>% summarize(mean(points)),
      2
    )
  })
  
  avg_away_win_ratio_r <- reactive({
    won_matches <-
      nrow(
        team_details_r() %>% filter(away_team == input$details_team) %>% mutate(points = get_points(away_team_goal, home_team_goal)) %>% filter(points == 3)
      )
    all_matches <-
      nrow(team_details_r() %>% filter(away_team == input$details_team))
    round(won_matches / all_matches,
          2)
  })
  
  avg_away_goal_ratio_r <- reactive({
    goal_scored <-
      team_details_r() %>% filter(away_team == input$details_team) %>%  summarise(sum(away_team_goal))
    goal_against <-
      team_details_r() %>% filter(away_team == input$details_team) %>%  summarise(sum(home_team_goal))
    round(goal_scored / goal_against,
          2)
  })
  
  
  
  output$details_home_avg_points <-
    renderValueBox(valueBox(
      avg_home_points_r(),
      'Average points per match',
      color = get_points_color(avg_home_points_r())
    ))
  
  output$details_away_avg_points <-
    renderValueBox(valueBox(
      avg_away_points_r(),
      'Average points per match',
      color = get_points_color(avg_away_points_r())
    ))
  
  output$details_home_avg_win_ratio <-
    renderValueBox(valueBox(
      avg_home_win_ratio_r(),
      'Average win ratio',
      color = get_win_ratio_color(avg_home_win_ratio_r())
    ))
  
  output$details_away_avg_win_ratio <-
    renderValueBox(valueBox(
      avg_away_win_ratio_r(),
      'Average win ratio',
      color = get_win_ratio_color(avg_away_win_ratio_r())
    ))
  
  output$details_home_avg_goals_ratio <-
    renderValueBox(
      valueBox(
        avg_home_goal_ratio_r(),
        'Average goal scored/against ratio',
        color = get_goal_ratio_color(avg_home_goal_ratio_r())
      )
    )
  
  output$details_away_avg_goals_ratio <-
    renderValueBox(
      valueBox(
        avg_away_goal_ratio_r(),
        'Average goal scored/against ratio',
        color = get_goal_ratio_color(avg_away_goal_ratio_r())
      )
    )
  
  
  
  avg_home_points_vs_r <- reactive({
    df <-
      team_details_r() %>% filter(home_team == input$details_team &
                                    away_team == input$details_team_enemy) %>% mutate(points = get_points(home_team_goal, away_team_goal))
    validate(need(nrow(df) != 0, "No matches"))
    round(df %>% summarize(mean(points)),
          2)
  })
  
  avg_home_win_ratio_vs_r <- reactive({
    df <-
      team_details_r() %>% filter(home_team == input$details_team &
                                    away_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    won_matches <-
      nrow(df %>% mutate(points = get_points(home_team_goal, away_team_goal)) %>% filter(points == 3))
    round(won_matches / nrow(df),
          2)
  })
  
  avg_home_goal_ratio_vs_r <- reactive({
    df <-
      team_details_r() %>% filter(home_team == input$details_team &
                                    away_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    goal_scored <-  df %>%  summarise(sum(home_team_goal))
    goal_against <- df %>%  summarise(sum(away_team_goal))
    if (goal_against == 0) {
      return(1)
    }
    round(goal_scored / goal_against,
          2)
  })
  
  avg_away_points_vs_r <- reactive({
    df <-
      team_details_r() %>% filter(away_team == input$details_team &
                                    home_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    round(df %>% mutate(points = get_points(away_team_goal, home_team_goal)) %>% summarize(mean(points)),
          2)
  })
  
  avg_away_win_ratio_vs_r <- reactive({
    df <- team_details_r() %>% filter(away_team == input$details_team &
                                        home_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    won_matches <-
      nrow(df %>% mutate(points = get_points(away_team_goal, home_team_goal)) %>% filter(points == 3))
    
    round(won_matches / nrow(df),
          2)
  })
  
  avg_away_goal_ratio_vs_r <- reactive({
    df <-  team_details_r() %>% filter(away_team == input$details_team &
                                         home_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    goal_scored <-
      df %>%  summarise(sum(away_team_goal))
    goal_against <-
      df %>%  summarise(sum(home_team_goal))
    if (goal_against == 0) {
      return(1)
    }
    round(goal_scored / goal_against,
          2)
  })
  
  
  
  output$details_home_avg_points_vs <-
    renderValueBox(
      valueBox(
        avg_home_points_vs_r(),
        'Average points per match',
        color = get_points_color(avg_home_points_vs_r())
      )
    )
  
  output$details_away_avg_points_vs <-
    renderValueBox(
      valueBox(
        avg_away_points_vs_r(),
        'Average points per match',
        color = get_points_color(avg_away_points_vs_r())
      )
    )
  
  output$details_home_avg_win_ratio_vs <-
    renderValueBox(
      valueBox(
        avg_home_win_ratio_vs_r(),
        'Average win ratio',
        color = get_win_ratio_color(avg_home_win_ratio_vs_r())
      )
    )
  
  output$details_away_avg_win_ratio_vs <-
    renderValueBox(
      valueBox(
        avg_away_win_ratio_vs_r(),
        'Average win ratio',
        color = get_win_ratio_color(avg_away_win_ratio_vs_r())
      )
    )
  
  output$details_home_avg_goals_ratio_vs <-
    renderValueBox(
      valueBox(
        avg_home_goal_ratio_vs_r(),
        'Average goal scored/against ratio',
        color = get_goal_ratio_color(avg_home_goal_ratio_vs_r())
      )
    )
  
  output$details_away_avg_goals_ratio_vs <-
    renderValueBox(
      valueBox(
        avg_away_goal_ratio_vs_r(),
        'Average goal scored/against ratio',
        color = get_goal_ratio_color(avg_away_goal_ratio_vs_r())
      )
    )
  
  avg_home_wins_vs_r <- reactive({
    df <-  team_details_r() %>% filter(home_team == input$details_team &
                                         away_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    nrow(df %>% mutate(points = get_points(home_team_goal, away_team_goal)) %>% filter(points == 3))
  })
  
  avg_home_draws_vs_r <- reactive({
    df <-  team_details_r() %>% filter(home_team == input$details_team &
                                         away_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    nrow(df %>% mutate(points = get_points(home_team_goal, away_team_goal)) %>% filter(points == 1))
  })
  
  avg_home_loses_vs_r <- reactive({
    df <-  team_details_r() %>% filter(home_team == input$details_team &
                                         away_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    nrow(df %>% mutate(points = get_points(home_team_goal, away_team_goal)) %>% filter(points == 0))
  })
  
  output$details_home_wins_vs <-
    renderValueBox(valueBox(avg_home_wins_vs_r(),
                            'Wins count'))
  
  output$details_home_draws_vs <-
    renderValueBox(valueBox(avg_home_draws_vs_r(),
                            'Draws count'))
  
  output$details_home_loses_vs <-
    renderValueBox(valueBox(avg_home_loses_vs_r(),
                            'Loses count'))
  
  
  avg_away_wins_vs_r <- reactive({
    df <-  team_details_r() %>% filter(away_team == input$details_team &
                                         home_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    nrow(df %>% mutate(points = get_points(away_team_goal, home_team_goal)) %>% filter(points == 3))
  })
  
  avg_away_draws_vs_r <- reactive({
    df <-  team_details_r() %>% filter(away_team == input$details_team &
                                         home_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    nrow(df %>% mutate(points = get_points(away_team_goal, home_team_goal)) %>% filter(points == 1))
  })
  
  avg_away_loses_vs_r <- reactive({
    df <-  team_details_r() %>% filter(away_team == input$details_team &
                                         home_team == input$details_team_enemy)
    validate(need(nrow(df) != 0, "No matches"))
    nrow(df %>% mutate(points = get_points(away_team_goal, home_team_goal)) %>% filter(points == 0))
  })
  
  
  output$details_away_wins_vs <-
    renderValueBox(valueBox(avg_away_wins_vs_r(),
                            'Wins count'))
  
  output$details_away_draws_vs <-
    renderValueBox(valueBox(avg_away_draws_vs_r(),
                            'Draws count'))
  
  output$details_away_loses_vs <-
    renderValueBox(valueBox(avg_away_loses_vs_r(),
                            'Loses count'))
  
  team_points_r <- reactive({
    team_details_r() %>% mutate(points = ifelse(
      home_team == input$details_team,
      get_points(home_team_goal, away_team_goal),
      get_points(away_team_goal, home_team_goal)
    )) %>%
      arrange(stage) %>%
      mutate(total_points = cumsum(points))
  })
  
  
  
  
  output$details_team_points <- renderPlot({
    ggplot(data = team_points_r(), aes(x = stage, y = total_points)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      ylab("Total points") +
      xlab("Stage")
  })
  
  overview_dat_r <- reactive({
    # browser()
    df <- overview_dat %>%
      filter(season == input$overview_season) %>%
      filter(stage >= input$overview_stage_range[1]) %>%
      filter(stage <= input$overview_stage_range[2])
    # browser()
    validate(need(nrow(df) != 0, "No data"))
    return(df)
    
  })
  
  output$overview_total_score <- renderPlot({
    df <-
      overview_dat_r() %>% group_by(team) %>% summarize(total_score = sum(score))
    # df <- overview_dat %>% group_by(team) %>% summarize(total_score=sum(score))
    zig <- df %>% arrange(total_score) %>% pull(team)
    ggplot(data = as.data.frame(df), aes(x = team, y = total_score)) +
      geom_col() +
      geom_text(aes(label = total_score), hjust = -1.0) +
      coord_flip() +
      xlab('') +
      ylab(
        paste0(
          "Total score in season ",
          input$overview_season,
          ' within stages ',
          input$overview_stage_range[[1]],
          ' and ',
          input$overview_stage_range[[2]]
        )
      ) +
      scale_x_discrete(limits = zig) +
      theme_gdocs()
  })
  
  output$overview_total_goals <- renderPlot({
    df <-
      overview_dat_r() %>% group_by(team) %>% summarize(total_goals = sum(own_goals))
    # df <- overview_dat %>% group_by(team) %>% summarize(total_score=sum(score))
    zig <- df %>% arrange(total_goals) %>% pull(team)
    ggplot(data = as.data.frame(df), aes(x = team, y = total_goals)) +
      geom_col() +
      geom_text(aes(label = total_goals), hjust = -1.0) +
      coord_flip() +
      xlab('') +
      ylab(
        paste0(
          "Total goals in season ",
          input$overview_season,
          ' within stages ',
          input$overview_stage_range[[1]],
          ' and ',
          input$overview_stage_range[[2]]
        )
      ) +
      scale_x_discrete(limits = zig) +
      theme_gdocs()
  })
  
  output$details_opponent_input <- renderUI({
    selectInput(
      inputId = 'details_team_enemy',
      label = 'Select opponent:',
      choices = c('None', teams %>% filter(team != input$details_team))
    )
  })
  
  output$home_vs <- renderUI({
    h3(paste0('Home matches vs ', input$details_team_enemy))
  })
  output$away_vs <- renderUI({
    h3(paste0('Away matches vs ', input$details_team_enemy))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
