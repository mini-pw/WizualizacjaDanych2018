sidebar_menu_component <- function() {
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("futbol")),
    menuItem("Analyzer", tabName = "analyzer", icon = icon("chart-bar"))
  )
}

overview_tab_item <- function() {
  tabItem(
    tabName = "overview",
    
    fluidRow(
      column(
        width = 7,
        box(
          title = "Winning percentage",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          plotOutput("win_percentage_plot")
        )
      ),
      column(
        width = 5,
        box(
          title = "Goal differences",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          plotOutput("head2head_plot")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(title = "Current streaks",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            plotOutput("streaks_plots")
        )
      )
    )
  )
}


analyzer_tab_item <- function(football_data_team_view) {
  input_box <- function() {
    box(
      width = NULL,
      title = "Inputs",
      status = "warning",
      solidHeader = TRUE,
      selectInput(
        inputId = "selected_team",
        label = "Select your team",
        choices = unique(football_data_team_view$team)
      ),
      checkboxInput(
        inputId = "opponent_selection_allowed",
        label = "Include statistics against specific opponent"
      ),
      selectInput(
        inputId = "selected_opponent",
        label = "Select your opponent",
        choices = unique(football_data_team_view$team)
      )
    )
  }
  
  tabItem(
    tabName = "analyzer",
    fluidRow(
      column(width = 2,
             valueBoxOutput(outputId = "streak_box", width = NULL),
             valueBoxOutput(outputId = "home_win_percentage", width = NULL),
             valueBoxOutput(outputId = "road_win_percentage", width = NULL)
      ),
      box(
        width = 6,
        title = textOutput("goals_over_time_label"),
        status = "primary",
        solidHeader = TRUE,
        plotOutput("goals_over_time_plot")
      ),
       column(
         width = 4,
         input_box()
       )
    ),
    fluidRow(
      div(
        id = "specific_stats_box",
        box(
        width = 12,
        title = "Opponent specific stats",
        fluidRow(
          column(
            width = 1,
            valueBoxOutput("win_amount_box", width = NULL),
            valueBoxOutput("draw_amount_box", width = NULL),
            valueBoxOutput("loss_amount_box", width = NULL)
          ),
          column(
            width = 11,
            plotOutput("goal_difference_plot")
          )
        )
      )
    )
    )
  )
}

create_value_box_from_team_streak <- function(selected_team_streak) {
  box_color <- ifelse(selected_team_streak$streak_value == "win",
                      "green",
                      ifelse(selected_team_streak$streak_value == "lost",
                             "red",
                             "light-blue"))
  
  box_title <- ifelse(selected_team_streak$streak_value == "win",
                      "Winning",
                      ifelse(selected_team_streak$streak_value == "lost",
                             "Losing",
                             "Drawing"))
  
  valueBox(
    subtitle = sprintf("%s streak", box_title),
    value = selected_team_streak$streak_length,
    color = box_color,
    icon = icon("chart-bar")
  ) 
}