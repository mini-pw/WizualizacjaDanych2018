library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# helpers

get_team_goals <- function(data, sum_season) {
  season_data <- data %>% 
    filter(season == sum_season)
  
  as.data.frame(rbind(season_data %>% select(stage, date, scored_goals = home_team_goal, lost_goals = away_team_goal, team = home_team),
                      season_data %>% select(stage, date, scored_goals = away_team_goal, lost_goals = home_team_goal, team = away_team)))
}

# ile goli strzelila kazda z druzyn dla kazdego sezonu
get_summary_goals <- function(data, sum_season) {
  df <- get_team_goals(data, sum_season) %>%
    select(scored_goals, lost_goals, team) %>% 
    group_by(team) %>% 
    summarize(scored_goals=sum(scored_goals),
              lost_goals=sum(lost_goals))
  rbind(
    df %>% select(team, goals = scored_goals) %>% mutate(status="scored"),
    df %>% select(team, goals = lost_goals) %>% mutate(status="lost")
  )
}

get_summary_goals_plot <- function(data, sum_season) {
  get_summary_goals(data, sum_season) %>% 
    ggplot(aes(x = team, y = goals, group = status, fill = status)) +
    geom_col(position = position_dodge(-0.9)) +
    scale_fill_manual(values=c("#ff4f61", "#00c800")) +
    theme_minimal() +
    geom_text(aes(label = goals), color = "White", size = 5, position=position_dodge(-0.9), vjust=2) +
    theme(axis.text.x = element_text(angle=60, hjust=1, size=15))
}

# 2: liczba strzelonych goli dla kazdej rundy
get_goals_for_team_per_stage <- function(data, wseason, wteam, status) {
  r <- get_team_goals(data, wseason) %>% 
    filter(team == wteam)
  if (status == "scored") {
    r <- r %>% select(stage, goals=scored_goals)
  } else {
    r <- r %>% select(stage, goals=lost_goals)
  }
  
  as.data.frame(r) %>%
    arrange(stage)
}

get_goals_for_team_per_stage_plot <- function(data, wseason, wteam, status) {
  df <- get_goals_for_team_per_stage(data, wseason, wteam, status)
  df %>% 
    ggplot(aes(x=stage, y = goals)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    scale_x_continuous(breaks=seq(1,max(df$stage),1)) +
    scale_y_continuous(breaks=seq(0,max(df$goals),1)) 
}

won_matches <- function(data, wseason, wteam) {
  get_team_goals(data, wseason) %>% 
    filter(team == wteam) %>% 
    filter(scored_goals > lost_goals)
}

won_matches_num <- function(data, wseason, wteam) {
  won_matches(data, wseason, wteam) %>% 
    group_by(team) %>% 
    summarize(cnt=n()) %>% 
    pull(cnt)
}

lost_matches <- function(data, wseason, wteam) {
  get_team_goals(data, wseason) %>% 
    filter(team == wteam) %>% 
    filter(scored_goals < lost_goals)
}

lost_matches_num <- function(data, wseason, wteam) {
  lost_matches(data, wseason, wteam) %>% 
    group_by(team) %>% 
    summarize(cnt=n()) %>% 
    pull(cnt)
}

drawn_matches <- function(data, wseason, wteam) {
  get_team_goals(data, wseason) %>% 
    filter(team == wteam) %>% 
    filter(scored_goals == lost_goals)
}

drawn_matches_num <- function(data, wseason, wteam) {
  drawn_matches(data, wseason, wteam) %>% 
    group_by(team) %>% 
    summarize(cnt=n()) %>% 
    pull(cnt)
}

goals_boxplot <- function(data, wteam) {
  data %>%  
    filter(team == wteam) %>% 
    select(goals, stage)  %>% 
    ggplot(aes(x = stage, y = goals)) +
    geom_boxplot() +
    theme_classic() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

scored_goals_boxplot <- function(data, wseason, wteam) {
  goals_boxplot(
    get_team_goals(data, wseason) %>% select(team, goals=scored_goals, stage),
    wteam
  )
}

lost_goals_boxplot <- function(data, wseason, wteam) {
  goals_boxplot(
    get_team_goals(data, wseason) %>% select(team, goals=lost_goals, stage),
    wteam
  )
}

data <- as.data.frame(read.csv("./PL_dat.csv", header=TRUE, sep=";"))

seasons <- unique(data$season)

get_the_biggest_number_of_goals <- function(data, wseason) {
  (get_team_goals(data, wseason) %>% 
    arrange(desc(goals)))[1,]
}

get_teams_list <- function(data, wseason) {
  sort(unique(get_team_goals(data, wseason) %>% pull(team)))
}

get_matches_table <- function(goals_type, team, wseason, brush_xmin, brush_xmax, brush_ymin, brush_ymax) {
  begin_stage <- ceiling(as.numeric(brush_xmin))
  end_stage <- floor(as.numeric(brush_xmax))
  begin_goals <- ceiling(as.numeric(brush_ymin))
  end_goals <- floor(as.numeric(brush_ymax))
  
  r <- data %>% 
    filter(home_team == team | away_team == team) %>% 
    filter(season == wseason) %>% 
    filter(stage >= begin_stage, stage <= end_stage)
  
  if(goals_type == "scored") {
    r <- r %>% filter((home_team == team & home_team_goal >= begin_goals & home_team_goal <= end_goals)
             | (away_team == team & away_team_goal >= begin_goals & away_team_goal <= end_goals))
  } else {
    r <- r %>% filter((away_team == team & home_team_goal >= begin_goals & home_team_goal <= end_goals)
                | (home_team == team & away_team_goal >= begin_goals & away_team_goal <= end_goals))
  }
  r %>% arrange(stage)
}

# UI
ui <- dashboardPage(
   
   dashboardHeader(title="Soccer"),
    
   dashboardSidebar(
      sidebarMenu(
        selectInput(inputId="season_select", label="season", choices=seasons, selected=seasons[1]),
         menuItem("General", tabName = "general_menu_item", icon=icon("th")),
         menuItem("Team info", tabName =  "team_info_menu_item", icon=icon("user-friends"))
      )
    ),
      
    dashboardBody(
      tabItems(
        tabItem(tabName="general_menu_item",
                fluidRow(
                  box(title="Total number of goals",
                      status="primary",
                      solidHeader = TRUE,
                      collapsible=TRUE,
                      width="100%",
                      plotOutput(outputId="summary_goals_per_season_plot"))
                )
        ),
        tabItem(tabName="team_info_menu_item",
              uiOutput("team_select"),
              fluidRow(
                valueBoxOutput("won_matches_value"),
                valueBoxOutput("drawn_matches_value"),
                valueBoxOutput("lost_matches_value")
              ),
              box(title="Number of scored goals for each stage",
                    status="primary",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    width="100%",
                    plotOutput(outputId="scored_goals_per_season_plot", brush="scored_goals_stage_brush"),
                    tableOutput(outputId="scored_goals_per_season_table")
              ),
              box(title="Number of lost goals for each stage",
                    status="warning",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    width="100%",
                    plotOutput(outputId="lost_goals_per_season_plot", brush="lost_goals_stage_brush"),
                    tableOutput(outputId="lost_goals_per_season_table")
              ),
              fluidRow(
                box(title="Distribution of scored goals in one match",
                    status="primary",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="scored_goals_distribution")),
                box(title="Distribution of lost goals in one match",
                    status="warning",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="lost_goals_distribution"))
              )
          )
        )
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  teams_r <- reactive({
    get_teams_list(data, input$season_select)
  })
  
  output$team_select <- renderUI({
    selectInput("team_select", "Select a team", teams_r(), teams_r()[1])
  })
  
  observeEvent(input$season_select, {
    if(!is.null(input$team_select) && (input$team_select %in% teams_r())[1]) {
      selected_item <- input$team_select
    } else {
      selected_item <- teams_r()[1]
    }
    updateSelectInput(session=session, inputId="team_select", choices=teams_r(), selected=selected_item)
  })
  
  output$summary_goals_per_season_plot <- renderPlot({
     get_summary_goals_plot(data, input$season_select)
  })
  
  validate_at_least_one_match <- function(key) {
    validate(
      need(input[[key]], "Select at least one match")
    )
  }
  
  output$scored_goals_per_season_table <- renderTable({
    validate_at_least_one_match("scored_goals_stage_brush")
    get_matches_table(goals_type="scored",
                      input$team_select,
                      input$season_select,
                      input[["scored_goals_stage_brush"]][["xmin"]],
                      input[["scored_goals_stage_brush"]][["xmax"]],
                      input[["scored_goals_stage_brush"]][["ymin"]],
                      input[["scored_goals_stage_brush"]][["ymax"]])
  })
  
  output$lost_goals_per_season_table <- renderTable({
    validate_at_least_one_match("lost_goals_stage_brush")
    get_matches_table(goals_type="lost",
                      input$team_select,
                      input$season_select,
                      input[["lost_goals_stage_brush"]][["xmin"]],
                      input[["lost_goals_stage_brush"]][["xmax"]],
                      input[["lost_goals_stage_brush"]][["ymin"]],
                      input[["lost_goals_stage_brush"]][["ymax"]])
  })
  
  is_team_chosen_r <- reactive({
    !is.null(input$team_select) && input$team_select %in% teams_r()
  })
  
  output$scored_goals_per_season_plot <- renderPlot({
    if(is_team_chosen_r()) {
      get_goals_for_team_per_stage_plot(data, input$season_select, input$team_select, "scored")
    }
  })
   
  output$lost_goals_per_season_plot <- renderPlot({
    if(is_team_chosen_r()) {
      get_goals_for_team_per_stage_plot(data, input$season_select, input$team_select, "lost")
    }
  })
  
  output$won_matches_value <- renderValueBox({
    if(is_team_chosen_r()) {
      valueBox(won_matches_num(data, input$season_select, input$team_select),
               subtitle="matches won",
               icon=icon("thumbs-up"),
               color="green")
    }
  })
  
  output$lost_matches_value <- renderValueBox({
    if(is_team_chosen_r()) {
      valueBox(lost_matches_num(data, input$season_select, input$team_select),
               subtitle="matches lost",
               icon=icon("thumbs-down"),
               color="red")
    }
  })
  
  output$drawn_matches_value <- renderValueBox({
    if(is_team_chosen_r()) {
      valueBox(drawn_matches_num(data, input$season_select, input$team_select),
               subtitle="matches drawn",
               icon=icon("grip-lines"),
               color="yellow")
    }
  })
  
  output$scored_goals_distribution <- renderPlot({
    if(is_team_chosen_r()) {
      scored_goals_boxplot(data, input$season_select, input$team_select)
    }
  })
  
  output$lost_goals_distribution <- renderPlot({
    if(is_team_chosen_r()) {
      lost_goals_boxplot(data, input$season_select, input$team_select)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

