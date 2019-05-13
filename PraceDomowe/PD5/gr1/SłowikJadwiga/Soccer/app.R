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
    scale_y_continuous(breaks=seq(0,max_goals_num,1), limits=c(0,max_goals_num)) 
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

goals_for_each_common_match <- function(data, wseason, first_team, second_team) {
  team_goals <- get_team_goals(
    data %>% filter((home_team == first_team & away_team == second_team) | (home_team == second_team & away_team == first_team)),
    wseason)
  
  team_goals %>%
    mutate(team=ifelse(team == first_team, "first", "second")) %>% 
    select(team, goals=scored_goals, date)
}

goals_for_each_common_match_plot <- function(data, wseason, first_team, second_team) {
  r <- goals_for_each_common_match(data, wseason, first_team, second_team) %>% 
    mutate(date = gsub(x=date, pattern=" 00:00:00", replacement="", fixed=TRUE))
  r %>% 
    ggplot(aes(x=date, y=goals, group=team, fill=team)) +
    geom_col(position = position_dodge(-0.9)) +
    theme_minimal() +
    geom_text(aes(label = goals), color = "Black", size = 5, position=position_dodge(-0.9), vjust=-1) +
    scale_y_continuous(breaks=seq(0, max(r$goals)+1), limits = c(0,max(r$goals)+1)) +
    theme(axis.text.x = element_text(size=15))
}

data <- as.data.frame(read.csv("./PL_dat.csv", header=TRUE, sep=";"))

max_goals_num <- max(rbind(data %>% select(g=home_team_goal), data %>% select(g=away_team_goal)) %>% pull(g))

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
              fluidRow(
                box(uiOutput("team_select_1")),
                box(uiOutput("team_select_2"))
              ),
              fluidRow(
                box(
                  valueBoxOutput("won_matches_value_1"),
                  valueBoxOutput("drawn_matches_value_1"),
                  valueBoxOutput("lost_matches_value_1")
                ),
                box(
                  valueBoxOutput("won_matches_value_2"),
                  valueBoxOutput("drawn_matches_value_2"),
                  valueBoxOutput("lost_matches_value_2")
                )
              ),
              box(title="Matches between two chosen teams",
                  status="primary",
                  width="70%",
                  solidHeader=TRUE,
                  collapsible=TRUE,
                  plotOutput(outputId="matches_between_teams_plot")),
              fluidRow(
                box(title="Number of scored goals for each stage (1)",
                    status="primary",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="scored_goals_per_season_plot_1", brush="scored_goals_stage_brush_1"),
                    tableOutput(outputId="scored_goals_per_season_table_1")
                ),
                box(title="Number of scored goals for each stage (2)",
                    status="primary",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="scored_goals_per_season_plot_2", brush="scored_goals_stage_brush_2"),
                    tableOutput(outputId="scored_goals_per_season_table_2")
                )
              ),
              fluidRow(
                box(title="Number of lost goals for each stage (1)",
                    status="warning",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="lost_goals_per_season_plot_1", brush="lost_goals_stage_brush_1"),
                    tableOutput(outputId="lost_goals_per_season_table_1")
                ),
                box(title="Number of lost goals for each stage (2)",
                    status="warning",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="lost_goals_per_season_plot_2", brush="lost_goals_stage_brush_2"),
                    tableOutput(outputId="lost_goals_per_season_table_2")
                )
              ),
              fluidRow(
                box(title="Distribution of scored goals in one match (1)",
                    status="primary",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="scored_goals_distribution_1")),
                box(title="Distribution of scored goals in one match (2)",
                    status="primary",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="scored_goals_distribution_2"))
              ),
              fluidRow(
                box(title="Distribution of lost goals in one match (1)",
                    status="warning",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="lost_goals_distribution_1")),
                box(title="Distribution of lost goals in one match (2)",
                    status="warning",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    plotOutput(outputId="lost_goals_distribution_2"))
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
  
  output$team_select_1 <- renderUI({
    selectInput("team_select_1", "Select the first team", teams_r(), teams_r()[1])
  })
  
  output$team_select_2 <- renderUI({
    selectInput("team_select_2", "Select the second team", teams_r(), teams_r()[1])
  })
  
  observeEvent(input$season_select, {
    if(!is.null(input$team_select_1) && (input$team_select_1 %in% teams_r())[1]) {
      selected_item <- input$team_select_1
    } else {
      selected_item <- teams_r()[1]
    }
    updateSelectInput(session=session, inputId="team_select_1", choices=teams_r(), selected=selected_item)
  })
  
  observeEvent(input$season_select, {
    if(!is.null(input$team_select_2) && (input$team_select_2 %in% teams_r())[1]) {
      selected_item <- input$team_select_2
    } else {
      selected_item <- teams_r()[1]
    }
    updateSelectInput(session=session, inputId="team_select_2", choices=teams_r(), selected=selected_item)
  })
  
  validate_at_least_one_match <- function(key) {
    validate(
      need(input[[key]], "Select at least one match")
    )
  }
  
  is_team_chosen_r_1 <- reactive({
    !is.null(input$team_select_1) && input$team_select_1 %in% teams_r()
  })
  
  is_team_chosen_r_2 <- reactive({
    !is.null(input$team_select_2) && input$team_select_2 %in% teams_r()
  })
  
  output[["matches_between_teams_plot"]] <- renderPlot({
    if(is_team_chosen_r_1() && is_team_chosen_r_2()) {
      validate(
        need(input$team_select_1 != input$team_select_2, "Choose different teams")
      )
      goals_for_each_common_match_plot(data, input$season_select, input$team_select_1, input$team_select_2)
    }
  })
  
  output[["summary_goals_per_season_plot"]] <- renderPlot({
    get_summary_goals_plot(data, input$season_select)
  })
  
  output[["scored_goals_per_season_table_1"]] <- renderTable({
    validate_at_least_one_match("scored_goals_stage_brush_1")
    get_matches_table(goals_type="scored",
                      input$team_select_1,
                      input$season_select,
                      input[["scored_goals_stage_brush_1"]][["xmin"]],
                      input[["scored_goals_stage_brush_1"]][["xmax"]],
                      input[["scored_goals_stage_brush_1"]][["ymin"]],
                      input[["scored_goals_stage_brush_1"]][["ymax"]])
  })
  
  output[["scored_goals_per_season_table_2"]] <- renderTable({
    validate_at_least_one_match("scored_goals_stage_brush_2")
    get_matches_table(goals_type="scored",
                      input$team_select_2,
                      input$season_select,
                      input[["scored_goals_stage_brush_2"]][["xmin"]],
                      input[["scored_goals_stage_brush_2"]][["xmax"]],
                      input[["scored_goals_stage_brush_2"]][["ymin"]],
                      input[["scored_goals_stage_brush_2"]][["ymax"]])
  })
  
  output[["lost_goals_per_season_table_1"]] <- renderTable({
    validate_at_least_one_match("lost_goals_stage_brush_1")
    get_matches_table(goals_type="lost",
                      input$team_select_1,
                      input$season_select,
                      input[["lost_goals_stage_brush_1"]][["xmin"]],
                      input[["lost_goals_stage_brush_1"]][["xmax"]],
                      input[["lost_goals_stage_brush_1"]][["ymin"]],
                      input[["lost_goals_stage_brush_1"]][["ymax"]])
  })
  
  output[["lost_goals_per_season_table_2"]] <- renderTable({
    validate_at_least_one_match("lost_goals_stage_brush_2")
    get_matches_table(goals_type="lost",
                      input$team_select_2,
                      input$season_select,
                      input[["lost_goals_stage_brush_2"]][["xmin"]],
                      input[["lost_goals_stage_brush_2"]][["xmax"]],
                      input[["lost_goals_stage_brush_2"]][["ymin"]],
                      input[["lost_goals_stage_brush_2"]][["ymax"]])
  })
  
  output[["scored_goals_per_season_plot_1"]] <- renderPlot({
    if(is_team_chosen_r_1()) {
      get_goals_for_team_per_stage_plot(data, input$season_select, input$team_select_1, "scored")
    }
  })
  
  output[["scored_goals_per_season_plot_2"]] <- renderPlot({
    if(is_team_chosen_r_2()) {
      get_goals_for_team_per_stage_plot(data, input$season_select, input$team_select_2, "scored")
    }
  })
  
  output[["lost_goals_per_season_plot_1"]] <- renderPlot({
    if(is_team_chosen_r_1()) {
      get_goals_for_team_per_stage_plot(data, input$season_select, input$team_select_1, "lost")
    }
  })
  
  output[["lost_goals_per_season_plot_2"]] <- renderPlot({
    if(is_team_chosen_r_2()) {
      get_goals_for_team_per_stage_plot(data, input$season_select, input$team_select_2, "lost")
    }
  })
  
  output[["won_matches_value_1"]] <- renderValueBox({
    if(is_team_chosen_r_1()) {
      valueBox(won_matches_num(data, input$season_select, input$team_select_1),
               subtitle=paste("matches won"),
               icon=icon("thumbs-up"),
               color="green")
    }
  })
  
  output[["won_matches_value_2"]] <- renderValueBox({
    if(is_team_chosen_r_2()) {
      valueBox(won_matches_num(data, input$season_select, input$team_select_2),
               subtitle="matches won",
               icon=icon("thumbs-up"),
               color="green")
    }
  })
  
  output[["lost_matches_value_1"]] <- renderValueBox({
    if(is_team_chosen_r_1()) {
      valueBox(lost_matches_num(data, input$season_select, input$team_select_1),
               subtitle="matches lost",
               icon=icon("thumbs-down"),
               color="red")
    }
  })
  
  output[["lost_matches_value_2"]] <- renderValueBox({
    if(is_team_chosen_r_2()) {
      valueBox(lost_matches_num(data, input$season_select, input$team_select_2),
               subtitle="matches lost",
               icon=icon("thumbs-down"),
               color="red")
    }
  })
  
  output[["drawn_matches_value_1"]] <- renderValueBox({
    if(is_team_chosen_r_1()) {
      valueBox(drawn_matches_num(data, input$season_select, input$team_select_1),
               subtitle="matches drawn",
               icon=icon("grip-lines"),
               color="yellow")
    }
  })
  
  output[["drawn_matches_value_2"]] <- renderValueBox({
    if(is_team_chosen_r_2()) {
      valueBox(drawn_matches_num(data, input$season_select, input$team_select_2),
               subtitle="matches drawn",
               icon=icon("grip-lines"),
               color="yellow")
    }
  })
  
  output[["scored_goals_distribution_1"]] <- renderPlot({
    if(is_team_chosen_r_1()) {
      scored_goals_boxplot(data, input$season_select, input$team_select_1)
    }
  })
  
  output[["scored_goals_distribution_2"]] <- renderPlot({
    if(is_team_chosen_r_2()) {
      scored_goals_boxplot(data, input$season_select, input$team_select_2)
    }
  })
  
  output[["lost_goals_distribution_1"]] <- renderPlot({
    if(is_team_chosen_r_1()) {
      lost_goals_boxplot(data, input$season_select, input$team_select_1)
    }
  })
  
  output[["lost_goals_distribution_2"]] <- renderPlot({
    if(is_team_chosen_r_2()) {
      lost_goals_boxplot(data, input$season_select, input$team_select_2)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

