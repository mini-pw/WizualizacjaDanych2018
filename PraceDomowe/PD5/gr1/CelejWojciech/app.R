library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(DT)
library(waffle)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")
dat$date <- as.Date(dat$date)

title <- tags$a(tags$img(src="https://static.independent.co.uk/s3fs-public/thumbnails/image/2016/02/09/09/premier-league.jpg?w968h681", 
                         height=60))

colorado <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)
  src_levels <- levels(src)
  brave <- boulder %in% src_levels
  if (all(brave)) {
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels))
    b_vec <- rep("plain", length(src_levels))
    b_vec[b_pos] <- "bold"
    b_vec
  }
}

colorado2 <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)
  src_levels <- levels(src)
  brave <- boulder %in% src_levels
  if (all(brave)) {
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels))
    b_vec <- rep(11, length(src_levels))
    b_vec[b_pos] <- 14
    b_vec
  }
}

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = title),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("See also", icon = icon("send",lib='glyphicon'), 
               href = "https://www.premierleague.com/"),
      radioButtons(
        "season",
        "Select season",
        choices = unique(dat$season)
      ),
      sliderInput(
        "stage",
        "Stages",
        min = min(dat$stage),
        max = max(dat$stage),
        value = c(min(dat$stage), max(dat$stage)),
        step = 1, 
        ticks = FALSE
      ),
      selectInput(
        "team",
        "Select specific team", 
        choices = sort(unique(dat$home_team))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(box(title = "All teams' points", status = "primary",
                          solidHeader = TRUE, collapsible = TRUE, 
                          plotOutput("points_plot"), width = 5),
                      box(title = "All teams' goals for/against", status = "primary",
                          solidHeader = TRUE, collapsible = TRUE, 
                          plotOutput("goals_plot"), width = 7)),
              fluidRow(box(title = "Team's points during stages", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, 
                           plotOutput("team_points_plot"), width = 8),
                       box(title = "Team's statistics", status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, 
                       valueBoxOutput("team_points_per_match"),
                       valueBoxOutput("team_goals_for_per_match"),
                       valueBoxOutput("team_goals_against_per_match"), 
                       width = 4)), 
              fluidRow(box(title = "Team's results", status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, 
                         dataTableOutput("matches_table", height = "100%"), width = 5),
                      box(title = "Team's matches", status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, 
                         plotOutput("team_matches_plot"), width = 4),
                      box(title = "Team's goals", status = "primary",
                          solidHeader = TRUE, collapsible = TRUE, 
                          plotOutput("team_goals_plot"), width = 3))
      ) ,
      tabItem("about",
              "App developed for WD project"
      )
    )
  )
)
server <- function(input, output) {
  dat_r <- reactive({
    validate(
      need(input[["season"]], "Select season")
    )
    filter(dat, season==input[["season"]] & between(stage, input[["stage"]][1], input[["stage"]][2]))
  })
  
   dat1_r <-reactive({
     rbind(select(dat_r(), stage, goals_for = home_team_goal, 
                               goals_against = away_team_goal, team_name = home_team),
          select(dat_r(), stage, goals_for = away_team_goal, 
                 goals_against = home_team_goal, team_name = away_team)) 
    })
  
  total_goal_r <- reactive({
    total_goal <- group_by(dat1_r(), team_name) %>% 
    summarise(goals_for = sum(goals_for),
              goals_against = sum(goals_against)) %>% 
    mutate(diff = goals_for - goals_against)
    
    team_order <- arrange(total_goal, desc(diff)) %>% 
      pull(team_name) %>% 
      as.character()
    
    total_goal$team_name <- factor(total_goal$team_name, levels = team_order)
    gather(total_goal, type, goals, c("goals_for", "goals_against"), factor_key = TRUE)
  })
  
  point_r <- reactive(
    mutate(dat1_r(), points = ifelse(goals_for>goals_against, 3, ifelse(goals_for==goals_against, 1, 0)))
  )
  
  total_point_r <- reactive({
      total_point <- point_r() %>% 
      group_by(team_name) %>% 
      summarise(points = sum(points))
      
      team_order <- arrange(total_point, desc(points)) %>% 
        pull(team_name) %>% 
        as.character()
      total_point$team_name <- factor(total_point$team_name, levels = team_order)
      total_point
  })
  
  team_matches_r <- reactive({
    validate(
      need(input[["team"]] %in% dat1_r()$team_name, "Team doesn't play this season")
    )
    select(dat_r(), -season) %>% 
      filter(home_team==input[["team"]] | away_team==input[["team"]])
  })
  
  team_points_r <- reactive({
    team_matches_r()
    temp <- point_r() %>% 
      filter(team_name == input[["team"]]) %>% 
      arrange(stage)

    temp$points <- cumsum(temp$points)
    temp
  })
  
  team_matches_stats_r <- reactive({
    team_matches_r()
    temp <- mutate(dat1_r(), result = ifelse(goals_for>goals_against, "win", ifelse(goals_for==goals_against, "draw", "loss"))) %>% 
      filter(team_name == input[["team"]]) %>% 
      select(team_name, result) %>% 
      group_by(result) %>% 
      summarise(number = n())
    temp$result = factor(temp$result, levels = c("win", "draw", "loss"))
    temp
  })
  
  team_goals_stats_r <- reactive({
    team_matches_r()
    total_goal_r() %>% 
      filter(team_name == input[["team"]]) %>% 
      select(type, goals)
  })
  
  output[["points_plot"]] <- renderPlot({
    
    ggplot(data = total_point_r(), aes(x = team_name, y = points, fill = "blue")) +
      geom_bar(stat = "identity", position = "dodge", width = .7) +
      geom_text(aes(x = team_name, label = points), 
                position=position_dodge(width=.8), vjust = -0.4) +
      scale_y_continuous(expand = expand_scale(add = c(0, 8))) +
      labs(title = "", x = "", y = "") +
      theme(axis.text.x = element_text(size = colorado2(total_point_r()$team_name, input[["team"]]), 
                                       angle = 90, vjust = 0.4, 
                                       face = colorado(total_point_r()$team_name, input[["team"]])),
            axis.text.y = element_text(size=15)) +
      theme_hc() + scale_fill_hc(guide=FALSE)
  })
  
  output[["goals_plot"]] <- renderPlot(
    ggplot(data = total_goal_r(), aes(x = team_name, y = goals, fill = type)) +
      geom_bar(stat = "identity", position = "dodge", width = .7) +
      geom_text(aes(x = team_name, label = goals), 
                position=position_dodge(width=.8), vjust = -0.4) +
      scale_y_continuous(expand = expand_scale(add = c(0, 8))) +
      labs(title = "", x = "", y = "") +
      theme(axis.text.x = element_text(size = colorado2(total_goal_r()$team_name, input[["team"]]), 
                                       angle = 90, vjust = 0.4, 
                                       face = colorado(total_goal_r()$team_name, input[["team"]]))) +
      theme_hc() + scale_fill_hc(name = "Goals: ", labels = c("for", "against")) +
      theme(legend.position = "right", legend.text = element_text(size=15),
            legend.title = element_text(size=17),
            axis.text.y = element_text(size=15))
  )
  
  output[["team_points_plot"]] <- renderPlot(
    ggplot(data = team_points_r(), aes(x = stage, y = points)) +
      geom_line(color = "#7CB5EC", size = 2) +
      geom_point(color = "#444349", size = 4) +
      scale_x_continuous(expand = expand_scale(add = c(1, 1)), 
                         breaks = seq(2, 38, 2)) +
      scale_y_continuous(expand = expand_scale(add = c(5, 8)),
                         breaks = seq(0, 200, 10)) +
      labs(title = "", x = "", y = "") +
      theme_hc() + 
      theme(panel.grid.major.x = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
            axis.text = element_text(size=15))
  )
  
  output[["team_points_per_match"]] <- renderValueBox(
    valueBox(
      format(mean(pull(filter(point_r(), team_name==input[["team"]]), points)), digits = 3), 
             "Points per match", color = "purple"
    )
  )
  
  output[["team_goals_for_per_match"]] <- renderValueBox(
    valueBox(
      format(mean(pull(filter(dat1_r(), team_name==input[["team"]]), goals_for)), digits = 3), 
      "Goals for per match", color = "blue"
    )
  )

  output[["team_goals_against_per_match"]] <- renderValueBox(
    valueBox(
      format(mean(pull(filter(dat1_r(), team_name==input[["team"]]), goals_against)), digits = 3), 
      "Goals against per match", color = "red"
    )
  )
  
  output[["matches_table"]] <- renderDataTable(
    mutate(team_matches_r(), result = paste(home_team_goal, away_team_goal, sep = ":")) %>% 
      select(stage, date, home_team, away_team, result), 
    colnames = c("Stage", "Date", "Home team", "Away team", "Result"), 
    rownames= FALSE, 
    class = 'cell-border stripe',
    options = list(
      pageLength = 8,
      lengthMenu = c(8, 16, 24, -1),
      order = list(1, "asc")
    )
  )
  
  output[["team_matches_plot"]] <- renderPlot(
    ggplot(team_matches_stats_r(), aes(x = 2, y = number, fill = result)) +
      geom_col(stat = "identity", color = "white") +
      geom_text(aes(label = number), position = position_stack(vjust=0.5), size = 9, color = "white") +
      coord_polar("y", start = 0) +
      labs(title = "", x = "", y = "") +
      theme_hc() + scale_fill_manual(name = "Matches: ", labels = c("win", "draw", "loss"), 
                                     values = c("#0073B6", "#444349", "#DD4C39")) +
      theme_void() +
      theme(legend.position = "right", axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
            legend.text = element_text(size=15),
            legend.title = element_text(size=17)) +
      xlim(0.5, 2.5)
  )

  output[["team_goals_plot"]] <- renderPlot({
    waffle(team_goals_stats_r(), rows = 10) +
      scale_fill_manual(name = "Goals: ", labels = c("for", "against", ""), 
                                     #values = c("#7CB5EC", "#444349", "#FFFFFF")) +
                        values = c("#0073B6", "#DD4C39", "#FFFFFF")) +
      theme(legend.text = element_text(size =15), legend.title = element_text(size=17), 
            legend.position = "right")
  })
  
}
shinyApp(ui, server)