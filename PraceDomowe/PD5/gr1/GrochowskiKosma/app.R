library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyWidgets)
library(markdown)
library(DT)
dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

compute_points <- function(x, y) {
  ifelse(x > y, 3, ifelse(x == y, 1, 0))
}

point_dat <- mutate(dat, home_team_points = compute_points(home_team_goal, away_team_goal),
                    away_team_points = compute_points(away_team_goal, home_team_goal))

win_perc_dat <- rbind(select(point_dat, team = home_team, points = home_team_points, season), 
                      select(point_dat, team = away_team, points = away_team_points, season)) %>% 
  mutate(win = as.numeric(points == 3) + (points == 1)/2) %>% 
  group_by(team, season) %>% 
  summarise(win_perc = mean(win)) %>% 
  complete(season, nesting(team), fill = list(win_perc = NA)) %>% 
  ungroup()


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "BookieHelper"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      selectInput(inputId = "selected_season", label = "Select season", 
                  choices = levels(dat[["season"]])),
      selectInput(inputId = "home_team", label = "Choose home team", choices = levels(win_perc_dat[["team"]])),
      selectInput(inputId = "away_team", label = "Choose away team", choices = levels(win_perc_dat[["team"]])),
      
      sliderInput("round", "Choose round:",
                  min = 0, max = 38,
                  value = 38, step = 1)
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              
              fluidRow(box(title = "Home Team's last matches", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, 
                  dataTableOutput("last_matches_home")
              ),
              box(title = "Away Team's last matches", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, 
                  dataTableOutput("last_matches_away")
              )),
              fluidRow(box(title = "Last matches between teams", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, 
                  dataTableOutput("matches_between")
              ),
              box(title = "Matches between teams results", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, 
                  plotOutput("between_overview")
              )),
              
              fluidRow(box(title = "Current league form - last matches", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, 
                  plotOutput("last_matches_points")
              ),
              
              box(title = "Current table", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, 
                  plotOutput("total_point_plot") )
              ),
              
              fluidRow(
                box(title = "Goals shot", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, 
                  plotOutput("total_goal_plot"))
              ,
              box(title = "Goals conceded", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, 
                  plotOutput("total_lost_goal_plot"))
      )),
      tabItem("about",
              "About the app",
              includeMarkdown("example.md")
      )
    )
  )
)

server <- function(input, output) {
  
  
  points_against_dat <- reactive({
    
    
    # | away_team == input[["home_team"]]
    rbind(filter(point_dat, home_team == input[["home_team"]]) %>% 
            select(opponent = away_team, points = home_team_points),
          filter(point_dat, away_team == input[["home_team"]]) %>% 
            select(opponent = home_team, points = away_team_points)) %>% 
      group_by(opponent) %>% 
      summarise(mean_points = mean(points))
  })
  
  
  output[["n_matches_plot"]] <- renderPlot(
    select(dat, season, home_team, away_team) %>% 
      melt(id.vars = "season") %>% 
      group_by(value) %>% 
      summarise(n = length(value)) %>% 
      arrange(desc(n)) %>% 
      ggplot(aes(x = value, y = n)) +
      geom_col()
  )
  
  output[["n_goals_plot"]] <- renderPlot(
    select(dat, season, home_team_goal, away_team_goal) %>% 
      melt(id.vars = "season") %>% 
      group_by(season, variable) %>% 
      summarise(total = sum(value)) %>% 
      ggplot(aes(x = season, y = total, fill = variable)) +
      geom_col(position = "dodge")
  )
  
  
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
  output[["between_overview"]] <- renderPlot({
    
    compute_result <- function(x, y) {
      ifelse(x > y, "Win", ifelse(x == y, "Draw", "Loss"))
    }
    matches_between <- filter(dat, ((home_team == input[["home_team"]] & away_team == input[["away_team"]] )
                                    | (home_team == input[["away_team"]] & away_team == input[["home_team"]]))
                              & ((season == input[["selected_season"]] & stage<= input[["round"]])
                                 | (input[["selected_season"]] == "2015/2016" & season == "2014/2015")))#)
    results_between <- mutate(matches_between, 
                              home_result = compute_result(home_team_goal, away_team_goal), 
                              away_result = compute_result(away_team_goal, home_team_goal))
    flat_matches <-   rbind(select(results_between, season, team_result = home_result, 
                                team_name = home_team),
                            select(results_between, season, team_result = away_result, 
                                   team_name = away_team)
)
    flat_matches <-  filter(flat_matches, team_name == input[["home_team"]]) %>% 
                          group_by(team_result)  %>%  summarise(count = n())
    ggplot(flat_matches, aes(x = team_result, y = count, fill=team_result)) +
      geom_col(position = "dodge") + 
      scale_fill_manual( values = c( "Win"="tomato", "Loss"="blue", "Draw"="gray" ), guide = FALSE )+ 
      labs(x = "Result", y = "Count")
    
  })
  output[["matches_between"]] <- renderDataTable(
    #ifelse(input[["selected_season"]] == "2014/2015", 
    #       filter(dat, ((home_team == input[["home_team"]] & away_team == input[["away_team"]] )
    #                     | (home_team == input[["away_team"]] & away_team == input[["home_team"]]))
    #              & season == input[["selected_season"]] & stage<= input[["round"]]),
    datatable(filter(dat, ((home_team == input[["home_team"]] & away_team == input[["away_team"]] )
                        | (home_team == input[["away_team"]] & away_team == input[["home_team"]]))
                  & ((season == input[["selected_season"]] & stage<= input[["round"]])
                     | (input[["selected_season"]] == "2015/2016" & season == "2014/2015"))),
              rownames = FALSE,
              colnames = c("Season","Stage", "Date", "Home team goals", "Away team goals", "Home team", "Away team"),
              options = list(dom = 't')
    )
           #, 
           #colnames = c("Season","Stage", "Date", "Home team goals", "Away team goals", "Home team", "Away team")
    
  )
  output[["last_matches_home"]] <- renderDataTable(
    datatable(filter(dat, (home_team == input[["home_team"]] | away_team == input[["home_team"]] )
               & season == input[["selected_season"]] & stage > input[["round"]] - 5 & stage<= input[["round"]]),
              rownames = FALSE,
              colnames = c("Season","Stage", "Date", "Home team goals", "Away team goals", "Home team", "Away team"),
              options = list(dom = 't')
              )
  )
  output[["last_matches_away"]] <- renderDataTable(
    datatable(filter(dat, (home_team == input[["away_team"]] | away_team == input[["away_team"]] )
           & season == input[["selected_season"]] & stage > input[["round"]] - 5 & stage<= input[["round"]]),
           rownames = FALSE,
           colnames = c("Season","Stage", "Date", "Home team goals", "Away team goals", "Home team", "Away team"),
           options = list(dom = 't')
    )
    #, 
    #colnames = c("Season","Stage", "Date", "Home team goals", "Away team goals", "Home team", "Away team")
    
  )
  output[["total_lost_goal_plot"]] <- renderPlot({
    up_to_round <- filter(dat,stage <= input[["round"]])
    total_goal <- rbind(select(up_to_round, season, team_goal = home_team_goal, 
                               team_name = away_team), 
                        select(up_to_round, season, team_goal = away_team_goal, 
                               team_name = home_team)) %>% 
      group_by(season, team_name) %>% 
      summarise(total_goals = sum(team_goal)) 
    
    team_order <- filter(total_goal, season == input[["selected_season"]]) %>% 
      arrange(desc(total_goals)) %>% 
      pull(team_name) %>% 
      as.character()
    filter_total_goal <- filter(total_goal, season == input[["selected_season"]])
    filter_total_goal <- filter_total_goal %>% mutate( ToHighlight = ifelse( team_name == input[["home_team"]], "home",  ifelse( team_name == input[["away_team"]], "away", "no" ) ) )
    
    mutate(filter_total_goal, 
           team_name = factor(team_name, levels = team_order[20:1])) %>%
      complete(season, nesting(team_name), fill = list(total_goals = 0)) %>% 
      ggplot(aes(x = team_name, y = total_goals, fill = ToHighlight )) +
      geom_col(position = "dodge") +
      coord_flip()+ 
      scale_fill_manual( values = c( "home"="tomato", "away"="blue", "no"="gray" ), guide = FALSE )+ 
      labs(x = "Team", y = "Goals conceded")
  })
  output[["total_goal_plot"]] <- renderPlot({
    up_to_round <- filter(dat,stage <= input[["round"]])
    total_goal <- rbind(select(up_to_round, season, team_goal = home_team_goal, 
                               team_name = home_team), 
                        select(up_to_round, season, team_goal = away_team_goal, 
                               team_name = away_team)) %>% 
      group_by(season, team_name) %>% 
      summarise(total_goals = sum(team_goal)) 
    
    team_order <- filter(total_goal, season == input[["selected_season"]]) %>% 
      arrange(desc(total_goals)) %>% 
      pull(team_name) %>% 
      as.character()
    filter_total_goal <- filter(total_goal, season == input[["selected_season"]])
    filter_total_goal <- filter_total_goal %>% mutate( ToHighlight = ifelse( team_name == input[["home_team"]], "home",  ifelse( team_name == input[["away_team"]], "away", "no" ) ) )
    
    mutate(filter_total_goal, 
           team_name = factor(team_name, levels = team_order[20:1])) %>%
      complete(season, nesting(team_name), fill = list(total_goals = 0)) %>% 
      ggplot(aes(x = team_name, y = total_goals, fill = ToHighlight )) +
      geom_col(position = "dodge") +
      coord_flip()+ 
      scale_fill_manual( values = c( "home"="tomato", "away"="blue", "no"="gray" ), guide = FALSE )+ 
      labs(x = "Team", y = "Goals shot")
  })
  output[["total_point_plot"]] <-  renderPlot({
    up_to_round <- filter(point_dat,stage <= input[["round"]])
    
      flat_points <- rbind(select(up_to_round, season, team_points = home_team_points, 
                                team_name = home_team), 
                            select(up_to_round, season, team_points = away_team_points, 
                                  team_name = away_team))%>% 
                            group_by(season, team_name)%>% 
                            summarise(total_points = sum(team_points))
      team_order <- filter(flat_points, season == input[["selected_season"]]) %>% 
        arrange(desc(total_points)) %>% 
        pull(team_name) %>% 
        as.character()
      filter_flat_points <- filter(flat_points, season == input[["selected_season"]])
      filter_flat_points <- filter_flat_points %>% mutate( ToHighlight = ifelse( team_name == input[["home_team"]], "home",  ifelse( team_name == input[["away_team"]], "away", "no" ) ) )
      mutate(filter_flat_points, 
             team_name = factor(team_name, levels = team_order[20:1])) %>%
        complete(season, nesting(team_name), fill = list(total_points = 0)) %>% 
        ggplot(aes(x = team_name, y = total_points, fill = ToHighlight )) +
        geom_col(position = "dodge")+
        coord_flip()+ 
        scale_fill_manual( values = c( "home"="tomato", "away"="blue", "no"="gray" ), guide = FALSE )+ 
        labs(x = "Team", y = "Points")
  })
  
  output[["last_matches_points"]] <- renderPlot({
    up_to_round <- filter(point_dat,stage <= input[["round"]] & stage > input[["round"]] - 5)
    
    flat_points <- rbind(select(up_to_round, season, team_points = home_team_points, 
                                team_name = home_team), 
                         select(up_to_round, season, team_points = away_team_points, 
                                team_name = away_team))%>% 
      group_by(season, team_name)%>% 
      summarise(total_points = sum(team_points))
    team_order <- filter(flat_points, season == input[["selected_season"]]) %>% 
      arrange(desc(total_points)) %>% 
      pull(team_name) %>% 
      as.character()
    filter_flat_points <- filter(flat_points, season == input[["selected_season"]])
    filter_flat_points <- filter_flat_points %>% mutate( ToHighlight = ifelse( team_name == input[["home_team"]], "home",  ifelse( team_name == input[["away_team"]], "away", "no" ) ) )
    mutate(filter_flat_points, 
           team_name = factor(team_name, levels = team_order[20:1])) %>%
      complete(season, nesting(team_name), fill = list(total_points = 0)) %>% 
      ggplot(aes(x = team_name, y = total_points, fill = ToHighlight )) +
      geom_col(position = "dodge")+
      coord_flip()+ 
      scale_fill_manual( values = c( "home"="tomato", "away"="blue", "no"="gray" ), guide = FALSE )+ 
      labs(x = "Team", y = "Last 5 matches points")
  })
}

shinyApp(ui, server)
