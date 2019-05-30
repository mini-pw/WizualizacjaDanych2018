library(shiny)
library(shinydashboard)
library(jsonlite)
library(dplyr)
library(tidyr)
library(httr)
library(ggplot2)
library(stringr)
library(RCurl)

# get yours at https://www.football-data.org/client/login
API_KEY <- ""

load_premier_league_matches <- function(API_KEY) {
  if (str_length(API_KEY) == 0) {
    result_newer = fromJSON("result.json")
  }
  else {
    available_seasons <- list(2017, 2018)
    
    results <- lapply(available_seasons, function(x) {
      url <-
        paste0("http://api.football-data.org/v2/competitions/2021/matches?season=",
               x)
      httpResponse <-
        GET(url,
            add_headers("X-Auth-Token" = API_KEY),
            accept_json())
      result <- fromJSON(content(httpResponse, "text"))
      
      matches <- result$matches
      season <- paste0(x, "/", x + 1)
      results <- data.frame(
        stage = matches$matchday,
        date = paste0(as.Date(matches$utcDate, '%Y-%m-%d'), " 00:00:00"),
        home_team_goal = matches$score$fullTime$homeTeam,
        away_team_goal = matches$score$fullTime$awayTeam,
        home_team = matches$homeTeam$name,
        away_team = matches$awayTeam$name
      ) %>%
        mutate(
          season = season,
          home_team = str_replace_all(home_team, " FC|AFC | AFC", ""),
          away_team = str_replace_all(away_team, " FC|AFC | AFC", "")
        ) %>%
        mutate_if(is.factor, as.character)
      
      return(results)
    })
    
    result_newer <- bind_rows(results)
    write(toJSON(result_newer), "result.json")
  }
  
  result_older <-
    read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv") %>%
    mutate_if(is.factor, as.character)
  merged_results <- bind_rows(result_older, result_newer) %>%
    mutate(
      season = as.factor(season),
      home_team = as.factor(home_team),
      away_team = as.factor(away_team)
    )
  
  return(merged_results)
}

matches <- load_premier_league_matches(API_KEY)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("About", tabName = "about", icon = icon("info"))
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "dashboard",
      h2("Betting helper app"),
      h3("written by Piotr Podbielski"),
      box(title = "How to use",
          "Please select two teams for head-to-head comparison."),
      box(
        title = "Pick two teams",
        selectInput("first_team",
                    "Select first team",
                    choices = sort(unique(matches$home_team))),
        uiOutput('second_team_ui'),
        uiOutput('last_matches')
      ),
      box(title = "Last matches between teams",
          dataTableOutput("matches_table")
      ),
      box(title = "Past results",
          valueBoxOutput("first_team_wins"),
          valueBoxOutput("draws"),
          valueBoxOutput("second_team_wins")),
      box(title = "Quick stats about teams",
          selectInput(inputId = "selected_seasons", label = "Pick seasons", 
                      choices = levels(matches[["season"]]),
                      multiple = TRUE,
                      selected = levels(matches[["season"]]))),
      box(title = "Average goals",
          fluidRow(
            column(6, plotOutput("average_goals_scored")),
            column(6, plotOutput("average_goals_conceded"))
          )
      ),
      box(title = "Results percentage",
          fluidRow(
            column(6, plotOutput("results_percentage_first_team")),
            column(6, plotOutput("results_percentage_second_team"))
          )
      )
    ),
    tabItem(tabName = "about",
            h2("About this app"),
            box("App made as part of Data Visualisation course @ MiNI PW."))
  )),
  skin = "black"
)

server <- function(input, output) {
  second_teams_r <- reactive({
    sort(unique(filter(matches, home_team != input[["first_team"]])$home_team))
  })
  
  team_matches_r <- reactive({
    matches %>% 
      filter(home_team == input[["first_team"]] & away_team == input[["second_team"]]
         | away_team == input[["first_team"]] & home_team == input[["second_team"]])
  })
  
  output[["second_team_ui"]] = renderUI({
    selectInput("second_team",
                "Select second team",
                choices = second_teams_r())
  })
  
  output[["matches_table"]] <- renderDataTable(
    mutate(team_matches_r(), result = paste(home_team_goal, away_team_goal, sep = ":")) %>%
      select(season, date, home_team, result, away_team),
    options = list(
      order = list(1, "desc"),
      columns = list(
        list(title = 'Season'),
        list(title = 'Date'),
        list(title = 'Home team'),
        list(title = 'Result'),
        list(title = 'Away team')
      )
    )
  )
  
  mark_win.function <- function(first_team_goals, second_team_goals, for_team) {
    if (for_team == "FIRST_TEAM") {
      if (first_team_goals > second_team_goals) {
        return(1)  
      }
    }
    else if (for_team == "SECOND_TEAM") {
      if (second_team_goals > first_team_goals) {
        return(1)
      }
    }
    return(0)
  }
  
  team_matches_marked_win <- reactive({
    team_matches_marked_not_split <- team_matches_r() %>% mutate(home_team_wins = mapply(mark_win.function, home_team_goal, away_team_goal, "FIRST_TEAM"),
                              away_team_wins = mapply(mark_win.function, home_team_goal, away_team_goal, "SECOND_TEAM"))
    
    host <- team_matches_marked_not_split %>%
      select(home_team, home_team_wins) %>% 
      rename(team = home_team, wins = home_team_wins)
    
    away <- team_matches_marked_not_split %>%
      select(away_team, away_team_wins) %>% 
      rename(team = away_team, wins = away_team_wins)
    
    bind_rows(host, away)
  })
  
  
  first_team_wins_r <- reactive({
    team_matches_marked_win() %>%
      filter(team == input[["first_team"]]) %>% 
      summarize(wins = sum(wins)) %>%
      select(wins)
  })
  
  draws_r <- reactive({
    team_matches_r() %>% 
      filter(home_team_goal == away_team_goal) %>% 
      summarise(count = n()) %>% 
      select(count)
  })

  second_team_wins_r <- reactive({
    team_matches_marked_win() %>%
      filter(team == input[["second_team"]]) %>% 
      summarize(wins = sum(wins)) %>%
      select(wins)
  })
  
  matches_merged_r <- reactive({
    host <- matches %>%
      filter(season %in% input[["selected_seasons"]]) %>% 
      select(season, stage, date, home_team, away_team_goal) %>% 
      rename(team = home_team, goal = away_team_goal) %>% 
      mutate(as = "host")
    
    away <- matches %>%
      filter(season %in% input[["selected_seasons"]]) %>% 
      select(season, stage, date, away_team, home_team_goal) %>% 
      rename(team = away_team, goal = home_team_goal) %>% 
      mutate(as = "guest")
    
    bind_rows(host, away)
  })

  
  # original idea: https://github.com/mini-pw/WizualizacjaDanych2018/blob/master/PraceDomowe/PD5/gr1/KowalikSzymon/app.R
  results_percentage_r <- reactive({
    matches %>% 
    filter(season %in% input[["selected_seasons"]]) %>%
    mutate(home_team_status = ifelse(home_team_goal > away_team_goal, "Win", ifelse(home_team_goal == away_team_goal, "Draw", "Loss")),
           away_team_status = ifelse(away_team_goal > home_team_goal, "Win", ifelse(away_team_goal == home_team_goal, "Draw", "Loss")))
  })
  
  results_percentage_split_r <- reactive({
    rbind(select(results_percentage_r(), season, date, team_status = home_team_status, team_name = home_team),
                               select(results_percentage_r(), season, date, team_status = away_team_status, team_name = away_team))
  })

  output[["first_team_wins"]] <- renderValueBox(
    valueBox(
      first_team_wins_r(),
      paste(input[["first_team"]], "wins"), color = "green"
    )
  )
  
  output[["draws"]] <- renderValueBox(
    valueBox(
      draws_r(),
      paste("draws"), color = "orange"
    )
  )

  output[["second_team_wins"]] <- renderValueBox(
    valueBox(
      second_team_wins_r(),
      paste(input[["second_team"]], "wins"), color = "green"
    )
  )
  
  output[["average_goals_scored"]] <- renderPlot({

    host <- matches %>%
      select(season, stage, date, home_team, home_team_goal) %>% 
      rename(team = home_team, goal = home_team_goal) %>% 
      mutate(as = "host")
    
    away <- matches %>%
      select(season, stage, date, away_team, away_team_goal) %>% 
      rename(team = away_team, goal = away_team_goal) %>% 
      mutate(as = "guest")
    
    matches_merged <- bind_rows(host, away)
    
    ggplot(matches_merged_r() %>%
             filter(team == input[["first_team"]] | team == input[["second_team"]]) %>% 
             group_by(season, team) %>% 
             summarize(avg_goals = mean(goal)), 
           aes(x=team, y=avg_goals, fill=season)) +
      geom_bar(position="dodge", stat="identity") +
      labs(y="Goals per match", x="team", title = "Scored")
  })
  
  output[["average_goals_conceded"]] <- renderPlot({
    
    
    
    ggplot(matches_merged_r() %>%
             filter(team == input[["first_team"]] | team == input[["second_team"]]) %>% 
             group_by(season, team) %>% 
             summarize(avg_goals = mean(goal)), 
           aes(x=team, y=avg_goals, fill=season)) +
      geom_bar(position="dodge", stat="identity") +
      labs(y="Goals per match", x="team", title = "Conceded")
  })
  
  output[["results_percentage_first_team"]] <- renderPlot({
      ggplot(results_percentage_split_r() %>%
               filter(team_name == input[["first_team"]]) %>% 
               group_by(team_status) %>% 
               summarise(count = n()) %>% 
               ungroup(),
             aes(x = "", y = count, fill = team_status)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      labs(y = "Percent of results", fill="", title = input[["first_team"]]) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  })
  
  output[["results_percentage_second_team"]] <- renderPlot({
    ggplot(results_percentage_split_r() %>%
             filter(team_name == input[["second_team"]]) %>% 
             group_by(team_status) %>% 
             summarise(count = n()) %>% 
             ungroup(),
           aes(x = "", y = count, fill = team_status)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      labs(y = "Percent of results", fill="", title = input[["second_team"]]) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  })
  
}

shinyApp(ui, server)