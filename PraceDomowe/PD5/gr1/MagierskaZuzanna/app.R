library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggiraph)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Statistics", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Teams", tabName = "teams", icon = icon("users")),
      conditionalPanel("true",
                      selectInput(inputId = "selected_season", label = "Select season", 
                                  choices = levels(dat[["season"]])
                      )
      ),
      conditionalPanel("input.tabs === 'teams'",
                     selectInput(inputId = "selected_team", label = "Select team", 
                                 choices = levels(dat[["home_team"]])
                     )
      ),
      conditionalPanel("input.tabs === 'teams'",
                       selectInput(inputId = "selected_oponent", label = "Select oponent", 
                                   choices = levels(dat[["home_team"]])
                       )
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                column(6, 
                 box(title = "Statistics", status = "primary",
                     solidHeader = TRUE, collapsible = TRUE, 
                     plotOutput("stats_plot", height = "800px"), width = "100%"
                 )
                ),
                column(6, 
                  box(title = "Goals for teams", status = "primary",
                      solidHeader = TRUE, collapsible = TRUE, 
                      plotOutput("goals_plot"), width = "100%"
                  ),
                  box(title = "Total goals", status = "primary",
                      solidHeader = TRUE, collapsible = TRUE, 
                      ggiraphOutput("goals_pie", height = "300px")
                  )
                )
              )
      ),
      tabItem("teams",
              box(title = textOutput("team_goals_plot_title"), status = "primary",
                  solidHeader = TRUE, 
                  plotOutput("team_goals_plot")
              ),
              box(title = textOutput("teams_comparison_title"), status = "primary",
                  solidHeader = TRUE, 
                  plotOutput("teams_comparison")
              ),
              fluidRow(
                infoBoxOutput("home_result_box")
              ),
              fluidRow(
                infoBoxOutput("away_result_box")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_dat <- reactive({
    
    droplevels(filter(dat, season == input[["selected_season"]]))
  })
  
  observe({
    df <- filtered_dat()
    print(df)
    choices <- levels(df[["home_team"]])
    selected_team <- ifelse(input[["selected_team"]] %in% choices, input[["selected_team"]], head(choices, 1))
    
    updateSelectInput(session, "selected_team",
                      label = "Select team",
                      choices = choices,
                      selected = selected_team
    )
    
    choices_oponent <- choices[choices != selected_team]
    selected_oponent <- ifelse(input[["selected_oponent"]] %in% choices_oponent, input[["selected_oponent"]], head(choices_oponent, 1))
    
    updateSelectInput(session, "selected_oponent",
                      label = "Select oponent",
                      choices = choices_oponent,
                      selected = selected_oponent
    )
  })
  
  output[["team_goals_plot_title"]] <- renderText({paste0("Goals for ", input[["selected_team"]])})
  output[["teams_comparison_title"]] <- renderText({paste0(input[["selected_team"]], " vs. ", input[["selected_oponent"]])})
  
  output[["goals_plot"]] <- renderPlot({
    df <- filtered_dat()
    home <- df %>% select(scored = home_team_goal, lost = away_team_goal, team = home_team)
    away <- df %>% select(scored = away_team_goal, lost = home_team_goal, team = away_team)
    df <- rbind(home, away)
    df <- aggregate(cbind(scored=df$scored, lost=-df$lost), by=list(team=df$team), FUN=sum) %>% 
      mutate(scoredDiff = ifelse(scored + lost > 0, scored + lost, 0),
             lostDiff = ifelse(scored + lost < 0, scored + lost, 0),
             scored = scored - scoredDiff,
             lost = lost - lostDiff)
    team_order <- df %>% 
      arrange(scoredDiff, lostDiff) %>% 
      pull(team) %>% 
      as.character()
    df <- melt(df, id.vars = c('team'))
    df <- df %>% mutate(team = factor(team, levels = team_order))    
    ggplot(df, aes(x = team, y = value, fill = variable)) + 
      geom_col() +
      coord_flip() +
      scale_fill_manual(breaks = c("scored", "lost"),
                        labels = c("Scored", "Lost"),
                        values=c("#23882380", "#D2222D80", "#238823", "#D2222D")) +
      labs(fill="Types", x ="Teams", y = "Goals")  
  })
  
  output[["goals_pie"]] <- renderggiraph({
    df <- filtered_dat()
    home <- sum(df$home_team_goal)
    away <- sum(df$away_team_goal)
    sum <- home + away
    df <- data.frame(class = c("home", "away"), goals = c(home * 100 / sum, away * 100 / sum))
    
    donut_plot <- ggplot(df, aes(y = goals, fill = class)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = paste0(round(goals), "% of ", class, " goals")),
        width = 0.1,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = paste0(sum, " goals"),
        size = 18,
        color = "black"
      ) +
      scale_fill_manual(values = c("#FFBF00", "#007000")) +
      coord_polar(theta = "y") +
      theme_void()
    
    ggiraph(ggobj = donut_plot)
  })
  
  output[["stats_plot"]] <- renderPlot({
    df <- filtered_dat()
    df <- df %>% 
      mutate(home_team_result = ifelse(df$home_team_goal > df$away_team_goal, 'Success', ifelse(df$home_team_goal < df$away_team_goal, 'Loss', 'Draw')),
             away_team_result = ifelse(home_team_result == 'Success', 'Loss', ifelse(home_team_result == 'Loss', 'Success', 'Draw'))) %>% 
      select(home_team_result, away_team_result, home_team, away_team)
    
    home <- df %>% select(result = home_team_result, team = home_team)
    away <- df %>% select(result = away_team_result, team = away_team)
    df <- rbind(home, away)
    df <- df %>% group_by(team) %>% count(result)
    
    ggplot(df, aes(x = team, y = n, fill = result)) + 
      geom_col(position = "dodge") +
      coord_flip() +
      scale_fill_manual(values=c("#FFBF00", "#D2222D", "#238823")) +
      labs(fill="Types", x ="Teams", y = "Results")
  })
  
  output[["team_goals_plot"]] <- renderPlot({
    df <- filtered_dat()
    home <- df %>% filter(home_team == input[["selected_team"]]) %>% mutate(oponent = away_team, scored = home_team_goal, lost = away_team_goal)
    away <- df %>% filter(away_team == input[["selected_team"]]) %>% mutate(oponent = home_team, scored = away_team_goal, lost = home_team_goal)
    df <- rbind(home, away) %>% select(stage, oponent, scored, lost)
    df <- df %>% 
      mutate(scoredDiff = ifelse(scored - lost > 0, scored - lost, 0),
             lostDiff = ifelse(scored - lost < 0, scored - lost, 0),
             scored = scored - scoredDiff,
             lost = -lost - lostDiff)
    df <- melt(df, id.vars = c('stage', 'oponent'))
    ggplot(df, aes(x = factor(stage), y = value, fill = variable)) + 
      geom_col() +
      scale_fill_manual(breaks = c("scored", "lost"),
                        labels = c("Scored", "Lost"),
                        values=c("#23882380", "#D2222D80", "#238823", "#D2222D")) +
      scale_x_discrete() +
      scale_y_continuous(breaks = seq(min(df$value), max(df$value), 1)) +
      labs(fill="Types", x ="Stages", y = "Goals") 
  })
  
  output[["teams_comparison"]] <- renderPlot({
    df <- filtered_dat()
    team1 <- input[["selected_team"]]
    team2 <- input[["selected_oponent"]]
    home <- df %>% 
      filter((home_team == team1 | home_team == team2) & away_team != team1 & away_team != team2) %>% 
      mutate(team = home_team, oponent = away_team, diff = home_team_goal - away_team_goal, loc = "home")
    away <- df %>% 
      filter((away_team == team1 | away_team == team2) & home_team != team1 & home_team != team2) %>% 
      mutate(team = away_team, oponent = home_team, diff = away_team_goal - home_team_goal, loc = "away")
    df <- rbind(home, away) %>% select(team, stage, oponent, diff, loc)
    df <- aggregate(list(diff=df$diff), by=list(team=df$team, oponent=df$oponent), FUN=sum)
    
    ggplot(df, aes(x = oponent, y = diff, fill = team)) + 
      geom_col(position="dodge") +
      coord_flip() +
      scale_fill_manual(values=c("#FFBF00", "#238823")) +
      scale_y_continuous(breaks = seq(min(df$diff), max(df$diff), 1)) +
      labs(fill="Teams", x ="Stages", y = "Goal difference") 
  })
  
  output[["home_result_box"]] <- renderInfoBox({
    df <- filtered_dat()
    team1 <- input[["selected_team"]]
    team2 <- input[["selected_oponent"]]
    result <- df %>% filter(home_team == team1 & away_team == team2)
    result1 <- result[1, "home_team_goal"]
    result2 <- result[1, "away_team_goal"]
    validate(
      need(!is.na(result1) & !is.na(result2), "")
    )
    infoBox(
      "Home result", paste(team1, result1, ":", result2, team2), icon = icon("home"),
      color = ifelse(result1 > result2, "green", ifelse(result1 == result2, "yellow", "red")), fill = TRUE
    ) 
  })
  
  output[["away_result_box"]] <- renderInfoBox({
    df <- filtered_dat()
    team1 <- input[["selected_team"]]
    team2 <- input[["selected_oponent"]]
    result <- df %>% filter(home_team == team2 & away_team == team1)
    result1 <- result[1, "away_team_goal"]
    result2 <- result[1, "home_team_goal"]
    validate(
      need(!is.na(result1) & !is.na(result2), "")
    )
    infoBox(
      "Away result", paste(team1, result1, ":", result2, team2), icon = icon("bus-alt"),
      color = ifelse(result1 > result2, "green", ifelse(result1 == result2, "yellow", "red")), fill = TRUE
    ) 
  })
}

shinyApp(ui, server)
