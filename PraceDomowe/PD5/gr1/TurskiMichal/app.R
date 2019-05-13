library(plyr)
library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(DT)
library(waffle)
library(data.table)

labels_size <- 13

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

total_goal <- rbind(select(dat, season, team_goal = home_team_goal, 
                           team_name = home_team), 
                    select(dat, season, team_goal = away_team_goal, 
                           team_name = away_team)) %>% 
  group_by(season, team_name) %>% 
  summarise(total_goals = sum(team_goal))

points_results <- dat %>% mutate(
  home_points=ifelse(home_team_goal>away_team_goal,
                     3,
                     ifelse(home_team_goal==away_team_goal,
                            1,
                            0)
                     ),
  away_points=ifelse(home_team_goal<away_team_goal,
                     3,
                     ifelse(home_team_goal==away_team_goal,
                            1,
                            0)
  )
)
home_points <- points_results %>% group_by(season, home_team) %>% 
  summarise(points = sum(home_points)) %>% 
  select(season, team_name = home_team, points)
away_points <- points_results %>% group_by(season, away_team) %>% 
  summarise(points = sum(away_points)) %>% 
  select(season, team_name = away_team, points)
final_points = away_points
final_points["points"] = away_points["points"] + home_points["points"]

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("See also", icon = icon("send",lib='glyphicon'), 
               href = "https://www.premierleague.com/history")
    ),
    selectInput(inputId = "selected_season", label = "Select season", 
                choices = levels(dat[["season"]])),
    selectInput(inputId = "selected_team", label = "Select team", 
                choices = levels(dat[["home_team"]]))
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(box(title = "Final classification", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("total_points_plot"), width = 12)
               ),
              fluidRow(box(title = "Total points", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           infoBoxOutput("points_box"), width = 4),
                       box(title = "Average goals scored home", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           infoBoxOutput("goals_home_box"), width = 4),
                       box(title = "Average goals scored away", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           infoBoxOutput("goals_away_box"), width = 4)
                       ),
              fluidRow(box(title = "Goals scored", status = "primary",
                      solidHeader = TRUE, collapsible = TRUE, 
                      plotOutput("total_goal_plot"), width = 12)),
              fluidRow(box(title = "Points during season", status = "primary",
                      solidHeader = TRUE, collapsible = TRUE, 
                      plotOutput("season_points_plot"), width = 12)),
              fluidRow(box(title = "Team's home results", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("home_results"), width = 6),
                       box(title = "Team's away results", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("away_results"), width = 6)),
              fluidRow(box(title = "Team's results", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           dataTableOutput("matches_table", height = "100%"), width = 12)
              )
      )
    )
  )
)

server <- function(input, output) {
  team_home_matches <- reactive({
    validate(
      need(! empty(filter(dat, season == input[["selected_season"]], away_team == input[["selected_team"]])),
           "This team did not play this season")
    )
    dat %>% filter(season == input[["selected_season"]], home_team == input[["selected_team"]]) %>%
      mutate ("Result" = ifelse(home_team_goal > away_team_goal,
                                "Win",
                                ifelse(home_team_goal == away_team_goal, "Draw", "Loose"))
      )
  })
  team_home_stats <- reactive({
    validate(
      need(! empty(filter(dat, season == input[["selected_season"]], away_team == input[["selected_team"]])),
           "This team did not play this season")
    )
    as.data.frame(table(team_home_matches()["Result"])) %>% 
      mutate(Var1 = as.factor(Var1))
  })
  team_away_matches <- reactive({
    validate(
      need(! empty(filter(dat, season == input[["selected_season"]], away_team == input[["selected_team"]])),
           "This team did not play this season")
    )
    dat %>% filter(season == input[["selected_season"]], away_team == input[["selected_team"]]) %>%
      mutate (Result = ifelse(home_team_goal < away_team_goal,
                                "Win",
                                ifelse(home_team_goal == away_team_goal, "Draw", "Loose"))
      )
  })
  team_away_stats <- reactive({
    validate(
      need(! empty(filter(dat, season == input[["selected_season"]], away_team == input[["selected_team"]])),
           "This team did not play this season")
    )
    as.data.frame(table(team_away_matches()["Result"])) %>% 
      mutate(Var1 = as.factor(Var1))
  })
  team_matches <- reactive({
    points_map = c("Win" = 3, "Draw" = 1, "Loose" = 0)
    rbind(
      team_home_matches()[,c(-1, -3, -6)] %>% 
        setnames(old=c("home_team_goal", "away_team_goal", "away_team"), new=c("Team goals", "Oponent goals", "Oponent")),
      team_away_matches()[,c(-1, -3, -7)] %>% 
        setnames(old=c("away_team_goal", "home_team_goal", "home_team"), new=c("Team goals", "Oponent goals", "Oponent"))
    ) %>% setnames("stage", "Stage") %>% 
      mutate("Points" = revalue(Result, points_map))
  })
  
  output[["total_goal_plot"]] <- renderPlot({
    team_order <- filter(total_goal, season == input[["selected_season"]]) %>% 
      arrange(desc(total_goals)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order <- c(team_order,
                         setdiff(total_goal[["team_name"]], team_order))

    mutate(total_goal, 
           team_name = factor(team_name, levels = full_team_order)) %>%
      complete(season, nesting(team_name), fill = list(total_goals = 0)) %>% 
      ggplot(aes(x = team_name, y = total_goals, 
                 fill = season)) +
      geom_col(position = "dodge") +
      labs(title = "", x = "", y = "Goals scored") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = labels_size),
        axis.text.y = element_text(size=labels_size),
        axis.title = element_text(size=labels_size),
        legend.title=element_text(size=labels_size),
        legend.text = element_text(size=labels_size))
  })
  
  output[["total_points_plot"]] <- renderPlot({
    team_order <- filter(final_points, season == input[["selected_season"]]) %>% 
      arrange(desc(points)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order <- c(team_order,
                         setdiff(final_points[["team_name"]], team_order))
    
    mutate(final_points, 
           team_name = factor(team_name, levels = full_team_order)) %>%
      complete(season, nesting(team_name), fill = list(final_points = 0)) %>% 
      ggplot(aes(x = team_name, y = points, 
                 fill = season)) +
      geom_col(position = "dodge") +
      labs(title = "", x = "", y = "Points") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = labels_size),
            axis.text.y = element_text(size=labels_size),
            axis.title = element_text(size=labels_size),
            legend.title=element_text(size=labels_size),
            legend.text = element_text(size=labels_size))
  })
  
  output[["season_points_plot"]] <- renderPlot({
    team_matches()[order(team_matches()$Stage),] %>% 
      mutate(Points = cumsum(Points)) %>% 
      ggplot(aes(x = Stage, y = Points)) +
        geom_line(color="blue")+
        geom_point(color="blue") +
        theme(axis.text.x = element_text(size = labels_size),
            axis.text.y = element_text(size=labels_size),
            axis.title = element_text(size=labels_size))
    })
  
  output[["points_box"]] <- renderInfoBox({
    validate(
      need(! empty(filter(dat, season == input[["selected_season"]], away_team == input[["selected_team"]])),
           "This team did not play this season")
    )
    result <- as.data.frame(final_points) %>%
      filter(season == input[["selected_season"]]) %>%
      filter(team_name == input[["selected_team"]])
    infoBox("", result["points"]
    )
  })
  output[["goals_home_box"]] <- renderInfoBox({
    validate(
      need(! empty(filter(dat, season == input[["selected_season"]], away_team == input[["selected_team"]])),
           "This team did not play this season")
    )
    result <- dat %>%
      filter(season == input[["selected_season"]]) %>%
      filter(home_team == input[["selected_team"]])
    infoBox("", format(round(mean(data.matrix(result["home_team_goal"])), 2), nsmall = 2))
  })
  output[["goals_away_box"]] <- renderInfoBox({
    validate(
      need(! empty(filter(dat, season == input[["selected_season"]], away_team == input[["selected_team"]])),
           "This team did not play this season")
    )
    result <- dat %>%
      filter(season == input[["selected_season"]]) %>%
      filter(away_team == input[["selected_team"]])
    infoBox("", format(round(mean(data.matrix(result["away_team_goal"])), 2), nsmall = 2))
  })
  
  output[["matches_table"]] = DT::renderDataTable({
    #team_points()
    team_matches()[,-6]
  },
  rownames=FALSE,
  options = list(
    pageLength = 8,
    lengthMenu = c(8, 16, 38),
    order = list(0, "asc")
  ))
  output[["home_results"]] <- renderPlot(
    waffle(unlist(team_home_stats()["Freq"]), rows = 4) +
      scale_fill_manual(name = "Result: ",
                        labels = c(levels(team_home_stats()$Var1), ""),
                       values = c("#7B7B7B", "#cc0000", "#00cc00", "#FFFFFF"))+
      theme(legend.title=element_text(size=labels_size),
            legend.text = element_text(size=labels_size))
  )
  output[["away_results"]] <- renderPlot(
    waffle(unlist(team_away_stats()["Freq"]), rows = 4) +
      scale_fill_manual(name = "Result: ",
                        labels = c(levels(team_away_stats()$Var1), ""),
                        values = c("#7B7B7B", "#cc0000", "#00cc00", "#FFFFFF"))+
      theme(legend.title=element_text(size=labels_size),
            legend.text = element_text(size=labels_size))
  )


}

shinyApp(ui, server)
