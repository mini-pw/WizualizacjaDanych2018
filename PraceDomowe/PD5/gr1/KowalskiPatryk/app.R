library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

total_goal <- rbind(select(dat, season, team_goal = home_team_goal, 
                           team_name = home_team), 
                    select(dat, season, team_goal = away_team_goal, 
                           team_name = away_team)) %>% 
  group_by(season, team_name) %>% 
  summarise(total_goals = sum(team_goal)) 

total_goal_w_stage <- rbind(select(dat, stage, team_goal = home_team_goal, 
                           team_name = home_team), 
                    select(dat, stage, team_goal = away_team_goal, 
                           team_name = away_team)) %>% 
  group_by(stage, team_name) %>% 
  summarise(total_goals = sum(team_goal)) 

total_goal_w_date <- rbind(select(dat, season, team_goal = home_team_goal, 
                           team_name = home_team, date), 
                    select(dat, season, team_goal = away_team_goal, 
                           team_name = away_team, date)) %>% 
  group_by(date, team_name) %>% 
  summarise(total_goals = sum(team_goal)) 


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App",
                  
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("exclamation-triangle"), status = "info",
                                                "This app is underdeveloped"
                               )
                  )),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              selectInput(inputId = "selected_season", label = "Select season", 
                          choices = levels(dat[["season"]])),
              plotOutput("total_goal_plot"),
              box(title = "wybierz drużyny których mecz zamierzasz obstawiac",
                  splitLayout(
                selectInput(width = "100%", inputId = "selected_team_1", label = "team 1", choices = levels(total_goal[["team_name"]])),
                selectInput(width = "100%", inputId = "selected_team_2", label = "team 2", choices = levels(total_goal[["team_name"]]), selected = 'Crystal Palace')
                  )
              ),
              plotOutput("czy_grali"),
              plotOutput("total_goal_w_stage_plot"),
              plotOutput("hist_po_dacie"),
              plotOutput("strzelili_vs_strzelono")

      )
    )
  )
)

server <- function(input, output) {
  
  selected_teams <- reactiveValues(
    selected = character()
  )
  
  observeEvent(input[["selected_team_1"]], {
    selected_teams[['selected']][1] <- input[['selected_team_1']]
  })
         
  observeEvent(input[["selected_team_2"]], {
    selected_teams[['selected']][2] <- input[['selected_team_2']]
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
      xlab('drużyny') +
      ylab("zdobyte w sezonie gole")
  })
  
  output[["total_goal_w_stage_plot"]] <- renderPlot({
    total_goal_w_stage %>%
      filter(team_name %in% c(input[["selected_team_1"]], input[["selected_team_2"]])) %>%
      ggplot() +
      aes(x = stage, y = total_goals) +
      geom_col() +
      geom_smooth() +
      facet_wrap(~team_name) +
      ggtitle('suma goli w zależności od stagea')
  })
  
  output[["czy_grali"]] <- renderPlot({
    validate(need(selected_teams[["selected"]][1] != selected_teams[["selected"]][2],
                  'druzyny nie moga byc takie same'))
    
    t1.points <- 0
    t2.points <- 1
    dat2 <- dat %>%
      filter(home_team %in% c(selected_teams[["selected"]][1], selected_teams[["selected"]][2]) &
               away_team %in% c(selected_teams[["selected"]][1], selected_teams[["selected"]][2]))
    
    total_goal_versus <- rbind(select(dat2, season, team_goal = home_team_goal, 
                               team_name = home_team), 
                        select(dat2, season, team_goal = away_team_goal, 
                               team_name = away_team)) %>% 
    group_by(team_name) %>% 
    summarise(total_goals = sum(team_goal)) 
    
    ggplot(total_goal_versus) +
      aes(x = team_name, y = total_goals) +
      geom_col() +
      ggtitle('suma goli strzelonych sobie wzajemnie')
  })
  
  output[["hist_po_dacie"]] <- renderPlot({
    total_goal_w_date$date <- as.Date(total_goal_w_date$date)
    total_goal_w_date$date <- as.Date(cut(total_goal_w_date$date, breaks = "month"))
    total_goal_w_date2 <- total_goal_w_date %>%
      filter(team_name %in% c(selected_teams[["selected"]][1], selected_teams[["selected"]][2]))
    
    ggplot(data = total_goal_w_date2,
      aes(date, total_goals)) +
      stat_summary(fun.y = sum, geom = "bar") + 
      facet_wrap(~team_name) +
      scale_x_date(
        labels = date_format("%Y.%m"),
        breaks = "3 month") +
        ggtitle('suma goli w czasie')

  })
  
  output[["strzelili_vs_strzelono"]] <- renderPlot({

    dat3 <- dat %>%
      filter(home_team %in% c(selected_teams[["selected"]][1], selected_teams[["selected"]][2]) |
               away_team %in% c(selected_teams[["selected"]][1], selected_teams[["selected"]][2]))
    
    total_goal_2 <- total_goal %>%
      filter(team_name %in% c(selected_teams[["selected"]][1], selected_teams[["selected"]][2]))
    
    strzelono_team1 <- dat3 %>%
      filter(home_team == selected_teams[["selected"]][1]) %>%
      pull(away_team_goal) + 
      dat3 %>%
      filter(away_team == selected_teams[["selected"]][1]) %>%
      pull(home_team_goal)
    
    strzelono_team1 <- strzelono_team1 %>% sum()

    strzelono_team2 <- dat3 %>%
      filter(home_team == selected_teams[["selected"]][2]) %>%
      pull(away_team_goal) + 
      dat3 %>%
      filter(away_team == selected_teams[["selected"]][2]) %>%
      pull(home_team_goal)
    
    strzelono_team2 <- strzelono_team2 %>% sum()
    
    
    strzelono.df <- tibble(team_name = c(selected_teams[["selected"]][1], selected_teams[["selected"]][2]),
                           stracone_gole = c(strzelono_team1, strzelono_team2))
    names(total_goal_2)[3] = 'strzelone gole'
    
    zestawienie <- inner_join(total_goal_2, strzelono.df, by = 'team_name') %>% 
      melt(id = c('season', 'team_name'))
    
    ggplot(zestawienie) +
      aes(x = team_name, y = value, fill = variable) +
      facet_wrap(~season) +
      geom_col() +
      ggtitle('strzelone gole vs stracone gole')
    
  })
    
  
}

shinyApp(ui, server)
