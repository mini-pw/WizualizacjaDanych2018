library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(formattable)

source("data-processing.R")

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
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("See also", icon = icon("send",lib='glyphicon'), 
               href = "https://github.com/mini-pw/WizualizacjaDanych2018/")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
              box(
                title = 'Choose season',
                solidHeader = TRUE,
                background = 'teal',
                selectInput(inputId = "chosen_season", label = "", choices = levels(win_perc_dat[["season"]]), selected="2015/2016")
                 )
              ),
              fluidRow(
                box(
                  title = 'Summary of home/away goals differences',
                  solidHeader = TRUE,
                  formattableOutput("summary_table"))
             
              ),
              fluidRow(
                box(
                  title = 'Wins/Draws/Losses',
                  solidHeader = TRUE,
                  plotOutput("summary_match_plot")
                ),
                box(
                  title = 'Win rate plot',
                  solidHeader = TRUE,
                  plotOutput("win_rate_plot")
                )
              ),
              fluidRow(
                box(
                  title = 'Choose team ',
                  solidHeader = TRUE,
                  background = 'teal',
                  selectInput(inputId = "chosen_team", label = "", choices = levels(win_perc_dat[["team"]]), selected="Arsenal")
                ),
                box(
                  title = 'Choose rival ',
                  solidHeader = TRUE,
                  background = 'teal',
                  selectInput(inputId = "chosen_rival", label = "", choices = levels(win_perc_dat[["team"]]), selected="Chelsea")
                )
              ),
              fluidRow(box(
                           background = 'teal',
                           valueBoxOutput('home_team', width = 7)
                           ),
                       box(
                         background = 'teal',
                         valueBoxOutput('departure_team', width = 7)
                       )),
              fluidRow(box(
                title = 'Chosen team vs rivals',
                solidHeader = TRUE,
                plotOutput("points_against_plot")
                
              )
              )),
      tabItem("about",
              "About the app",
              includeMarkdown("example.md"))
      )
    )
  )


server <- function(input, output) {
  
  
  output[['home_team']] <- renderValueBox({
    result_home_team <- point_dat %>% filter(home_team == input[["chosen_team"]] & away_team == input[["chosen_rival"]] & season == input[["chosen_season"]]) %>% pull(as.integer(home_team_goal))
    lose_home_team <-  point_dat %>% filter(home_team == input[["chosen_team"]] & away_team == input[["chosen_rival"]] & season == input[["chosen_season"]])  %>%  pull(as.integer(away_team_goal))
    
    color <- if(result_home_team>lose_home_team){
      "green"
    } else if (result_home_team==lose_home_team){
      "blue"
    } else {"red"}
    
    result <- paste0(result_home_team, " : ",lose_home_team)
      
    valueBox(subtitle ="Result at home", icon = icon("home"), value =result, color=color)
     
  })
  output[['departure_team']] <- renderValueBox({
    
    result_dep_team <- point_dat %>% filter(away_team == input[["chosen_team"]] & home_team == input[["chosen_rival"]] & season == input[["chosen_season"]]) %>% pull(as.integer(away_team_goal))
    lose_dep_team <-  point_dat %>% filter(away_team == input[["chosen_team"]] & home_team == input[["chosen_rival"]] & season == input[["chosen_season"]]) %>%  pull(as.integer(home_team_goal))
    
    color <- if(result_dep_team>lose_dep_team){
      "green"
    } else if (result_dep_team==lose_dep_team){
      "blue"
    } else {"red"}
    
    result <- paste0(result_dep_team, " : ",lose_dep_team)
    
    valueBox(subtitle ="Result on departure", icon = icon("bus-alt"), value = result, color = color)
    
  })
  
  points_against_dat <- reactive({
  
    rbind(filter(point_dat, home_team == input[["chosen_team"]]) %>% 
            select(opponent = away_team, points = home_team_points),
          filter(point_dat, away_team == input[["chosen_team"]]) %>% 
            select(opponent = home_team, points = away_team_points)) %>% 
      group_by(opponent) %>% 
      summarise(mean_points = mean(points))
  })
  
  
win_perc_r <- reactive({
  
  filter(win_perc_dat,  season==input[["chosen_season"]] ) %>%
    filter(!is.na(win_perc))
 
})

  output[["win_rate_plot"]] <- renderPlot({
    
    team_order2015 <- win_perc_r() %>% 
      arrange(identity(win_perc)) %>% 
      pull(team) %>% 
      as.character()
    
    mutate(win_perc_r(), team = factor(team, levels = team_order2015)) %>% 
      ggplot(aes(x = team, y = win_perc, fill = season)) +
      geom_col(position = "dodge") +
      labs(x="", y="", fill="")+  
      theme(legend.position = "none")+
      scale_fill_manual(values="#BCA18D")+
      theme_minimal() +
      coord_flip()
      
  })
 
  output[['summary_table']] <-renderFormattable({
    
    formattable( match_dat_all %>%
                  filter(season == input[['chosen_season']]) %>%
                  select(-season) %>%
                  arrange(desc(points)),
                          list(
                            'home goals difference' = color_tile("#D50B53", "#118C8B"),
                            'away goals difference' = color_tile("#D50B53", "#118C8B"),
                            area(col = points) ~ normalize_bar("#BCA18D", 0.2)))
    
  })
  
  summary_match_plot_r <- reactive({
    
   filter(match_dat_melt, season == input[["chosen_season"]])
  })
  
  output[['summary_match_plot']] <- renderPlot({
    
    season_fun <- if(input[["chosen_season"]]=="2015/2016") {
      "2015/2016"
    } else {
      "2014/2015"
    }
    
    
    team_order <- filter(match_dat_melt, season == season_fun & variable=="wins") %>% 
      arrange(desc(1/value)) %>% 
      pull(team) %>% 
      as.character()
    
  mutate(summary_match_plot_r(), team = factor(team, levels=team_order)) %>%
   ggplot( aes(x = team, y = value, fill=variable)) +
    geom_bar(stat='identity') + 
    theme_minimal() +
    labs(x="", y="", fill="")+
    scale_fill_manual(values=c("#118C8B","#BCA18D", "#D50B53"))+
    scale_y_continuous(trans = "reverse")+
    coord_flip() 
  }
  )
  
  points_against_dat <- reactive({
    
    
    # | away_team == input[["chosen_team"]]
    rbind(filter(point_dat, home_team == input[["chosen_team"]]) %>% 
            select(opponent = away_team, points = home_team_points),
          filter(point_dat, away_team == input[["chosen_team"]]) %>% 
            select(opponent = home_team, points = away_team_points)) %>% 
      group_by(opponent) %>% 
      summarise(mean_points = mean(points))
  })
  
  output[["points_against_plot"]] <- renderPlot({
   
    team_order <- points_against_dat() %>% 
      arrange(desc(mean_points)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(points_against_dat(), opponent = factor(opponent, levels = team_order)) %>% 
      ggplot(aes(x = opponent, y = mean_points)) +
      geom_col(fill = "#BCA18D") +
      labs(x= "Rivals", y="Mean Points") +
      theme_minimal()+
      scale_y_continuous(limits = c(0, 3)) +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) 
  })
}

shinyApp(ui, server)