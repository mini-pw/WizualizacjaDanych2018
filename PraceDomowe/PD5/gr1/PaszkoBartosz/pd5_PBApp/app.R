library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(scales)


dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

total_goal <- rbind(select(dat, season, team_goal = home_team_goal, 
                           team_name = home_team), 
                    select(dat, season, team_goal = away_team_goal, 
                           team_name = away_team)) %>% 
  group_by(season, team_name) %>% 
  summarise(total_goals = sum(team_goal)) 


scored_lost <- rbind(select(dat, season, date, scored_goals = home_team_goal,  
                     team_name = home_team, lost_goals = away_team_goal), 
              select(dat, season, date, scored_goals = away_team_goal, 
                     team_name = away_team, lost_goals = home_team_goal)) %>% 
              mutate(result = factor(ifelse(scored_goals > lost_goals, 'win', ifelse(scored_goals < lost_goals, 'lost', 'draw')), 
                                        levels = c('win', 'lost', 'draw'))) %>% 
              mutate(points = ifelse(result == 'win', 3, ifelse(result=='loss', 0, 1)),
                     date = as.Date(date))


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
            selectInput(inputId = "selected_season", label = "Wybierz sezon", 
                        choices = levels(dat[["season"]])),
            plotOutput("total_goal_plot"),
            width = NULL, title = 'Zdobyte gole', solidHeader = TRUE, status = "primary"
          )
        ),
        fluidRow(
          box(plotOutput("total_win_plot"), width = NULL, title = 'Wyniki meczów', solidHeader = TRUE, status = "primary")
        ),
        fluidRow(
          box(plotOutput("total_points_plot"), width = NULL, title = 'Zdobyte punkty', solidHeader = TRUE, status = "primary")
        ),
        fluidRow(
          column(width=6,
            box(
              selectInput(inputId = "selected_team", label = "Wybierz drużynę", 
                          choices = levels(dat[["home_team"]])),
              dataTableOutput("team_last_results"), width = NULL, title = 'Ostatnie mecze', solidHeader = TRUE, status = "primary"
            )
          ),
          column(width=6,
            box(plotOutput('points_plot'), width = NULL, title = 'Zdobyte punkty', solidHeader = TRUE, status = "primary"),
            column(width=6,
              box(plotOutput('wins_plot'), width = NULL, title = "Rezultaty", solidHeader = TRUE, status = "primary")
            ),
            column(width=6,
              infoBoxOutput("win_box", width=NULL),
              infoBoxOutput("lost_box", width=NULL),
              infoBoxOutput("draw_box", width=NULL),
              infoBoxOutput("points_box", width=NULL)
            )
          )
        )
      ),
      tabItem("about",
              "About the app"
      )
      
    )
  )
)

server <- function(input, output) {
  output[['win_box']] <- custom_render_results(input, 'Wygrane', 'win')
  output[['lost_box']] <- custom_render_results(input, 'Przegrane', 'lost')
  output[['draw_box']] <- custom_render_results(input, 'Remisy', 'draw')
  output[['points_box']] <- custom_render_results(input, 'Punkty', 'draw')
  
  output[['wins_plot']] <- renderPlot({
    data_tmp <- scored_lost %>%
      filter(season == input[['selected_season']], team_name == input[['selected_team']])
    
    if (nrow(data_tmp) != 0) {
      p <- data_tmp %>% 
        group_by(result) %>% 
        summarise(freq = n()) %>% 
        mutate(perc = round(freq/sum(freq)*100, 2)) %>% 
        mutate(lab.ypos = cumsum(perc) - 0.5*perc) %>% 
        ggplot(aes(x = 2, y = perc, fill = result)) +
          geom_bar(stat = 'identity') +
          coord_polar(theta = "y", start=0) + 
          geom_text(aes(y = 100-lab.ypos, label = perc), color = "white") +
          scale_fill_discrete(name = 'Rezultat', labels = c("Wygrana", "Przegrana", "Remis")) +
          theme_void() +
          xlim(0.5, 2.5)
    }
    else {
      p <- ggplot(data_tmp)
    }
    p
  })
  
  output[['team_last_results']] <- renderDataTable({
    dat %>% 
      mutate(date = as.Date(date), score = paste0(home_team_goal, ' : ', away_team_goal)) %>% 
      select(date, season, home_team, score, away_team) %>% 
      filter(home_team == input[['selected_team']] | away_team == input[['selected_team']]) %>% 
      filter(season == input[['selected_season']]) %>% 
      arrange(desc(date))
       
  }, options = list(pageLength=10))
  
  output[["total_goal_plot"]] <- renderPlot({
    team_order <- filter(total_goal, season == input[["selected_season"]]) %>% 
      arrange(desc(total_goals)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order <- c(team_order,
                         setdiff(total_goal[["team_name"]], team_order))

    mutate(total_goal, team_name = factor(team_name, levels = full_team_order)) %>%
      complete(season, nesting(team_name), fill = list(total_goals = 0)) %>% 
      ggplot(aes(x = team_name, y = total_goals, fill = season)) +
        geom_col(position = "dodge") +
        xlab('') + ylab('') +
        theme(axis.text.x = element_text(angle = 20, hjust=1)) +
        labs(fill = "Sezon")
  })
  
  output[["total_points_plot"]] <- renderPlot({
    team_order <- scored_lost %>%
      filter(season == input[["selected_season"]]) %>%
      group_by(team_name) %>% 
      summarise(total_points = sum(points)) %>% 
      arrange(desc(total_points)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order <- c(team_order,
                         setdiff(scored_lost[["team_name"]], team_order))
    
    scored_lost %>% 
      group_by(season, team_name) %>% 
      summarise(total_points = sum(points)) %>% 
      mutate(team_name = factor(team_name, levels = full_team_order)) %>%
      complete(season, nesting(team_name), fill = list(total_points = 0)) %>% 
      ggplot(aes(x = team_name, y = total_points, fill = season)) +
        geom_col(position = "dodge") +
        xlab('') + ylab('') +
        theme(axis.text.x = element_text(angle = 20, hjust=1)) +
        labs(fill = "Sezon")
  })
 
  output[['total_win_plot']] <- renderPlot({
    scored_lost %>% 
      filter(season == input[['selected_season']]) %>% 
      group_by(team_name, result) %>% 
      summarise(total_result = n()) %>%
      ggplot(aes(x=team_name, y=total_result, fill=result)) + 
        geom_col(position='dodge') +
        scale_fill_discrete(name = 'Rezultat', labels = c("Wygrana", "Przegrana", "Remis")) +
        xlab('') + ylab('') + 
        theme(axis.text.x = element_text(angle = 20, hjust=1))
  })
  
  output[['points_plot']] <- renderPlot({
    dat_tmp <- scored_lost %>%
      filter(season == input[['selected_season']], team_name == input[['selected_team']]) %>%
      arrange(date) %>% 
      mutate(cupoints = cumsum(points))
    
    p <- ggplot(dat_tmp)
    if (nrow(dat_tmp) != 0){
      p <- p + 
        geom_line(aes(x = date, y = cupoints, group = 1)) +
        scale_x_date(date_labels="%b %y", date_breaks="1 month") +
        xlab('Data') + ylab('Liczba punktów')
    }
    p
    
  })
   
}

custom_render_results <- function(input, res_title, res) {
  k <- renderInfoBox({
    n_res <- scored_lost %>% 
      filter(season == input[['selected_season']], team_name == input[['selected_team']], result==res) %>% 
      count()
    
    infoBox(
      res_title, n_res, icon = icon("list"),
      color = "purple", fill=TRUE
    )
  })
  return(k)
}


shinyApp(ui, server)
