library(shiny)
library(shinydashboard)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)

source("data_processing.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App"
                  
                  ),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    selectInput(
      "chosen_season",
      "Select season",
      choices = sort(unique(dat$season))),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("See also", icon = icon("send",lib='glyphicon'), 
               href = "https://github.com/mini-pw/WizualizacjaDanych2018/")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard", 
              
              fluidRow(box(title = "Teams overview", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE,
                           box(title="Comparison of results achieved in different seasons",
                               selectInput(inputId = "chosen_season1",
                                           label = "Choose season to sort data by",
                                           choices = levels(win_perc_dat[["season"]])),
                               plotOutput("win_rate_plot")),
                           box(title="Final classification (particular season)",plotOutput("final classification"),
                               height=540), width=12)
                       ),
              fluidRow(box(title = "About your favourite team - charts (particular season)", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE,
                           selectInput(inputId = "chosen_team",
                                       label = "Choose your team",
                                       choices = levels(win_perc_dat[["team"]])),
                           checkboxInput(inputId = "decreasing_checkbox", label = "Decreasing?", value = TRUE),
                           plotOutput("points_against_plot"),
                           column(6,plotOutput("results_at_home")),
                           column(6,title='match results away',plotOutput("results_away")),
                           column(12,plotOutput("diff_goals")),
                          column(12,plotOutput("progress")),width= 12)),#, height=1800)),

              fluidRow( box(title='About your favourite team - statistics (particular season)',
                            solidHeader = TRUE, collapsible = TRUE,
                            status='primary',width=12,
                box(title = "Average goals scored away", status = "primary",
                               solidHeader = TRUE, collapsible = TRUE,
                               infoBoxOutput("goals_scored_away"), width = 3 ),
                           box(title = "Average goals scored at home", status = "primary",
                               solidHeader = TRUE, collapsible = TRUE,infoBoxOutput("goals_scored_at_home"),width=3),
                           box(title = "Average goals lost away", status = "primary",
                               solidHeader = TRUE, collapsible = TRUE,
                               infoBoxOutput("goals_lost_away"), width=3),
                           box(title = "Average goals lost at home", status = "primary",
                               solidHeader = TRUE, collapsible = TRUE,
                               infoBoxOutput("goals_lost_at_home"), width=3))
                           ),

              fluidRow(box(title='Matches with particular opponent',status = "primary",
                           solidHeader = TRUE, collapsible = TRUE,
                           selectInput(inputId = "chosen_opponent",
                                       label = "Choose an opponent",
                                       choices = levels(win_perc_dat[["team"]]),
                                                        selected=c('Liverpool')),
                           tableOutput("previous_matches"), width = 12) )
               )
           )
       )
    )

server <- function(input, output) {
  
  points_against_dat <- reactive({

    rbind(filter(point_dat, home_team == input[["chosen_team"]]) %>%
            select(opponent = away_team, points = home_team_points, season),
          filter(point_dat, away_team == input[["chosen_team"]]) %>%
            select(opponent = home_team, points = away_team_points, season)) %>% 
      filter(season==input[['chosen_season']]) %>% 
      group_by(opponent) %>%
      summarise(mean_points = mean(points))
  })

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
    
    dane<- mutate(points_against_dat(), opponent = factor(opponent, levels = team_order)) 
    validate(
      need(!empty(dane),
           "This team did not play this season")
    )
    
   
      ggplot(dane, aes(x = opponent, y = mean_points)) +
      geom_col() +
      scale_y_continuous(limits = c(0, 3))+
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5))+
      ggtitle('Average number of points gained against the opponent')+
      theme(plot.title = element_text(lineheight=.8, face="bold", size=16))+
      scale_y_continuous(name="Average number of points")
  })

  output[["win_rate_plot"]] <- renderPlot({
    team_order <- filter(win_perc_dat, season == input[["chosen_season1"]]) %>%
      arrange(desc(win_perc)) %>%
      pull(team) %>%
      as.character()


    dane<-mutate(win_perc_dat, team = factor(team, levels = team_order))
      
      validate(
        need(!empty(dane),
             "This team did not play this season")
      )
      ggplot(dane,aes(x = team, y = win_perc, fill = season)) +
      geom_col(position = "dodge") +scale_fill_manual(values = c ('grey45', 'cornflowerblue'))+
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5))+
      ggtitle('Rate of matches won')+
      theme(plot.title = element_text(lineheight=.8, face="bold", size=18))+
      scale_y_continuous(name="win rate")
  })




  output[["final classification"]] <- renderPlot({
    final_class <- final %>%  group_by(season) %>% filter(season == input[["chosen_season"]])
     final_class_order<- final_class %>% arrange(desc(points)) %>%
      pull(team) %>%
      as.character()



    dane<-mutate(final_class, team = factor(team, levels = final_class_order)) 
      
      validate(
        need(!empty(dane),
             "This team did not play this season")
      )
      ggplot(dane, aes(x = team, y = points)) +
      geom_col(position = "dodge") +scale_fill_manual(values = c ('grey45', 'cornflowerblue'))+
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5))+
      ggtitle('Sum of points')+
      theme(plot.title = element_text(lineheight=.8, face="bold", size=16))+
        geom_text(aes(label = points), vjust = -0.5)

  })

  output[["results_at_home"]] <- renderPlot({

    season_home<-filter(match_result_at_home, season==input[['chosen_season']])
    to_melt_home<-select(season_home, home_team, win_times,draw_times, lost_times)
    res_home <- melt(to_melt_home, id.var="home_team")
    team_order <- filter(res_home, home_team==input[["chosen_team"]])
    team_order<-team_order %>% arrange(desc(variable)) %>% mutate( label_position = cumsum(value) - 0.5*value)

    validate(
      need(!empty(team_order),
           "This team did not play this season")
    )
      ggplot(team_order, aes(x =2, y= value, fill=variable)) +geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 0)+
        geom_text(aes(y = label_position, label = value),size = 9, color = "white")+
        theme_void()+ xlim(0.5, 2.5)+
        ggtitle('Match results at home')+theme(plot.title = element_text(lineheight=.8, face="bold", size=16))+
        scale_fill_manual(values = c ('green3','grey45', 'red3'),name='Number of ',labels=c("win","draw","loss"))+
        theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size=14))

  })



  output[["results_away"]] <- renderPlot({

    season_away<-filter(match_result_away, season==input[['chosen_season']])
    to_melt_away<-select(season_away, away_team, win_times,draw_times, lost_times)
    res_away <- melt(to_melt_away, id.var="away_team")
    team_order <- filter(res_away, away_team==input[["chosen_team"]])
    team_order<-team_order %>% arrange(desc(variable)) %>% mutate( label_position = cumsum(value) - 0.5*value)

    validate(
      need(!empty(team_order),
           "This team did not play this season")
    )
    ggplot(team_order, aes(x =2, y= value, fill=variable)) +geom_bar(stat = "identity", color = "white") +
      coord_polar(theta = "y", start = 0)+
      geom_text(aes(y = label_position, label = value),size = 9, color = "white")+
      theme_void()+
      xlim(0.5, 2.5)+ggtitle('Match results away')+theme(plot.title = element_text(lineheight=.8, face="bold", size=16))+
      scale_fill_manual(values = c ('green3','grey45', 'red3'),name='Number of ',labels=c("win","draw","loss") )+
      theme(legend.title = element_text(size=16, face="bold"), legend.text=element_text(size=14))

  })

  output[['diff_goals']]<-renderPlot({

    dg<- filter(diff_goals, team1==input[["chosen_team"]] | team2==input[["chosen_team"]],
                season==input[["chosen_season"]])%>%
      mutate(goals_gained=ifelse(team1==input[["chosen_team"]], goals1,goals2),
             goals_lost =ifelse(team1!=input[["chosen_team"]], goals1,goals2))%>%
    mutate(diff=goals_gained - goals_lost) %>% mutate(opponent=ifelse(team1!=input[["chosen_team"]],
                                                                      as.character(team1), as.character(team2))) %>%
    group_by(opponent) %>%  mutate (avg_diff=mean(diff)) %>% select(team1, team2, season, avg_diff, opponent)
    dg$label<- ifelse(dg$avg_diff>0, 'win', 'lost')

    validate(
      need(!empty(dg),
           "This team did not play this season")
    )
    ggplot(dg, aes(x=opponent, y=avg_diff, fill=label))+geom_bar(stat='identity')+
      ggtitle('Difference between number of goals gained and lost')+
      theme(plot.title = element_text(lineheight=.8, face="bold", size=16))+
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5))+
      scale_fill_manual(values = c ('green3', 'red3'))+theme(legend.position="none")+
      scale_y_continuous(name="Gain of goals")

  })


  output[["progress"]] <- renderPlot({
    dg<- filter(diff_goals, team1==input[['chosen_team']] | team2==input[['chosen_team']] ) %>%
      mutate(goals_gained=ifelse(team1==input[['chosen_team']] , goals1,goals2),
             goals_lost =ifelse(team1!=input[['chosen_team']] , goals1,goals2),
             opponent=ifelse(team1!=input[['chosen_team']] , as.character(team1), as.character(team2)))%>%
      mutate(diff=goals_gained - goals_lost)

    time_series<- dg%>% filter(season==input[['chosen_season']])%>%
      mutate(points=ifelse(diff>0, 3, ifelse(diff==0,1,0))) %>% mutate(cumulated=cumsum(points))
    time_series<-arrange(time_series, date)
    progress<-time_series %>% mutate(points_total=cumsum(points))



    validate(
      need(!empty(progress),
           "This team did not play this season")
    )


    ggplot(progress, aes(x =stage, y= points_total, group=1)) +geom_line(color='grey25', size=2)+geom_point(color="cornflowerblue", size=4)+
       ggtitle('Some of points after each stage')+theme(plot.title = element_text(lineheight=.8, face="bold", size=16))+
      scale_y_continuous(name="Sum of points")

  })


  output[["goals_scored_at_home"]] <- renderInfoBox({
    result <- filter(diff_goals, team1==input[["chosen_team"]] | team2==input[["chosen_team"]],
                     season==input[["chosen_season"]])%>%
      mutate(goals_gained=ifelse(team1==input[["chosen_team"]], goals1,goals2),
             goals_lost =ifelse(team1!=input[["chosen_team"]], goals1,goals2))
    result_at_home<- filter(diff_goals, team1==input[['chosen_team']]) %>%
      mutate(goals_gained=goals1, goals_lost=goals2)
    avg_gained_at_home<-summarise(result_at_home,mean(goals_gained))

    validate(
      need(!empty(result),
           "This team did not play this season")
    )
    infoBox("", round(avg_gained_at_home,2))

  })
  output[["goals_scored_away"]] <- renderInfoBox({
    result <- filter(diff_goals, team1==input[["chosen_team"]] | team2==input[["chosen_team"]],
                     season==input[["chosen_season"]])%>%
      mutate(goals_gained=ifelse(team1==input[["chosen_team"]], goals1,goals2),
             goals_lost =ifelse(team1!=input[["chosen_team"]], goals1,goals2))
    result_away<- filter(diff_goals, team2==input[['chosen_team']]) %>%
      mutate(goals_gained=goals2, goals_lost=goals1)
    avg_gained_away<-summarise(result_away,mean(goals_gained))
    
    validate(
      need(!empty(result),
           "This team did not play this season")
    )
    infoBox("", round(avg_gained_away,2))

})
  output[["goals_lost_away"]] <- renderInfoBox({
    result <- filter(diff_goals, team1==input[["chosen_team"]] | team2==input[["chosen_team"]],
                     season==input[["chosen_season"]])%>%
      mutate(goals_gained=ifelse(team1==input[["chosen_team"]], goals1,goals2),
             goals_lost =ifelse(team1!=input[["chosen_team"]], goals1,goals2))
    result_away<- filter(diff_goals, team2==input[['chosen_team']]) %>%
      mutate(goals_gained=goals2, goals_lost=goals1)
    
    avg_lost_away<-summarise(result_away,mean(goals_lost))
    validate(
      need(!empty(result),
           "This team did not play this season")
    )
    infoBox("", round(avg_lost_away,2))

  })
  output[["goals_lost_at_home"]] <- renderInfoBox({
    result <- filter(diff_goals, team1==input[["chosen_team"]] | team2==input[["chosen_team"]],
                     season==input[["chosen_season"]])%>%
      mutate(goals_gained=ifelse(team1==input[["chosen_team"]], goals1,goals2),
             goals_lost =ifelse(team1!=input[["chosen_team"]], goals1,goals2))
    result_at_home<- filter(diff_goals, team1==input[['chosen_team']]) %>%
      mutate(goals_gained=goals1, goals_lost=goals2)
    avg_lost_at_home<-summarise(result_at_home,mean(goals_lost))
    
    validate(
      need(!empty(result),
           "This team did not play this season")
    )
    infoBox("", round(avg_lost_at_home,2))

  })

  output[["previous_matches"]]<- renderTable({
    dane<-filter(point_dat,
                 home_team == input[['chosen_team']] & away_team==input[['chosen_opponent']]
                 | away_team==input[['chosen_team']] & home_team==input[["chosen_opponent"]]) %>%
      select(season, stage, home_team, home_team_goal, away_team_goal, away_team)
    dane
  })
}

shinyApp(ui, server)