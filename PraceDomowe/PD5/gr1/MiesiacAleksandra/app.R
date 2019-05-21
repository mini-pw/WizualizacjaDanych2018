library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#setwd("/run/media/miesiaca/Elements SE/STUDIA/Wizualizacja/dashbord")
dat<- read.csv("PL_dat.csv", sep = ';')
MK1516<- read.csv("MiesjceVsKolejka15_16.csv")
MK1415<- read.csv("MiesjceVsKolejka14_15.csv")

dat<- dat%>%
  mutate(
    home_team_pkt = if_else(home_team_goal>away_team_goal, as.integer(3), 
                            if_else(home_team_goal==away_team_goal,as.integer(1),as.integer(0))),
    away_team_pkt = if_else(home_team_goal>away_team_goal, as.integer(0), 
                            if_else(home_team_goal==away_team_goal,as.integer(1),as.integer(3))))
dat<- dat%>%
  mutate(which_stage = if_else(stage<=19, 'First','Second'))


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
               badgeColor = "green")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              selectInput(inputId = "home_or_away", label = 'Select home or away', 
                                      choices = c('home','away')),
              selectInput(inputId = "selected_season", label = "Select season", 
                          choices = levels(dat[["season"]])),
              plotOutput('goal_plot'),
              box(
                title = "Suma goli w sezonie"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("total_goal_plot", height = "400px")
              ),
              #plotOutput("total_pkt_plot"),
              box(
                title = "Suma punktów w sezonie"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("total_pkt_plot", height = "400px")
              ),
              #plotOutput("total_goal_plot"),
              box(
                title = "Zdobyte gole w podziale na półrocza"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("half_goal_plot", height = "400px")
              ),
              #plotOutput('half_goal_plot'),
              box(
                title = "Ilość straconych goli"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("total_lost_plot", height = "400px")
              ),
              #plotOutput('total_lost_plot', height = '300px'),
              selectInput(inputId = 'team', label = 'Select team', 
                          choices = colnames(MK1516)[3:ncol(MK1516)-1]),
              #plotOutput('team_stage_plot'), 
              box(
                title = "Miejsce w zależności od kolejki"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("team_stage_plot", height = "500px")
              )
      ),
      tabItem("about",
              "About the app"
      )
    )#,
    #fluidRow(
    #  box(
    #    title = "Jakiś wykres",
    #    status = 'Primary',
    #    solidHeader = TRUE,
    #    collapsible = TRUE,
    #    plotOutput("jakiswykres", height = "300px")
    #  )
    #)
  
  )
)

server <- function(input, output) {
  
  output[["total_goal_plot"]] <- renderPlot({

    if(input[["home_or_away"]]=='home'){
      wykres2<-dat%>%filter(season == input[["selected_season"]])%>%
        group_by(home_team)%>%
        summarise(gole_wyjazd = sum(home_team_goal))%>%
        arrange(-gole_wyjazd)
     
      ggplot(wykres2, aes(x=home_team, y = gole_wyjazd, fill = gole_wyjazd))+geom_bar(stat = 'identity')+ 
        scale_x_discrete(name ="Drużyny",limits = wykres2$home_team)+ 
        ggtitle("Łączna ilość goli strzelonych u siebie")+
        theme(
          plot.title=element_text(family='', face='bold', size=15)
        ) +scale_y_continuous(name = '')+
        scale_fill_continuous(name = "Suma goli") + 
        theme(axis.text.x = element_text(angle = 55,hjust = 1, size = 13))}
    else{
      wykres1<-dat%>%filter(season == input[["selected_season"]])%>%group_by(away_team)%>%
        summarise(gole_wyjazd = sum(away_team_goal))%>%
        arrange(-gole_wyjazd)
      
      ggplot(wykres1,aes(x=away_team, y = gole_wyjazd, fill = gole_wyjazd))+
        geom_bar(stat = 'identity')+ 
        scale_x_discrete(name ="Drużyny",limits = wykres1$away_team)+ 
        ggtitle("Łączna ilość goli strzelonych na wyjeździe")+
        theme(
          plot.title=element_text(family='', face='bold', size=15)
        ) +
        scale_y_continuous(name = '')+
        scale_fill_continuous(name = "Suma goli") + 
        theme(axis.text.x = element_text(angle = 55,hjust = 1, size = 13))
    }
  })
  output[["total_lost_plot"]]<- renderPlot({
    if(input[["home_or_away"]]=='home'){
      wykres3<-dat%>%filter(season == input[["selected_season"]])%>%group_by(home_team)%>%
        summarise(gole_wyjazd = sum(away_team_goal))%>%arrange(-gole_wyjazd)
      
      ggplot(wykres3, aes(x=home_team, y = gole_wyjazd, fill = gole_wyjazd))+geom_bar(stat = 'identity')+ 
        scale_x_discrete(name ="Drużyny",limits = wykres3$home_team)+ 
        ggtitle("Łączna ilość goli straconych u siebie")+
        theme(
          plot.title=element_text(family='', face='bold', size=15)
        ) +
        scale_y_continuous(name = "") +
        scale_fill_continuous(low = "black", high = "red", name = "Suma goli") +
        theme(axis.text.x = element_text(angle = 55,hjust = 1, size = 13))}
    
    else{
      wykres4<-dat%>%filter(season == input[["selected_season"]])%>%group_by(away_team)%>%
        summarise(gole_wyjazd = sum(home_team_goal))%>%arrange(-gole_wyjazd)
      
      ggplot(wykres4, aes(x=away_team, y = gole_wyjazd, fill = gole_wyjazd))+geom_bar(stat = 'identity')+ 
        scale_x_discrete(name ="Drużyny",limits = wykres4$away_team)+ 
        ggtitle("Łączna ilość goli straconych na wyjeździe")+
        theme(
          plot.title=element_text(family='', face='bold', size=15)
        ) +
        scale_y_continuous(name = "") +
        scale_fill_continuous(low = "black", high = "red", name = "Suma goli") +
        theme(axis.text.x = element_text(angle = 55,hjust = 1, size = 13))}
    
  })
  output[["total_pkt_plot"]]<- renderPlot({
    if(input[["home_or_away"]]=='home'){
      wykres6<-dat%>%filter(season == input[["selected_season"]])%>%group_by(home_team)%>%
        summarise(pkt_wyjazd = sum(home_team_pkt))%>%arrange(-pkt_wyjazd)
      
      ggplot(wykres6, aes(x=home_team, y = pkt_wyjazd, fill = pkt_wyjazd))+geom_bar(stat = 'identity')+ 
        scale_x_discrete(name ="Drużyny",limits = wykres6$home_team)+ 
        ggtitle("Łączna ilość punktów zyskanych u siebie")+
        theme(
          plot.title=element_text(family='', face='bold', size=15)
        ) +
        scale_y_continuous(name = '') +
        scale_fill_continuous(name = "Suma punktów") + 
        theme(axis.text.x = element_text(angle = 55,hjust = 1, size = 13))
    }
    else{
      wykres5<-dat%>%filter(season == input[["selected_season"]])%>%
        group_by(away_team)%>%summarise(pkt_wyjazd = sum(away_team_pkt))%>%arrange(-pkt_wyjazd)
      
      ggplot(wykres5, aes(x=away_team, y = pkt_wyjazd, fill = pkt_wyjazd))+geom_bar(stat = 'identity')+ 
        scale_x_discrete(name ="Drużyny",limits = wykres5$away_team)+ 
        ggtitle("Łączna ilość punktów zyskanych na wyjeździe")+
        theme(
          plot.title=element_text(family='', face='bold', size=15)
        ) +
        scale_y_continuous(name = '') +
        scale_fill_continuous(name = "Suma punktów") + 
        theme(axis.text.x = element_text(angle = 55,hjust = 1, size = 13))
    }
    
  })
  output[["goal_plot"]]<- renderPlot({
    
    wykres5<-dat%>%filter(season == input[["selected_season"]])%>%group_by(away_team)%>%
      summarise(pkt_wyjazd = sum(away_team_pkt))%>%arrange(-pkt_wyjazd)
    
    wykres6<-dat%>%filter(season == input[["selected_season"]])%>%group_by(home_team)%>%
      summarise(pkt_wyjazd = sum(home_team_pkt))%>%arrange(-pkt_wyjazd)
    
    data2<- inner_join(wykres5, wykres6, by = c("away_team"="home_team"))
    data2$suma<-data2$pkt_wyjazd.x+data2$pkt_wyjazd.y
    data2<-data2%>%arrange(-suma)
    
    ggplot(data2, aes(x=away_team, y = suma, fill = suma))+geom_bar(stat = 'identity')+ 
      scale_x_discrete(name ="Drużyny",limits = data2$away_team)+ 
      ggtitle("Łączna ilość punktów")+
      theme(
        plot.title=element_text(family='', face='bold', size=15)
      ) +
      scale_y_continuous(name = "") +
      scale_fill_continuous(name = "Suma punktów") + 
      theme(axis.text.x = element_text(angle = 55,hjust = 1, size = 13))
  })
  output[["half_goal_plot"]]<-renderPlot({
    wykres5<-dat%>%filter(season == input[["selected_season"]])%>%group_by(away_team)%>%
      summarise(pkt_wyjazd = sum(away_team_pkt))%>%arrange(-pkt_wyjazd)
    
    wykres6<-dat%>%filter(season == input[["selected_season"]])%>%group_by(home_team)%>%
      summarise(pkt_wyjazd = sum(home_team_pkt))%>%arrange(-pkt_wyjazd)
    
    data2<- inner_join(wykres5, wykres6, by = c("away_team"="home_team"))
    data2$suma<-data2$pkt_wyjazd.x+data2$pkt_wyjazd.y
    data2<-data2%>%arrange(-suma)
    
    
    wykres8<-dat%>%filter(season==input[["selected_season"]])%>%
      group_by(away_team,which_stage)%>%summarise(suma_pkt_half = sum(away_team_pkt))
    pom1<-dat%>%filter(season==input[["selected_season"]])%>%
      group_by(home_team,which_stage)%>%summarise(suma_pkt_half = sum(home_team_pkt))
    wykres8$suma_pkt_half_all<-wykres8$suma_pkt_half+pom1$suma_pkt_half
    
    ggplot(wykres8, aes(fill = which_stage, y = suma_pkt_half_all, x = away_team))+
      geom_bar(position = 'dodge', stat = "identity")+ 
      scale_x_discrete(name ="Drużyny",limits = data2$away_team)+ 
      ggtitle("Ilość punktów w podziale na połowy sezonu")+
      theme(
        plot.title=element_text(family='', face='bold', size=15)
      ) +
      scale_y_continuous(name = "") +
      scale_fill_discrete(name = "Półrocze") + 
      theme(axis.text.x = element_text(angle = 45,hjust = 1))
  })
  output[["team_stage_plot"]]<-renderPlot({
    if(input[["selected_season"]]=='2014/2015'){
    ggplot(MK1415, aes(x =1:38,  y = eval(parse(text = input[["team"]])), group=1))+
        geom_line()+geom_point() +
      scale_x_discrete(name = "Kolejka",labels = as.character(1:38), breaks = 1:38)+ 
      scale_y_reverse(name = "Miejsce w tabeli",labels = as.character(1:19), breaks = 1:19)+
      ggtitle(input[["team"]])+
      theme(
        plot.title=element_text(family='', face='bold', size=15)
      )
      }
    else{
      ggplot(MK1516, aes(x =1:38,  y = eval(parse(text = input[["team"]])), group=1))+
        geom_line()+geom_point() +
        scale_x_discrete(name = "Kolejka",labels = as.character(1:38), breaks = 1:38)+ 
        scale_y_reverse(name = "Miejsce w tabeli",labels = as.character(1:19), breaks = 1:19)+
        ggtitle(input[["team"]])+
        theme(
          plot.title=element_text(family='', face='bold', size=15)
        )
    }
  })
}

shinyApp(ui, server)
