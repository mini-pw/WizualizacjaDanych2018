library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(png)
library(grid)
library(gridExtra)
library(RCurl)
library(cowplot)
library(magick)
library(plot3D)
library(spData)
library(maptools)
library(sf)
library(cartogram)
library(RColorBrewer)
library(tmap)

us = st_transform(us_states, 2163)
us = us %>% 
  left_join(us_states_df, by = c("NAME" = "state"))
us$poverty_rate = us$poverty_level_15 / us$total_pop_15
us_carto = cartogram_cont(us, "total_pop_15", itermax=10)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "WiZŁAlizacja"),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem("Pie plot",
             tabName = "pie_plot",
             icon = icon("dashboard")),
    menuItem(
      "Bar plot (bad axis)",
      tabName = "bar_plot_bad_axis",
      icon = icon("dashboard")
    ),
    menuItem(
      "Bar plot (too much graphics)",
      tabName = "bar_plot_too_much_graphics",
      icon = icon("dashboard")
    ),
    menuItem(
      "Line plot with two y-axis",
      tabName = 'line_plot_2_y_axis',
      icon = icon("dashboard")
    ),
    menuItem(
      "Bar plot 3D",
      tabName = 'plot_3D',
      icon = icon("dashboard")
    ),
    menuItem(
      "Stacked bar plot",
      tabName = 'stacked_bar_plot',
      icon = icon("dashboard")
    ),
    menuItem(
      "Polar plot",
      tabName = 'polar_plot',
      icon = icon("dashboard")
    ),
    menuItem(
      "Cartogram",
      tabName = 'cartogram',
      icon = icon("dashboard")
    ),
    menuItem(
      "Colors",
      tabName = 'color',
      icon = icon("dashboard")
    )
  )),
  dashboardBody(tabItems(
    tabItem("pie_plot",
            fluidRow(
              box(
                title = "Bad plot",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotOutput("pie_plot_bad"),
                width = 3
              ),
              box(
                title = "Your plot",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                textInput(inputId = "pie_plot_data",
                          label = "Guess the proportion of category A (%)"),
                plotOutput("pie_plot_guessed"),
                width = 3
              ),
              box(
                title = "Good plot",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                collapsed = T,
                verbatimTextOutput("pie_plot_original_values"),
                plotOutput("pie_plot_good"),
                width = 3
              )
            )),
    tabItem("bar_plot_bad_axis",
            fluidRow(
              box(
                title = "Bad plot",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotOutput("bar_plot_bad_axis_bad"),
                width = 3
              ),
              box(
                title = "Your plot",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                textInput(
                  inputId = "bar_plot_bad_axis_data1",
                  label = "Guess min on Y scale",
                  value = "-1000"
                ),
                textInput(
                  inputId = "bar_plot_bad_axis_data2",
                  label = "Guess max on Y scale",
                  value = "1000"
                ),
                plotOutput("bar_plot_bad_axis_guessed"),
                width = 3
              ),
              box(
                title = "Good plot",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                collapsed = T,
                verbatimTextOutput("bar_plot_bad_axis_original_values"),
                plotOutput("bar_plot_bad_axis_good"),
                width = 3
              )
            )),
    tabItem("bar_plot_too_much_graphics",
            fluidRow(
              box(
                title = "Bad plot",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotOutput("bar_plot_too_much_graphics_bad"),
                width = 3
              ),
              box(
                title = "Your plot",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                textInput(inputId = "bar_plot_too_much_graphics_data",
                          label = "Guess the proportion of category A (%)"),
                plotOutput("bar_plot_too_much_graphics_guessed"),
                width = 3
              ),
              box(
                title = "Good plot",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                collapsed = T,
                verbatimTextOutput("bar_plot_too_much_graphics_original_values"),
                plotOutput("bar_plot_too_much_graphics_good"),
                width = 3
              )
            )),
    tabItem("line_plot_2_y_axis",
            fluidRow(
              box(
                title = "Bad plot",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotOutput("line_plot_2_y_axis_bad"),
                width = 5
              ),
              box(
                title = "Your plot",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                selectInput(inputId = "line_plot_2_y_axis_data",
                          label = "Which line shows Divorce rate?", 
                          choices = c('Wybierz odpowiedź','Blue','Red')),
                plotOutput("line_plot_answer"),
                width = 2
              ),
              box(
                title = "Good plot",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                collapsed = T,
                verbatimTextOutput("line_plot_2_y_axis_original_values"),
                plotOutput("line_plot_2_y_axis_good"),
                width = 5
              )
            )),
    tabItem("plot_3D",
            fluidRow(
              box(
                title = "Bad plot",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotOutput("bar_plot_3D_bad"),
                width = 5
              ),
              box(
                title = "Answer",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                textInput(inputId = "bar_plot_3D_data",
                          label = "Guess value Drama movies in USA"),
                plotOutput("3D_plot_answer"),
                width = 3
              ),
              box(
                title = "Good plot",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                collapsed = T,
                verbatimTextOutput("bar_plot_3D_values"),
                plotOutput("bar_plot_3D_good"),
                width = 4
              )
            )),
    tabItem('stacked_bar_plot',
            fluidRow(
              box(
                title = "Bad plot",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotOutput("stacked_bar_plot_bad"),
                width = 5
              ),
              box(
                title = "Answer",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                selectInput(inputId = "stacked_bar_plot_input",
                            label = "When is the value of group B the highest?",
                            choices = c("day", "Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
                verbatimTextOutput("stacked_bar_plot_ans"),
                width = 3
              )
            ),
          fluidRow(
            box(
              title = "Good plot",
              status = "primary",
              solidHeader = T,
              collapsible = T,
              collapsed = T,
              plotOutput("stacked_bar_plot_good"),
              width = 5
            ))
          ),
    tabItem('polar_plot',
            fluidRow(
              box(
                title = "Bad plot",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotOutput("polar_plot_bad"),
                width = 5
              ),
              box(
                title = "Answer",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                selectInput(inputId = "polar_plot_input",
                            label = "When is the ratio of F to C?",
                            choices = c("ratio", 1, 2, 3, 4, 5, 6)),
                verbatimTextOutput("polar_plot_ans"),
                width = 3
              )
            ),
            fluidRow(
              box(
                title = "Good plot",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                collapsed = T,
                plotOutput("polar_plot_good"),
                width = 5
              ))
    ),
    tabItem('cartogram',
            fluidRow(
              box(
                title = "Bad plot",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotOutput("cartogram_bad"),
                width = 5
              ),
              box(
                title = "Answer",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                selectInput(inputId = "cartogram_input",
                            label = "What is the poverty rate in Colorado?",
                            choices = c("rate", "0.08 to 0.10", "0.10 to 0.12", "0.12 to 0.14", "0.14 to 0.16", "0.16 to 0.18", "0.18 to 0.20", "0.20 to 0.22")),
                verbatimTextOutput("cartogram_ans"),
                width = 3
              )
            ),
            fluidRow(
              box(
                title = "Good plot",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                collapsed = T,
                plotOutput("cartogram_good"),
                width = 5
              ))
    ),
    tabItem('color',
            fluidRow(
              box(
                title = "Bad plot",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotOutput("color_bad"),
                width = 5
              ),
              box(
                title = "Answer",
                status = "primary",
                solidHeader = TRUE,
                collapsible = F,
                selectInput(inputId = "color_input",
                            label = "What was the population in Congo in 2005?",
                            choices = c("population", "0 mln to 15 mln", "15 mln to 30 mln", "30 mln to 45 mln", "45 mln to 60 mln", "60 mln to 75 mln", "75 mln to 90 mln")),
                verbatimTextOutput("color_ans"),
                width = 3
              )
            ),
            fluidRow(
              box(
                title = "Good plot",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                collapsed = T,
                plotOutput("color_good"),
                width = 5
              ))
    )
            
  ))
)

server <- function(input, output) {
  # PIE PLOT
  # ------------------------------------------------------------------------------------------------------------------------------------------
  data_pie_plot <- reactive({
    data.frame(
      Category = c("A", "B"),
      Value = c(51, 49),
      Value_Guessed = c(as.numeric(input[["pie_plot_data"]]), 100 - as.numeric(input[["pie_plot_data"]]))
    )
  })
  
  output[["pie_plot_bad"]] <- renderPlot({
    ggplot(data_pie_plot(), aes(x = "", y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      ylim(c(0, 100)) +
      coord_polar("y", start = 0) +
      ggtitle("Original") +
      theme_minimal()
  })
  
  output[["pie_plot_good"]] <- renderPlot({
    ggplot(data_pie_plot(), aes(x = Category, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Value), vjust = -0.5) +
      ylim(c(0, 100)) +
      ggtitle("Good") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output[["pie_plot_guessed"]] <- renderPlot({
    ggplot(data_pie_plot(),
           aes(
             x = "",
             y = data_pie_plot()$Value_Guessed,
             fill = Category
           )) +
      geom_bar(stat = "identity") +
      ylim(c(0, 100)) +
      labs(x = "x", y = "Value") +
      coord_polar("y", start = 0) +
      ggtitle("Your guess") +
      theme_minimal()
  })
  
  # BAR PLOT WITH BAD AXIS
  # ------------------------------------------------------------------------------------------------------------------------------------------
  
  data_bar_plot_bad_axis <- reactive({
    data.frame(
      Category = c("A", "B"),
      Value = c(75, 76),
      Value_Guessed = c(as.numeric(input[["bar_plot_bad_axis_data1"]]), as.numeric(input[["bar_plot_bad_axis_data2"]]))
    )
  })
  
  output[["bar_plot_bad_axis_bad"]] <- renderPlot({
    ggplot(data_bar_plot_bad_axis(),
           aes(x = Category, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_cartesian(ylim = c(74, 77))  +
      ggtitle("Original") +
      theme_minimal() +
      theme(axis.text.y = element_blank())
  })
  
  output[["bar_plot_bad_axis_good"]] <- renderPlot({
    ggplot(data_bar_plot_bad_axis(),
           aes(x = Category, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Value), vjust = -0.5) +
      ylim(c(0, 100)) +
      ggtitle("Good") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output[["bar_plot_bad_axis_guessed"]] <- renderPlot({
    ggplot(data_bar_plot_bad_axis(),
           aes(x = Category, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_cartesian(
        ylim = c(
          data_bar_plot_bad_axis()$Value_Guessed[1],
          data_bar_plot_bad_axis()$Value_Guessed[2]
        )
      ) +
      ggtitle("Your guess") +
      theme_minimal()
  })
  
  # BAR PLOT WITH TOO MUCH GRAPHICS
  # ------------------------------------------------------------------------------------------------------------------------------------------
  
  dogeImage = "http://www.stickpng.com/assets/images/5845e69dfb0b0755fa99d7ef.png"
  
  data_bar_plot_too_much_graphics <- reactive({
    data.frame(
      Category = c("A", "B"),
      Value = c(23, 77),
      Value_Guessed = c(as.numeric(input[["bar_plot_too_much_graphics_data"]]), 100 - as.numeric(input[["bar_plot_too_much_graphics_data"]]))
    )
  })
  
  output[["bar_plot_too_much_graphics_bad"]] <- renderPlot({
    ggdraw() +
      draw_plot(
        ggplot(
          data_bar_plot_too_much_graphics(),
          aes(x = Category, y = Value, fill = Category)
        ) +
          geom_bar(stat = "identity") +
          ggtitle("Original") +
          theme_minimal()
      ) +
      draw_image(dogeImage)
  })
  
  output[["bar_plot_too_much_graphics_good"]] <- renderPlot({
    ggplot(data_bar_plot_too_much_graphics(),
           aes(x = Category, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Value), vjust = -0.5) +
      ylim(c(0, 100)) +
      ggtitle("Good") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output[["bar_plot_too_much_graphics_guessed"]] <- renderPlot({
    ggdraw() +
      draw_plot(
        ggplot(
          data_bar_plot_too_much_graphics(),
          aes(x = Category,
              y = Value_Guessed,
              fill = Category)
        ) +
          geom_bar(stat = "identity") +
          ggtitle("Your guess") +
          theme_minimal()
      ) +
      draw_image(dogeImage)
  })
  
  # LINE PLOT WITH TWO Y-AXIS 
  # ------------------------------------------------------------------------------------------------------------------------------------------
  
  okImage <- "http://www.stickpng.com/assets/images/580b585b2edbce24c47b29e1.png"
  notokImage <- "http://www.stickpng.com/assets/images/5897a8c3cba9841eabab6156.png"
  
  
  data_line_plot_2_y_axis <- reactive({
    data.frame(cbind(category = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009),
                   Value = c(5,4.7,4.6,4.4,4.3,4.1,4.2,4.2,4.2,4.1),
                   Value2 = c(8.2,7,6.5,5.3,5.2,4,4.6,4.5,4.2,3.7))
    )
  })
  output[["line_plot_2_y_axis_bad"]]<-renderPlot({
    ggplot()+geom_line(aes(x = data_line_plot_2_y_axis()$category, y = data_line_plot_2_y_axis()$Value), size= 2, color = "red")+
      geom_point(aes(x = data_line_plot_2_y_axis()$category, y = data_line_plot_2_y_axis()$Value), size= 5, color = "red")+
      geom_line(aes(x = data_line_plot_2_y_axis()$category, y = data_line_plot_2_y_axis()$Value2/5+3.3), size = 2, color = "blue")+
      geom_point(aes(x = data_line_plot_2_y_axis()$category, y = data_line_plot_2_y_axis()$Value2/5+3.3), size = 5, color = "blue")+  
      scale_x_continuous(name = "Year", breaks = round(seq(2000, 2009, by = 1),1))+
      scale_y_continuous(name = "Divorce rate in Marine",sec.axis = sec_axis(~.*5-16.5,name = "Per capita consuption of margarine in US"))
    
  })
  output[["line_plot_answer"]]<-renderPlot({
    if(input[["line_plot_2_y_axis_data"]]=='Blue'){
      
      ggdraw()+draw_image(notokImage)
    }
    else{if(input[["line_plot_2_y_axis_data"]]=='Red'){
      ggdraw()+draw_image(okImage) 
    }
      
    }
  })
  
  
  output[["line_plot_2_y_axis_good"]]<-renderPlot({
   plot1<- ggplot()+
      geom_line(aes(x = data_line_plot_2_y_axis()$category, y = data_line_plot_2_y_axis()$Value2), size = 2, color = "blue")+
      geom_point(aes(x = data_line_plot_2_y_axis()$category, y = data_line_plot_2_y_axis()$Value2), size = 5, color = "blue")+  
      scale_x_continuous(name = "Year", breaks = round(seq(2000, 2009, by = 1),1))+
      scale_y_continuous(name = "Consuption of margarine")
    
    plot2<-ggplot()+geom_line(aes(x = data_line_plot_2_y_axis()$category, y = data_line_plot_2_y_axis()$Value), size= 2, color = "red")+
      geom_point(aes(x = data_line_plot_2_y_axis()$category, y = data_line_plot_2_y_axis()$Value), size= 5, color = "red")+
      scale_x_continuous(name = "Year", breaks = round(seq(2000, 2009, by = 1),1))+
      scale_y_continuous(name = "Divorce rate in Marine")
    grid.arrange(plot1, plot2, nrow=2)
  })
  # BAR PLOT 3D 
  # ------------------------------------------------------------------------------------------------------------------------------------------
  
  movies<- rbind(values = c(4,5,6,4,1),values2 = c(7,5,6,1,4))
  
  output[["bar_plot_3D_bad"]]<-renderPlot({
    hist3D (x = 1:2, y = 1:5, z = movies,
            bty = "g", phi = 20,  theta = -60,
            xlab='',ylab='',zlab='',
            col = "#0072B2", border = "black", shade = 0.8,
            ticktype = "detailed", space = 0.15, d = 1, cex.axis = 1e-9)
    
    text3D(x = 1:2, y = rep(0.5, 2), z = rep(0, 2),
           labels = c("Poland","USA"),
           add = TRUE, adj = 0)
   
    text3D(x = rep(0.65, 5),   y = 1:5, z = rep(0, 5),
           labels  = c("Comedy","Action","Romance","Drama","SciFi"),
           add = TRUE, adj = 1)
    
  })

  output[["3D_plot_answer"]]<-renderPlot({
      if(input[["bar_plot_3D_data"]]==1){
        
        ggdraw()+draw_image(okImage)
      }
      else{
        ggdraw()+draw_image(notokImage)
      }
  })
  typ<-c(rep("Comedy",2),rep("Action",2),rep("Romance",2),rep("Drama",2),rep("SciFi",2))
  condition<- rep(c("Poland","USA"),5)
  value <- c(4,7,5,5,6,6,4,1,1,4)
  data<-data.frame(typ,condition,value)
  
  output[["bar_plot_3D_good"]]<-renderPlot({
    
    ggplot(data,aes( fill = condition, y = value, x = typ))+
      geom_bar(position = "dodge",stat = "identity")+
      labs( fill = "Country")+
      scale_x_discrete(name='')+
      scale_y_continuous(name = '')
    })
  
  # STACKED BAR PLOT 
  # ------------------------------------------------------------------------------------------------------------------------------------------
  
  stacked_days <- c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  stacked_day <- unlist(lapply(stacked_days, function(x){rep(x , 4)}))
  
  # day=c(rep("Monday" , 4) , rep("Tuesday" , 4) , rep("Wednesday" , 4) , rep("Thursday" , 4), rep("Friday", 4),rep("Saturday" , 4),rep("Sunday" , 4) )
  stacked_group=rep(c("A" , "B" , "C", "D") , 7)
  stacked_value=c( 14, 2, 6, 2, 
                   15, 4, 1, 3,
                   13, 8, 8, 8,
                   16, 4, 20, 19,
                   14, 7 ,16, 13,
                   15, 6, 2, 1,
                   13, 3, 1, 12)
  stacked_data=data.frame(stacked_day, stacked_group, stacked_value)
  
  stacked_base <- ggplot(stacked_data, aes(fill=stacked_group, y=stacked_value, x=factor(stacked_day, levels = stacked_days)) ) +
    xlab('day') +
    ylab('value') +
    labs(fill = 'group')
  
  output[['stacked_bar_plot_bad']] <- renderPlot({
    # Stacked
    stacked_base + geom_bar( stat="identity")
  })
  
  output[['stacked_bar_plot_good']] <- renderPlot({
    # Grouped
    stacked_base + geom_bar(position="dodge", stat="identity")
  })
  
  output[['stacked_bar_plot_ans']] <- renderText({
    ifelse(input[['stacked_bar_plot_input']] == 'day', 'answer..', 
           ifelse(input[['stacked_bar_plot_input']] == 'Wednesday', 
                  "Good answer!",
                  "Bad, correct answer is Wednesday"))
  })
  
  # POLAR PLOT 
  # ------------------------------------------------------------------------------------------------------------------------------------------
  
  polar_values <- c(4,5,5,6,8,10)
  polar_labels <- c('A', 'B', 'C', 'D', 'E', 'F')
  
  polar_df <- data.frame(polar_values, polar_labels)
  
  polar_base_plot <- ggplot(polar_df, aes(x = factor(polar_labels), y = polar_values, fill = polar_labels)) +
    geom_bar(stat = 'identity', width = 1) +
    xlab('label') +
    ylab('value') +
    labs(fill = 'labels')
  
  
  
  output[['polar_plot_bad']] <- renderPlot({
    polar_base_plot +
      coord_polar() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      theme(axis.title.x=element_blank(),
            axis.ticks.x=element_blank())
      
  })
  
  output[['polar_plot_good']] <- renderPlot({
    polar_base_plot
  })
  
  output[['polar_plot_ans']] <- renderText({
   ifelse(input[['polar_plot_input']] == 'ratio', 'answer..', 
           ifelse(input[['polar_plot_input']] == "2", 
                  "Good answer!",
                  "Bad, correct answer is 2"))
  })
  
  
  # CARTOGRAM
  # ------------------------------------------------------------------------------------------------------------------------------------------
  
  
  output[['cartogram_bad']] <- renderPlot({
    tm_shape(us_carto) + tm_polygons("poverty_rate", title = "Poverty rate") + tm_layout(frame = F, legend.outside = T)
  })
  
  output[['cartogram_good']] <- renderPlot({
    tm_shape(us) + tm_polygons("poverty_rate", title = "Poverty rate") + tm_layout(frame = F, legend.outside = T)
  })
  
  output[['cartogram_ans']] <- renderText({
    ifelse(input[['cartogram_input']] == 'rate', 'answer..', 
           ifelse(input[['cartogram_input']] == "0.12 to 0.14", 
                  "Good answer!",
                  "Bad, correct answer is 0.12 to 0.14"))
  })
  
  # COLOR
  # ------------------------------------------------------------------------------------------------------------------------------------------
  
  data(wrld_simpl)
  afr=wrld_simpl[wrld_simpl$REGION==2,]
  
  bad_palette <- c("#feebe2", "#fa9fb5")
  good_palette <- brewer.pal(6, "RdPu")
  
  output[['color_bad']] <- renderPlot({
    tm_shape(afr) + 
      tm_polygons("POP2005", title = "Population in 2005",
                  breaks = c(0, 15000000, 30000000, 45000000, 60000000, 75000000, 90000000),
                  palette = bad_palette) +
      tm_layout(frame = F, legend.outside = T)
    })
  
  output[['color_good']] <- renderPlot({
    tm_shape(afr) + 
      tm_polygons("POP2005", title = "Population in 2005",
                  breaks = c(0, 15000000, 30000000, 45000000, 60000000, 75000000, 90000000),
                  palette = good_palette) +
      tm_layout(frame = F, legend.outside = T)
    })
  
  output[['color_ans']] <- renderText({
    ifelse(input[['color_input']] == 'population', 'answer..', 
           ifelse(input[['color_input']] == "45 mln to 60 mln", 
                  "Good answer!",
                  "Bad, correct answer is 45 mln to 60 mln"))
  })
}

shinyApp(ui, server)
