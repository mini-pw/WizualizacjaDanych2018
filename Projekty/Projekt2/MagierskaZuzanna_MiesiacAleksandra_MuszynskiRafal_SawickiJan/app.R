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
            ))
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
  
}

shinyApp(ui, server)
