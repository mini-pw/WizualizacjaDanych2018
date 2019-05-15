library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "WiZŁAlizacja"),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    )
  )),
  dashboardBody(tabItems(
    tabItem(
      "dashboard",
      fluidRow(
        box(
          title = "Bad plot",
          status = "primary",
          solidHeader = T,
          collapsible = F,
          plotOutput("pie_plot"),
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
      )
    )
  ))
)

server <- function(input, output) {
  data_pie_plot <- reactive({
    data.frame(
      Category = c("A", "B"),
      Value = c(51, 49),
      Value_Guessed = c(as.numeric(input[["pie_plot_data"]]), 100 - as.numeric(input[["pie_plot_data"]]))
    )
  })
  
  output[["pie_plot"]] <- renderPlot({
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
      geom_text(aes(label = Value), vjust = 0) +
      ylim(c(0, 100)) +
      ggtitle("Good") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output[["pie_plot_guessed"]] <- renderPlot({
    ggplot(data_pie_plot(),
           aes(x = "", y = Value_Guessed, fill = Category)) +
      geom_bar(stat = "identity") +
      ylim(c(0, 100)) +
      coord_polar("y", start = 0) +
      ggtitle("Your guess") +
      theme_minimal()
  })
  
  output[["pie_plot_original_values"]] <- renderText({
    data_pie_plot()$Values
  })
  
  values <- reactiveValues()
  values$show_pie_plot_good <- F
  
  output$show_pie_plot_good <- reactive({
    return(values$show_pie_plot_good)
  })
  
  observeEvent(input$pie_plot_button, {
    toggle('pie_plot_values')
    output$pie_plot_original_values <-
      renderText({
        paste0(
          data_pie_plot()$Category[1],
          ": ",
          data_pie_plot()$Value[1],
          " ",
          data_pie_plot()$Category[2],
          ": ",
          data_pie_plot()$Value[2]
        )
      })
    values$show_pie_plot_good = T
  })
}

shinyApp(ui, server)
