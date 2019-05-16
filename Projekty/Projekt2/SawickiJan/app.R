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
    menuItem("Pie plot",
             tabName = "pie_plot",
             icon = icon("dashboard")),
    menuItem(
      "Bar plot (bad axis)",
      tabName = "bar_plot_bad_axis",
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
                textInput(inputId = "bar_plot_bad_axis_data1",
                          label = "Guess the value of category A", value = "100"),
                textInput(inputId = "bar_plot_bad_axis_data2",
                          label = "Guess the value of category B", value = "50"),
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
           aes(x = "", y = Value_Guessed, fill = Category)) +
      geom_bar(stat = "identity") +
      coord_cartesian(ylim = c(data_bar_plot_bad_axis()$Value_Guessed[0], data_bar_plot_bad_axis()$Value_Guessed[1])) +
      ggtitle("Your guess") +
      theme_minimal()
  })
}

shinyApp(ui, server)
