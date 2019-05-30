library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Football App",
    dropdownMenu(
      type = "notifications",
      badgeStatus = "warning",
      notificationItem(
        icon = icon("exclamation-triangle"),
        status = "info",
        "This app is underdeveloped"
      )
    )
  ),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(
                       href = "#", icon("circle", class = "text-success"), "Online"
                     )),
    sidebarMenu(id = "tabs",
                menuItem(
                  "Dashboard",
                  tabName = "dashboard",
                  icon = icon("dashboard")
                ))
  ),
  dashboardBody(
    numericInput(inputId="n", label="n", 4),
    d3Output("graph")
  )
)

server <- function(input, output) {
  input_data <- reactive({
    data = runif(input[["n"]])
    data
  })
  
  output$graph <- renderD3({
    r2d3(data = input_data(),
         script = "r2d3-example.js")
  })
}

shinyApp(ui, server)
