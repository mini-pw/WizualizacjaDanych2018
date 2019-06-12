library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(patchwork)

source("BasicInfo.R")
source("bitwa_dane.R" )

modes <- c('Rzeczpospolita Obojga Narodów', 'Imperium Osmanskie', 'Zestawienie')

general_info_modes <- c("Populacja", "Liczebność wojsk")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Bitwa pod Chocimiem"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Strona glowna", tabName = "mainPage", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("mainPage",
              selectInput(inputId = "which_country",
                          label = "Wybierz tryb:", 
                          choices = modes),
              box(
                title="Informacje ogolne",
                status="primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width="100%",
                selectInput(inputId = "general_information_type",
                            label = "Wybierz rodzaj",
                            choices = general_info_modes),
                plotOutput(outputId = "general_information_plot", height = "500px")
              ),
              box(
                title="Siły przed bitwą",
                status="warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                width="100%",
                plotOutput(outputId = "forces_before_plot", height = "500px")
              ),
              box(
                title="Straty",
                status="danger",
                solidHeader = TRUE,
                collapsible = TRUE,
                width="100%",
                plotOutput(outputId = "loss_plot",  height = "500px")
              )
      )
    )
  )
)

country_population_plots <- list(population_poland_plot, population_turkey_plot, combined_population)
country_general_forces_plots <- list(poland_force_categories_general, turkey_force_categories_general, combined_infantry + combined_cavalry)
forces_plots <- list(wojska_rp_plot, wojska_io_plot, forces_both_plot + liczba_dzial_zestawienie_wykres)
loss_plots <- list(loss_rp_plot, loss_io_plot, loss_both_plot)

server <- function(input, output) {
  
  mode_index_r <- reactive({
    match(input[["which_country"]], modes)
  })

  output[["general_information_plot"]] <- renderPlot({
    if(input[["general_information_type"]] == general_info_modes[1]) { #populacja
      return(country_population_plots[[mode_index_r()]])
    } else {
      return(country_general_forces_plots[[mode_index_r()]])
    }
  })
  
  output[["forces_before_plot"]] <- renderPlot({
    forces_plots[[mode_index_r()]]
  })
  
  output[["loss_plot"]] <- renderPlot({
    loss_plots[[mode_index_r()]]
  })
  
}

shinyApp(ui, server)
