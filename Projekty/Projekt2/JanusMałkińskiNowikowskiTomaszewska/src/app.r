library(shiny)
library(shinydashboard)
library(shinyjs)
library(reshape2)
library(dplyr)
library(ggplot2)
library(SmarterPoland)
library(latticeExtra)
library(plotrix)
library(tidyr)
library(ggsci)
library(gridExtra)

source('ui/pie_chart_1_ui.r')
source('ui/pie_chart_2_ui.r')
source('ui/skipping_data.r')
source('ui/zus_ofe.r')
source('ui/misleading_graphs_1_ui.r')
source('ui/misleading_graphs_2_ui.r')
source('ui/wrong_1_ui.r')
source('ui/wrong_2_ui.r')

source('data/pie_chart_1.r')
source('data/pie_chart_2.r')
source('data/skipping_data.r')
source('data/zus_ofe.r')
source('data/misleading_graphs_1.R')
source('data/misleading_graphs_2.R')
source('data/wrong_1.R')
source('data/wrong_2.R')

ui <- dashboardPage(
    skin = 'black',
    dashboardHeader(
        title = 'WD Project 2'
    ),
    dashboardSidebar(
        sidebarMenu(id='tabs',
            menuItem('Pie chart 1', tabName = 'pie_chart_1', icon = icon('chart-pie')),
            menuItem('Pie chart 2', tabName = 'pie_chart_2', icon = icon('chart-pie')),
            menuItem('Skipping data', tabName = 'skipping_data', icon = icon('chart-pie')),
            menuItem('ZUS vs OFE', tabName = 'zus_ofe', icon = icon('chart-pie')),
            menuItem('Baseline', tabName = 'mislead_graph_1', icon = icon('chart-pie')),
            menuItem('Axis', tabName = 'mislead_graph_2', icon = icon('chart-pie')),
            menuItem('3D Barplot', tabName = 'wrong_1', icon = icon('chart-pie')),
            menuItem('Polar plot', tabName = 'wrong_2', icon = icon('chart-pie'))
        )
    ),
    dashboardBody(
        shinyjs:::useShinyjs(),
        tabItems(
            tab_item_pie_chart_1,
            tab_item_pie_chart_2,
            tab_item_skipping_data,
            tab_item_zus_ofe,
            tabItem(tabName = "mislead_graph_1", baselineUI("mislead_graph_1")),
            tabItem(tabName = "mislead_graph_2", axisUI("mislead_graph_2")),
            tabItem(tabName = "wrong_1", dimensionsUI("wrong_1")),
            tabItem(tabName = "wrong_2", polarUI("wrong_2"))
        )
    )
)

server <- function(input, output, session) {

    output[['bad_pie_chart_1']] <- renderPlot({
        data(countries)
        population_by_continent <- countries %>%
            select(population, continent) %>%
            group_by(continent) %>%
            summarise(population = round(sum(population) / sum(countries$population) * 100)) %>%
            arrange(desc(population)) %>%
            mutate(continent = factor(continent, levels = continent))

        pie3D(population_by_continent$population, labels = population_by_continent$continent, main = "Population percentage by continent",
            explode = 0.1, radius = .9, labelcex = 1.2, height = 0.2, shade=0.5, start = 0, mar=c(0,0,0,6),
            col=pal_jco()(5))

    })
    output[['good_pie_chart_1']] <- renderPlot({ good_pie_chart_1_plot })
    output[['bad_pie_chart_2']] <- renderPlot({ bad_pie_chart_2_plot })
    output[['good_pie_chart_2']] <- renderPlot({ good_pie_chart_2_plot })
    output[['skipping_data_increasing']] <- renderPlot({ skipping_data_increasing_plot })
    output[['skipping_data_decreasing']] <- renderPlot({ skipping_data_decreasing_plot })
    output[['skipping_data_full']] <- renderPlot({ skipping_data_full_plot })
    output[['skipping_data_user']] <- renderPlot({ get_skipping_data_user_plot(input) })
    output[['zus_ofe_orig']] <- renderPlot({ zus_ofe_orig_plot(input) })
    output[['zus_ofe_real_ratio']] <- renderValueBox({ get_zus_ofe_real_ratio(input) })
    output[['zus_ofe_current_ratio']] <- renderValueBox({ get_zus_ofe_current_ratio(input) })
    output[['zus_ofe_ratio']] <- renderText({ zus_ofe_ratio })
    output[['zus_ofe_good']] <- renderPlot({ get_zus_ofe_good_plot(input) })
    

    observeEvent(input$tabs,{
      if(input$tabs=="mislead_graph_1"){
        callModule(baseline_module, "mislead_graph_1")
      }
      if(input$tabs=="mislead_graph_2"){
        callModule(yaxis_module, "mislead_graph_2")
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)


    observeEvent(input$tabs,{
      if(input$tabs=="wrong_1"){
        callModule(dim_module, "wrong_1")
      }
      if(input$tabs=="wrong_2"){
        callModule(polar_module, "wrong_2")
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    callModule(baseline_module,'baseline_module')

    observeEvent(input$action_button_pie_chart_1, {
        if(input$action_button_pie_chart_1 %% 2 == 0) {
            shinyjs::hide(id = 'box_pie_chart_1_good_viz')
            shinyjs::hide(id = 'box_pie_chart_1_actual_values')
        } else {
            shinyjs::show(id = 'box_pie_chart_1_good_viz')
            shinyjs::show(id = 'box_pie_chart_1_actual_values')
        }
    })

    observeEvent(input$zus_ofe_action_button, {
      if(input$zus_ofe_action_button %% 2 == 0) {
        shinyjs::hide(id = 'zus_ofe_parameters')
      } else {
        shinyjs::show(id = 'zus_ofe_parameters')
      }
    })

    shinyjs::hide(id = 'zus_ofe_parameters')

    observeEvent(input$action_button_pie_chart_2, {
        if(input$action_button_pie_chart_2 %% 2 == 0) {
            shinyjs::hide(id = 'box_pie_chart_2_good_viz')
            shinyjs::hide(id = 'box_pie_chart_2_actual_values_1')
            shinyjs::hide(id = 'box_pie_chart_2_actual_values_2')
        } else {
            shinyjs::show(id = 'box_pie_chart_2_good_viz')
            shinyjs::show(id = 'box_pie_chart_2_actual_values_1')
            shinyjs::show(id = 'box_pie_chart_2_actual_values_2')
        }
    })
    observeEvent(input$skipping_data_try_it, {
      if (input$skipping_data_try_it %% 2 == 0) {
        shinyjs::hide(id = 'box_skipping_data_input')
        shinyjs::hide(id = 'box_skipping_data_user')
      } else {
        shinyjs::show(id = 'box_skipping_data_input')
        shinyjs::show(id = 'box_skipping_data_user')
      }
    })

    observeEvent(input$action_button_polar, {
      if(input$action_button_polar %% 2 == 0) {
        shinyjs::hide(id = 'box_good_polar')
        shinyjs::hide(id = 'answer_polar')
      } else {
        shinyjs::show(id = 'box_good_polar')
        shinyjs::show(id = 'answer_polar')
      }
    })


    observeEvent(input$action_button_dim, {
      if(input$action_button_dim %% 2 == 0) {
        shinyjs::hide(id = 'box_good_dim')
        shinyjs::hide(id = 'answer_dim')
      } else {
        shinyjs::show(id = 'box_good_dim')
        shinyjs::show(id = 'answer_dim')
      }
    })
}

shinyApp(ui, server)
