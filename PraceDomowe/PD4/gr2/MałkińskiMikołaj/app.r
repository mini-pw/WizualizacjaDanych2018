library('shiny')
library('dplyr')
library('ggplot2')
library('stringr')

load_data <- function() {
    Health<-data.frame(
        reason=c('No initial health problems',  'No change in health', 'No initial problems or change'), 
        value=c(-3.2, -1.6, -4.8) 
    )
    Employment<-data.frame(
        'reason'=c('No voluntary switches','No job loss, with new job','No job loss, w/out new job'),
        'value'=c(0.6, 0.3, -2.3) 
    )
    Family_related<-data.frame(
        'reason'=c('No spouse retirement','No parents moving in'),
        'value'=c(-0.8,-0.2) 
    )
    Financial<-data.frame(
        'reason'=c('No financial gains','No financial losses'),
        'value'=c(-0.7,-0.9) 
    )
    Health <- cbind(Health, type = c('Health', 'Health', 'Health'))
    Employment <- cbind(Employment, type = c('Employment', 'Employment', 'Employment'))
    Family_related <- cbind(Family_related, type = c('Family-related', 'Family-related'))
    Financial <- cbind(Financial, type = c('Financial', 'Financial'))
    
    all_df <- rbind(Health, Employment, Family_related, Financial)
    all_df$reason = factor(all_df$reason, levels = all_df$reason)
    
    custom_fill <- c()
    custom_fill[0:8] <- 'darkred'
    custom_fill[9:10] <- 'firebrick1'
    all_df <- cbind(all_df, custom_fill)
    
    return (all_df)
}

draw_main_plot <- function(df){
    p <- ggplot(data = df, aes(x = reason, y = value, font)) +
        geom_bar(stat = 'identity', width = 0.4, color = 'black', fill = df$custom_fill) +
        geom_text(aes(y = value + 0.3 * sign(value), label = paste0(value, '%'))) +
        geom_vline(xintercept = 3.6, linetype = 'dashed') +
        scale_y_continuous(name = '', limits = c(-6, 2), labels = function(x) paste0(x, '%')) +
        scale_x_discrete(name = '', labels = function(x) str_wrap(x, width = 8)) +
        scale_fill_manual(values = c('darkred' = 'darkred', 'forestgreen' = 'orange')) +
        facet_wrap(~ type, nrow = 1, scales = 'free_x') +
        theme(
            text = element_text(family = 'serif', size = 12, face = 'bold'),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = 'lightgray'),
            panel.grid.major.x = element_blank(),
            panel.spacing.x = unit(0, 'lines'),
            strip.background = element_blank(),
            strip.text = element_text(face = 'italic')
        )
    return (p)
}

all_df <- load_data()

ui <- fluidPage(
    titlePanel('Praca domowa 4'),
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = 'selected_types', label = 'Select type', choices = unique(all_df$type), selected = unique(all_df$type)),
            sliderInput(inputId = 'selected_value_range', label = 'Select value range', min = min(all_df$value), max = max(all_df$value), value = c(-Inf, Inf))
        ),
        mainPanel(
            h2('Barplot'),
            plotOutput('main_plot', height = 600, click = 'main_plot_click'),
            tableOutput('main_table')
        )
    )
)

server <- function(input, output) {
    filtered_df <- reactive({
        all_df %>%
            filter(type %in% input[['selected_types']]) %>%
            filter(value >= min(input[['selected_value_range']]) & value <= max(input[['selected_value_range']]))
    })
    
    output[['main_plot']] <- renderPlot({
        draw_main_plot(filtered_df())
    })
    
    output[['main_table']] <- renderTable({
        filtered_df() %>% select(reason, value, type)
    })
}

shinyApp(ui = ui, server = server)
