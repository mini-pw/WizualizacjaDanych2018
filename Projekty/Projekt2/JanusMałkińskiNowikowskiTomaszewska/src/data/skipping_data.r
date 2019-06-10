require(ggplot2)
require(dplyr)
require(SmarterPoland)
require(ggsci)

accidents <-
  c(13, 120, 40, 100, 115, 13, 38, 70, 84, 110, 48, 133, 63)
accidents_df <-
  data.frame(year_n = seq(2005, 2017), accidents = accidents) %>%
  mutate(year_f = as.factor(year_n))

description <- 'Selective data picking'

skipping_data_increasing_plot <-
  ggplot(accidents_df %>% filter(year_n %in% c(2010, 2012, 2014, 2016)),
         aes(x = year_f, y = accidents, label = accidents)) +
  geom_bar(
    width = 0.5,
    stat = "identity",
    color = "black",
    fill = "red"
  ) +
  geom_label() +
  ggtitle('Measles accidents noted in Poland over years') +
  xlab('Year') +
  ylab('Number of accidents') +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


skipping_data_decreasing_plot <-
  ggplot(accidents_df %>% filter(year_n %in% c(2006, 2008, 2013, 2017)),
         aes(x = year_f, y = accidents, label = accidents)) +
  geom_bar(
    width = 0.5,
    stat = "identity",
    color = "black",
    fill = "#005500"
  ) +
  geom_bar(
    width = 0.5,
    stat = "identity",
    color = "black",
    fill = "#006400"
  ) +
  geom_label() +
  ggtitle('Measles accidents noted in Poland over years') +
  xlab('Year') +
  ylab('Number of accidents') +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


skipping_data_full_plot <-
  ggplot(accidents_df, aes(x = year_f, y = accidents, label = accidents)) +
  geom_bar(
    width = 0.5,
    stat = "identity",
    color = "black",
    fill = "blue"
  ) +
  geom_label() +
  ggtitle('Measles accidents noted in Poland over years') +
  xlab('Year') +
  ylab('Number of accidents') +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


get_skipping_data_user_plot <- function(input) {
  validate(need(
    !is.null(input$accidents_year_selected),
    "Please select some years"
  ))
  
  selected <- as.numeric(input$accidents_year_selected)
  
  df <- accidents_df %>%
    filter(min(selected) <= year_n &
             year_n <= max(selected)) %>%
    mutate(accidents = ifelse(year_n %in% selected, accidents, NA))
  
  skipping_data_user_plot <-
    ggplot(df, aes(x = year_f, y = accidents, label = accidents)) +
    geom_bar(
      width = 0.5,
      stat = "identity",
      color = "black",
      fill = "blue"
    ) +
    geom_label() +
    ggtitle('Measles accidents noted in Poland over years') +
    xlab('Year') +
    ylab('Number of accidents') +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  
  return(skipping_data_user_plot)
}