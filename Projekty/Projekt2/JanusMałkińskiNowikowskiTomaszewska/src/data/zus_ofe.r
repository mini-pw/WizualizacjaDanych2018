require(gridExtra)
require(reshape2)

zus = c(12.7, 6.7, 1.9, 2, 3.6, 5.6, 6.9, 12.9, 16.3, 7.2, 3.9, 4.5)
ofe = c(13.9, 5.7, 12.6, 11.4, 14.5, 14, 15.9, 5.2,-13.9, 14.3, 10.8,-4.8)
year = seq(2000, 2011)

zus_ofe_df <- data.frame(year, zus, ofe)

zus_ofe_orig_plot <- function(input) {
  zus <-
    ggplot(zus_ofe_df, aes(
      x = sort(year, decreasing = T),
      y = zus,
      label = zus
    )) +
    geom_bar(stat = 'identity', fill = "#8bbf41") +
    coord_flip() +
    scale_y_continuous(
      breaks = seq(0, 20, 5),
      limits = c(0, 20),
      minor_breaks = NULL,
      expand = c(0, 0),
      name = "Gain in percentage"
    ) +
    scale_x_discrete(
      limits = sort(year, decreasing = T),
      name = "",
      labels = as.character(year)
    ) +
    geom_label(fill = "white") +
    ggtitle('ZUS', subtitle = 'Gain in perctange') +
    theme(
      legend.position = "none",
      panel.border = element_rect(colour = "black", fill = NA),
      # panel.grid = element_line(colour="black"),
      panel.background = element_rect(colour = "red")
    )
  ofe_limit <- input$zus_ofe_ofe_range
  ofe <-
    ggplot(zus_ofe_df, aes(
      x = sort(year, decreasing = T),
      y = ofe,
      label = ofe,
      fill = ofe < 0
    )) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = c("#94509e", "#ec1747")) +
    coord_flip() +
    scale_y_continuous(
      breaks = seq(-ofe_limit, ofe_limit, 10),
      limits = c(-ofe_limit, ofe_limit),
      minor_breaks = NULL,
      expand = c(0, 0),
      name = "Gain in percentage"
    ) +
    scale_x_discrete(
      limits = sort(year, decreasing = T),
      name = "",
      labels = as.character(year)
    ) +
    geom_label(fill = "white") +
    ggtitle('OFE', subtitle = 'Gain in perctange') +
    theme(
      legend.position = "none",
      panel.border = element_rect(colour = "black", fill = NA),
      # panel.grid = element_line(colour="black"),
      panel.background = element_rect(colour = "red")
    )
  
  
  grid.arrange(zus, ofe, ncol = 2)
}

get_zus_ofe_real_ratio <- function(input) {
  item <-
    zus_ofe_df %>% filter(year == input$zus_ofe_year) %>% top_n(1)
  ratio <- round(item$zus / item$ofe, 2)
  valueBox(abs(ratio), "Real ratio* (ZUS/OFE)", color = "green")
}

get_zus_ofe_current_ratio <- function(input) {
  item <-
    zus_ofe_df %>% filter(year == input$zus_ofe_year) %>% top_n(1)
  ratio <-
    round((item$zus / 20) / (item$ofe / (2 * input$zus_ofe_ofe_range)), 2)
  valueBox(abs(ratio), "Presented ratio* (ZUS/OFE)", color = "red")
}

zus_ofe_ratio <- "* ratio based on bar heights"

get_zus_ofe_good_plot <- function(input) {
  dat3 <- zus_ofe_df %>% gather(Organization, Value,-year)
  ggplot(dat3, aes(
    x = year,
    y = Value,
    fill = Organization,
    label = Value
  )) +
    geom_col(position = "dodge") +
    scale_y_continuous(
      breaks = seq(-20, 20, 5),
      limits = c(-20, 20),
      minor_breaks = NULL,
      expand = c(0, 0),
      name = "Gain in percentage"
    ) +
    scale_x_discrete(
      limits = sort(year, decreasing = F),
      labels = as.character(year),
      name = "Year"
    ) +
    geom_label(
      aes(group = Organization),
      fill = "white",
      position = position_dodge(width = 1),
      size = 4
    ) +
    ggtitle('How much have we gained') +
    theme_light()
}