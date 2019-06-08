######################################################################
# Area is hard to read
######################################################################

library(magrittr)
source("./builder.R")

# https://www.data-to-viz.com/caveat/area_hard.html
# https://www.data-to-viz.com/caveat/radius_or_area.html

data <- data.frame(name=c("United States", "China", "Japan", "Germany", "France"),
                   gdp=c(14.6,5.7,5.3,3.3,2.5) )

bad_plot <- ggplot(data, aes(x=name, y=1, size=gdp)) +
  geom_point(color="#69b3a2") +
  geom_text(aes(label=name), size=3) +
  scale_size_continuous(range=c(5,29.2)) +
  theme_void() +
  theme(
    legend.position="none"
  ) +
  ylim(0.9,1.1)

good_plot <- ggplot(data, aes(x=reorder(name, -gdp), y=gdp)) +
  geom_bar(stat="identity", fill="#69b3a2")

# US GDP vs China GDP? How many times more?
# Ordering, area vs radius for circle plots

builder <- create_visualization_case_builder() %>% 
  add_bad_plot(bad_plot) %>% 
  add_good_plot(good_plot) %>% 
  add_qa_case("How many times US GDP is larger than China's?", 2.56)
  
  
app <- build(builder)
shinyApp(app$ui, app$server)
