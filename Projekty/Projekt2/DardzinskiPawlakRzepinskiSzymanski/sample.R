library(magrittr)
source("./builder.R")

pie_chart <- ggplot(mtcars, aes(x = "", y = cyl, fill = cyl)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0)

bar_chart <- ggplot(mtcars, aes(x=cyl, fill=cyl)) +
  geom_bar()

builder <- create_visualization_case_builder() %>% 
  add_good_plot(bar_chart) %>% 
  add_bad_plot(pie_chart) %>% 
  add_qa_case("What is the value for xxx?", 30) %>% 
  add_qa_case("What is the value for yyy?", 60) %>% 
  add_qa_case("What is the value for zzz?", 30) %>% 
  add_qa_case("What is the value for aaa?", 60)
  

app <- build(builder)
shinyApp(app$ui, app$server)

