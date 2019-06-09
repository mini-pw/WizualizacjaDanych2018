######################################################################
# Scale importance
######################################################################

library(magrittr)
source("./builder.R")

trial <- c("A","B","C")
people <- c(10, 100, 1000)
pd <- data.frame(trial, people)

bad_plot <- ggplot(pd, aes(x=trial, y=people)) + geom_bar(stat="identity")

good_plot <- ggplot(pd, aes(x=trial, y=people)) + geom_bar(stat="identity") + scale_y_log10()

builder <- create_visualization_case_builder() %>% 
  add_bad_plot(bad_plot) %>% 
  add_good_plot(good_plot) %>% 
  add_qa_case("What is the value of trial A?", 10) %>% 
  add_qa_case("What is the value of trial B?", 100)


app <- build(builder)
shinyApp(app$ui, app$server)