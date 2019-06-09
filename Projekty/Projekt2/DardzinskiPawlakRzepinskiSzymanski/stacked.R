library(ggplot2)


stacked_example <- function() {
  data <- data.frame(
    year = rep(seq(2017,2021), 3),
    n = c(33, 42, 39, 38, 40,  40, 32, 35, 36, 45,  22, 45, 43, 38, 35),
    type = rep(c("Printer", "Projector", "Whiteboard"), each=5)
  )
  
  bad <- ggplot(data, aes(x=year, y=n)) +
    geom_area(aes(fill=type)) + 
    ggtitle("School inventory over years") +
    ylab("Number of items") +
    xlab("Year") +
    scale_fill_brewer() +
    labs(fill = "Item")
  
  good <- ggplot(data, aes(x=year, y = n)) +
    geom_bar(aes(fill = type), stat = "identity", position = "dodge") +
    ggtitle("School inventory over years") +
    ylab("Number of items") +
    xlab("Year") +
    labs(fill = "Item")  
  
  builder <- create_visualization_case_builder() %>% 
    add_good_plot(good) %>% 
    add_bad_plot(bad) %>% 
    add_qa_case("How many printers were in 2019", 30) %>% 
    add_qa_case("Did the number of projectors decrease in 2020", "No")
  
  return (builder)
}
