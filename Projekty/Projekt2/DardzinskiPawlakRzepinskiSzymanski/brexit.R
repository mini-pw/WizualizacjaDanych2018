library(ggplot2)
source("./builder.R")

# sources
# https://blogs.lse.ac.uk/brexit/2019/06/06/what-do-11-year-olds-think-about-brexit-i-asked-and-they-didnt-hold-back/
# https://www.reddit.com/r/dataisugly/comments/bxmprz/data_is_yes/

brexit_example <- function() {
  data <- data.frame(
    val = c(27,45,28, 34, 54, 12) / 100,
    answer = rep(c("Yes","No", "Don't know"), times=2),
    brexit = rep(c("Good idea", "Bad idea"), each=3)
  )
  
  bad <- ggplot(data, aes(x = brexit, y = val, fill= answer)) +
    scale_y_continuous(labels = scales::percent) +
    geom_bar(stat = "identity", width = 0.4) +
    ggtitle("Brexit by whether politicians are doing a good job") +
    ylab("Are politicians are doing a good job?") +
    xlab("Is it a good or bad idea to leave the EU?") +
    theme(legend.position="bottom") +
    scale_fill_brewer() + 
    labs(fill = "",
         caption = "source: https://blogs.lse.ac.uk/brexit/2019/06/06/what-do-11-year-olds-think-about-brexit-i-asked-and-they-didnt-hold-back/")

  good <- ggplot(data, aes(x = answer, y = val, fill= answer)) +
    scale_y_continuous(labels = scales::percent) +
    geom_bar(stat = "identity", width = 0.4) +
    ggtitle("Assesment of politicians by opinion on Brexit") +
    ylab("Are politicians are doing a good job?") +
    labs(fill = "") +
    xlab("") +
    scale_fill_brewer() + 
    facet_wrap(~brexit)
  
  builder <- create_visualization_case_builder() %>% 
    add_good_plot(good) %>% 
    add_bad_plot(bad) %>% 
    add_qa_case("To which axis does the legend refer to?", "Y-axis") %>% 
    add_qa_case("What did the author want to show?", "I don't know either, but probably correlation between support of Brexit and politicians' assesment")
  
  return (builder)
}