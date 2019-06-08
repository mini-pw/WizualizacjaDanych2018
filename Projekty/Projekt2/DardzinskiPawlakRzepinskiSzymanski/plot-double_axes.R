######################################################################
# Double-axes plot
######################################################################

library(magrittr)
source("./builder.R")

pd <- read.csv("plot-double_axes.csv")

# https://blog.datawrapper.de/dualaxis/

ggplot(pd, aes(x=Year, y=Germany)) + geom_bar(stat="identity")


ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  scale_y_continuous(
    "mpg (US)", 
    sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
  )

to_trillion <- function(b) { paste0(round(b / 10^12, 0), "T")}

ratio <- max(pd$World) / max(pd$Germany)

bad_plot <- ggplot() + 
  geom_line(mapping = aes(x = pd$Year, y = pd$World), color = "grey") +
  geom_line(mapping = aes(x = pd$Year, y = pd$Germany*ratio), color = "blue") + 
  scale_y_continuous(name = "World", labels = to_trillion,
                     sec.axis = sec_axis(~./ratio, name = "Germany", labels = to_trillion)) + 
  xlab("Year") +
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "blue"))

# So while the chart looks like the German GDP and the global GDP go up at roughly the same rate (at least until 2014), 
# they don’t. The global GDP increased by 80% until 2014; the GDP of Germany by 40%.

library(dplyr)
library(tidyr)
new_pd <- mutate(pd) %>%
  mutate(Germany_Change = Germany/Germany[1] * 100 - 100, World_Change = World/World[1] * 100 - 100) %>%
  ungroup %>%
  select(Year, Germany_Change, World_Change) %>% 
  gather(key, value, Germany_Change, World_Change)

good_plot <- ggplot(new_pd, aes(x=Year, y=value, colour=key)) +
  geom_line()

builder <- create_visualization_case_builder() %>% 
  add_good_plot(good_plot) %>% 
  add_bad_plot(bad_plot) %>% 
  add_qa_case("How much global GDP increased until 2014?", 80) %>% 
  add_qa_case("How much GDP of Germany increased until 2014?", 40)


app <- build(builder)
shinyApp(app$ui, app$server)
