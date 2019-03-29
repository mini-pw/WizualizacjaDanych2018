library(dplyr)
library(ggplot2)
library(gridSVG)

df <- airquality %>% na.omit() %>% 
  group_by(Day) %>% 
  summarise(mean_ozone=mean(Ozone), mean_wind=mean(Wind))

days <- c(df$Day, df$Day)
vals <- c(df$mean_ozone, df$mean_wind)
type <- as.factor(c(rep('ozone', nrow(df)), rep('wind', nrow(df))))
new_df <- data.frame(day=days, vals=vals, type=type)


p <- new_df %>% ggplot(aes(x=day, y=vals)) +
  geom_line() +
  facet_wrap(~type)


svg("ozone.svg", height = 7.5, width = 8)
p
dev.off()
