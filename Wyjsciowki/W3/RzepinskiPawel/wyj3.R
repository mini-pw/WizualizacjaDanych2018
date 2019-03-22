library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)

category <- c("nieustalony status na rynku pracy", "bierni zawodowo", "bezrobotni", "pracujący")
men <- c(800271, 5504794, 1082640, 8264503)
men <- men / sum(men)
women <- c(871179, 8402675, 967414, 6786138)
women <- women / sum(women)
df1_raw <- data.frame(category, men, women)
df1 <- melt(df1_raw, id.vars='category', value.name="percentage")

ggplot(df, aes(x = category)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))

gender_plot <- ggplot(df1, aes(fill=category, y=percentage, x=variable)) + 
  geom_bar(position="dodge", stat="identity") 

city <- c(1134364, 8656019, 1285944, 9090543)
city <- city / sum(city)
countryside <- c(537087, 5251450, 764111, 5960098)
countryside <- countryside / sum(countryside)
df2_raw <- data.frame(category, city, countryside)
df2 <- melt(df2_raw, id.vars='category', value.name="percentage")

place_plot <- ggplot(df2, aes(fill=category, y=percentage, x=variable)) + 
  geom_bar(position="dodge", stat="identity") 

grid.arrange(gender_plot, place_plot, nrow = 2)
