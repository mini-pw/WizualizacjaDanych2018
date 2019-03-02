library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(tidyr)
library(scales)

#Source: https://www.nytimes.com/2019/01/31/learning/whats-going-on-in-this-graph-feb-6-2019.html

genre <- c("R&B/HIP-HOP", "POP", "ROCK", "COUNTRY")
including_streaming <- c(0.54, 0.19, 0.13, 0.095) 
only_traditional <- c(0.2, 0.23, 0.33, 0.14) 
blank <- c(0, 0, 0, 0) 

music_consumption_df = data.frame(genre, including_streaming, only_traditional, blank)
music_consumption_narrow_df = gather(music_consumption_df, "type", "share", including_streaming, only_traditional)

music <- mutate(music_consumption_narrow_df,
       pct = share * 100) 
  
music_back <- music %>% top_n(2) %>% 
  mutate(share = 1.0, pct = 100)


ggplot(music, aes(x = factor(type, levels = c("only_traditional", "including_streaming" ) ), fill = genre, y = share)) +
  geom_bar(data = music_back, colour = "black", size = 2, stat = "identity", position = "stack", width=0.4 ) +
  geom_bar(colour = "black", size = 2, stat = "identity", position = "stack", width=0.4 ) +
  geom_text(aes(label = paste0(pct, '%' )), position = position_stack(vjust=0.5), size=5, fontface="bold") +
  geom_text(data = music_back, aes(label = toupper(gsub("_", " ", type)) ), nudge_x = 0.4, nudge_y = -0.9) +
  ggtitle("2018 music consumption by genre..." ) +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.key.size = unit(10, "mm"),
        legend.spacing.x = unit(7, "mm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_blank(),
        plot.title = element_text(size = 20, face = "bold")) +
  coord_flip() 
     
















