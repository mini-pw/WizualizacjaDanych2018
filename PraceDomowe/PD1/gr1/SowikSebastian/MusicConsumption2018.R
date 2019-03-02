library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(tidyr)
library(scales)

#Source: https://www.nytimes.com/2019/01/31/learning/whats-going-on-in-this-graph-feb-6-2019.html

genre <- c("R&B/HIP-HOP", "POP", "ROCK", "COUNTRY")
including_streaming <- c(0.54, 0.19, 0.13, 0.095) 
traditional_sales_only<- c(0.2, 0.23, 0.33, 0.14) 

music_consumption_df = data.frame(genre, including_streaming, traditional_sales_only)
music_consumption_narrow_df = gather(music_consumption_df, "type", "share", including_streaming, traditional_sales_only
                                    )
genre_order <- music_consumption_narrow_df %>% 
  filter(grepl("including_streaming", type)) %>% 
  arrange(share) %>% 
  pull(genre) %>% 
  as.character()
genre <- factor(genre, levels=genre_order)
music <- mutate(music_consumption_narrow_df,
       pct = share * 100) 
  
music_back <- music %>% top_n(2) %>% 
  mutate(share = 1.0, pct = 100)


ggplot(music, aes(x = factor(type, levels = c("traditional_sales_only", "including_streaming" ) ), fill = factor(genre, levels=genre_order) , y = share)) +
  geom_bar(data = music_back, aes(fill = NA), colour = "black", size = 2, stat = "identity", position = "stack", width=0.4 ) +
  geom_bar(colour = "black", size = 2, stat = "identity", position = "stack", width=0.4 ) +
  geom_text(aes(label = paste0(pct, '%' )), position = position_stack(vjust=0.5), size=5, fontface="bold") +
  geom_text(data = music_back, aes(label = toupper(gsub("_", " ", type)) ), nudge_x = 0.4, nudge_y = -0.9) +
  ggtitle("2018 music consumption by genre..." ) +
  scale_fill_brewer(type = "qual", palette = 8, na.value="white") +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.key.size = unit(10, "mm"),
        legend.spacing.x = unit(7, "mm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold")) +
  guides(fill = guide_legend(aes(fill = genre_order))) +
  coord_flip() 
     
















