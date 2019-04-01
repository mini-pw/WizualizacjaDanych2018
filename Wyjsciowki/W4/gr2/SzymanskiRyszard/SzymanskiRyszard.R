library(ggplot2)
library(magick)
library(patchwork)
library(viridis)

line_per_chicken <- ChickWeight %>% 
  ggplot(aes(x = Time, y = weight, group = Chick)) +
  geom_line(aes(color = Chick)) +
  scale_fill_viridis() +
  facet_grid(~ Diet)

diet_trend <- ChickWeight %>% 
  ggplot(aes(x = Time, y = weight, color = Diet)) +
  geom_smooth()

chick_plot <- diet_trend / line_per_chicken
ggsave(filename = "raw_chick_plot.svg", plot = chick_plot)

chick_image <- image_read("chick.jpg") %>% 
  image_scale("150")
chick_plot_image <- image_read_svg("raw_chick_plot.svg")

final_plot <- image_composite(chick_plot_image,
                chick_image,
                offset = "+50+10")
image_write(final_plot, path = "chick_plot.svg", format = "svg")
