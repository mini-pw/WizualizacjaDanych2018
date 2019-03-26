library(anytime)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(readr)
library(gridSVG)
library(scales)
library(viridis)


#' Steps data
activity <- read_csv("~/mi-dane/ACTIVITY/ACTIVITY_1551727501568.csv")

activity_by_week <- activity %>% 
  group_by(week = as.Date(cut(date, "week"))) %>% 
  summarise(steps = mean(steps)) %>% 
  mutate(
    Quality = ifelse(steps < 8000, "Bad", "Good") %>% 
      factor(levels = c("Good", "Bad"))
  )

Sys.setlocale("LC_TIME", "en_US.UTF-8")

activity_plot <- ggplot(data = activity_by_week,
       aes(x = week, y = steps, fill = Quality)) +
  geom_bar(stat = "summary", fun.y = "identity") +
  scale_x_date() +
  scale_fill_viridis_d(begin = 0.8, end = 0.1) +
  xlab("Date") +
  ylab("Average number of steps") +
  geom_hline(yintercept = 8000, color = 'red') + 
  geom_text(x=as.Date('2019-02-01'), y = 8000,label = 'recommended', vjust = -1, size = 5) +
  theme_minimal(base_size = 22)


svg("~/activity_plot.svg", bg = "transparent", height = 4.5, width = 7)
activity_plot
dev.off()

#' Sleep data
sleep <- read_csv("~/mi-dane/SLEEP/SLEEP_1551727501867.csv")

# sleep density 
# src: https://www.ofeminin.pl/fitness-i-zdrowie/dolegliwoscichoroby/ile-godzin-snu-potrzebujemy-w-zaleznosci-od-wieku-tabela/g8flrtx
# < 6 - bad 
# 6-7 to little but ok
# 7 - 9 bueno
# 9 - 11 too much but ok
# > 11 too much
sleepBreaks <- c(6,7,9,11)

getSleepQuality <- function(duration){
  sapply(duration, function(duration){
    if(duration < 6 || duration > 11){
      return('Bad')
    } else if(duration < 7 || duration > 9) {
      return('Poor')
    } else
      return('Good')
  })
}

sleepDensity <-
  sleep %>% filter(deepSleepTime > 0 & shallowSleepTime > 0) %>%
  mutate(total = (deepSleepTime + shallowSleepTime) / 60) %>%
  pull(total) %>%
  density()

sleep_plot <- data.frame(x=sleepDensity$x, y=sleepDensity$y) %>%
  mutate(Quality = factor(getSleepQuality(x), levels = c('Good','Poor','Bad'))) %>%
  mutate(groups = factor(findInterval(x, sleepBreaks))) %>%
  ggplot(aes(x,y, group = groups)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=Quality), color = NA) +
  scale_x_continuous(breaks=sleepBreaks, name = 'Hours of sleep') +
  scale_fill_viridis_d(end = 0.1, begin = 0.8) +
  scale_y_continuous(labels = NULL, name = NULL) +
  theme_minimal(base_size = 22)


svg("~/sleep_plot.svg", bg = "transparent", height = 4.5, width = 7)
sleep_plot
dev.off()

# compute cumulative distribution function
f <- approxfun(sleepDensity$x, sleepDensity$y, yleft=0, yright=0)
sleepCDF <-function(x){integrate(f, -Inf, x)}
fractions <- sapply(sleepBreaks, sleepCDF)['value', ]

# some cool usage of cdf
percentageOfReallyBadSleep <- fractions[[1]] + (1 - fractions[[4]])

## messenger data
files <- list.files(path="~/Downloads/facebook-data/messages/inbox", pattern='message.json', full.names=TRUE, recursive=TRUE)

df <- lapply(files, function(filename) {
  fromJSON(filename)$messages %>%
    mutate(time = ymd_hms(anytime(timestamp_ms/1000)), "%H:%M:%S") %>% 
    select(time)
}) %>% bind_rows()

#'
fb_msg_cycle <- df %>% 
  mutate(time = time %>% 
           strftime(format = "%H:00:00") %>% 
           as.POSIXct(format = "%H:%M:%S")
  ) %>% 
  group_by(time) %>% 
  summarize(msg_count = n()) 

fb_activity_plot <- fb_msg_cycle %>% 
  ggplot(aes(x = time, y = 0, fill = msg_count)) +
  geom_tile() + 
  scale_fill_viridis() +
  scale_x_datetime(labels = date_format("%H"),
                   date_breaks = "1 hours") +
  theme_minimal(base_size = 22) +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  coord_polar(start = -0.38) +
  labs(fill = "Amount of messages")
  
svg("~/fb_plot.svg", bg = "transparent", height = 4.5, width = 7)
fb_activity_plot
dev.off()
