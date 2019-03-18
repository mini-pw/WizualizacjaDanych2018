library(anytime)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(readr)
library(scales)
library(viridis)


#' Step data
activity <- read_csv("~/mi-dane/ACTIVITY/ACTIVITY_1551727501568.csv")

activity %>% 
  mutate(
    month = month(date, abbr = T),
    quality = ifelse(steps > 8000, "Yes", "No")) %>% 
  ggplot(aes(x = date, y = steps, fill = quality)) + 
  geom_bar(stat = "summary", fun.y = "identity")

#' Sleep data
sleep <- read_csv("~/mi-dane/SLEEP/SLEEP_1551727501867.csv")

sleep_data <- sleep %>% 
  mutate(
    weekday = weekdays(date),
    total_sleep_time = deepSleepTime + shallowSleepTime,
    bed_time = factor(start %>% anytime() %>% hour()),
    wakeup_time = factor(stop %>% anytime() %>% hour()),
  ) %>% 
  filter(total_sleep_time > 0) %>% 
  mutate(
    deep_sleep_pct = deepSleepTime / total_sleep_time
  )

#' sleep per day of the week
ggplot(sleep_data, aes(x = weekday, y = total_sleep_time, fill = weekday)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  coord_flip()

#' Percentage of deep sleep
ggplot(sleep_data, aes(x = weekday, y = deep_sleep_pct, fill = weekday)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  coord_flip()

#' At what time do I go to sleep usually?
ggplot(sleep_data, aes(x = bed_time)) + 
  geom_bar(stat = "count") +
  coord_flip()

#' At What time do I wake up usually?
ggplot(sleep_data, aes(x = wakeup_time)) + 
  geom_bar(stat = "count") +
  coord_flip()

#' sleep per weeks
sleep %>%
  mutate(total_sleep_time = deepSleepTime + shallowSleepTime) %>% 
  filter(total_sleep_time > 300) %>% 
  ggplot(aes(x = date, y = total_sleep_time)) + 
  geom_line()

# sleep density 
# src: https://www.ofeminin.pl/fitness-i-zdrowie/dolegliwoscichoroby/ile-godzin-snu-potrzebujemy-w-zaleznosci-od-wieku-tabela/g8flrtx
# < 6 - bad 
# 6-7 to little but ok
# 7 - 9 bueno
# 9 - 11 too much but ok
# > 11 too much
sleepBreaks <- c(6,7,9,11)

sleepDensity <-
  sleep %>% filter(deepSleepTime > 0 & shallowSleepTime > 0) %>%
  mutate(total = (deepSleepTime + shallowSleepTime) / 60) %>%
  pull(total) %>%
  density()

data.frame(x=sleepDensity$x, y=sleepDensity$y) %>%
  mutate(quality = factor(findInterval(x, sleepBreaks))) %>%
  ggplot(aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quality), color = NA) +
  scale_x_continuous(breaks=sleepBreaks, name = 'długość snu') +
  scale_fill_manual( values = c('red','orange','green','orange','red')) +
  scale_y_continuous(labels = NULL) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())


# compute cumulative distribution function
f <- approxfun(sleepDensity$x, sleepDensity$y, yleft=0, yright=0)
sleepCDF <-function(x){integrate(f, -Inf, x)}
fractions <- sapply(sleepBreaks, sleepCDF)['value', ]

# some cool usage of cdf
percentageOfReallyBadSleep <- fractions[[1]] + (1 - fractions[[4]])

## messenger data
files <- list.files(path="~/Downloads/facebook-data/messages/inbox", pattern='message.json', full.names=TRUE, recursive=TRUE)

data <- lapply(files, function(filename) {
  fromJSON(filename)$messages %>%
    mutate(time = ymd_hms(anytime(timestamp_ms/1000)), "%H:%M:%S") %>% 
    select(time)
})

df <- do.call(rbind, data)

#'
fb_msg_cycle <- df %>% 
  mutate(time = time %>% 
           as.POSIXct(format = "%H:%M:%S") %>%
           strftime(format = "%H:%M:00") %>% 
           as.POSIXct(format = "%H:%M:%S")
  ) %>% 
  group_by(time) %>% 
  summarize(msg_count = n()) 

fb_msg_cycle %>% 
  ggplot(aes(x = time, y = 0, fill = msg_count)) +
  geom_tile() + 
  scale_fill_viridis() +
  scale_x_datetime(labels = date_format("%H"),
                   date_breaks = "1 hours") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_polar(start = -0.38) +
  labs(fill = "Liczba wiadomości")
  