library(anytime)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(readr)
library(scales)


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


sleep %>% filter(deepSleepTime > 0 & shallowSleepTime > 0) %>%
  ggplot(aes(x = deepSleepTime + shallowSleepTime)) + 
  geom_density()


## messenger data
files <- list.files(path="~/Downloads/facebook-rysiekszymanski/messages/inbox", pattern='message.json', full.names=TRUE, recursive=TRUE)

data <- lapply(files, function(filename) {
  fromJSON(filename)$messages %>%
    mutate(time = ymd_hms(anytime(timestamp_ms/1000)), "%H:%M:%S") %>% 
    select(time)
})

df <- do.call(rbind, data)

#'
df %>% 
  mutate(time = time %>% 
           as.POSIXct(format = "%H:%M:%S") %>%
           strftime(format = "%H:00:00") %>% 
           as.POSIXct(format = "%H:%M:%S")
         ) %>% 
  group_by(time) %>% 
  summarize(msg_count = n()) %>% 
  ggplot(aes(x = time, y = 0, fill = msg_count)) +
  geom_tile() + 
  scale_fill_gradient(low = "blue", high = "red") +
  scale_x_datetime(labels = date_format("%H"),
                   date_breaks = "1 hours")
