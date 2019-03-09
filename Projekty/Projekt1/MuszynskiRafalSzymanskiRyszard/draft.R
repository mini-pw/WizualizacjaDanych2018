library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

activity <- read_csv("~/mi-dane/ACTIVITY/ACTIVITY_1551727501568.csv")

#' Steps per day of week
activity %>% 
  mutate(weekday = wday(date)) %>% 
  ggplot(aes(x = weekday, y = steps, fill = weekday)) + 
  geom_bar(stat = "summary", fun.y = "mean") 

#' Steps per month
activity %>% 
  mutate(month = month(date)) %>% 
  ggplot(aes(x = month, y = steps, fill = month)) + 
  geom_bar(stat = "summary", fun.y = "mean") 


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


## messenger data

library(jsonlite)
library(dplyr)
library(anytime)

files <- list.files(path="~/messages/inbox", pattern='message.json', full.names=TRUE, recursive=TRUE)

data <- lapply(files, function(filename) {
  fromJSON(filename)$messages %>%
    filter(sender_name == "Rafa\u00c5\u0082 Muszy\u00c5\u0084ski") %>%
    mutate(length = nchar(content), date = anydate(timestamp_ms/1000)) %>%
    select(length, date)
})

data <- lapply(files, function(filename) {
  fromJSON(filename)$messages %>%
    filter(sender_name == "Rafa\u00c5\u0082 Muszy\u00c5\u0084ski") %>%
    mutate(date = anydate(timestamp_ms/1000)) %>%
    group_by(date) %>%
    summarise(msgCount =  count())
})

df <- do.call(rbind, data)

df %>%
  group_by(date) %>%
  summarise(lengthSum = sum(length))


