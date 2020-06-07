# Author: Casey Rodriguez
# Find me on GitHub: https://github.com/Xenosplitter

# File for finding beginning and ending of emails for a given bank

rm(list = ls()) # clean environment

# manage what knits install.packages("tidyverse")
knitr::opts_chunk$set(echo = T, error = F, warning = F)

# load libraries for later use
library(tidyverse)
library(ggthemes)
library(scales)
library(lubridate)

# load in data for later use
emails = read_csv("Email Logs.csv")

start_end = emails %>%
  group_by(BANK) %>% # do calculation per bank
  summarize(start_time = min(TIME), # first TIME for BANK
            end_time = max(TIME), # last TIME for BANK
            duration = end_time - start_time,
            start_date = `LOG DATE`,
            # if times overlap midnight (at 86400), set end date to next day
            end_date = ifelse(start_time + duration >= 86400, "5/30/20", start_date)) %>%
  unique()

new_emails = start_end %>%
  mutate(start_time = seconds_to_period(start_time),
         start_time = sprintf('%02d:%02d:%02d', start_time@hour, minute(start_time), second(start_time)),
         end_time   = seconds_to_period(end_time),
         end_time   = sprintf('%02d:%02d:%02d', end_time@hour, minute(end_time), second(end_time)))

new_emails = new_emails[,c(1,2,5,3,6,4)]

write.csv(new_emails, "processed_email_logs.csv")