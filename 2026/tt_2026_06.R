# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 6
# 2026-02-10
# Winter Olympics

# Load packages ----

library(tidyverse)
library(tidytuesdayR)
# library(png)
# library(grid)
# library(sysfonts)

# font_add_google("Roboto")
# showtext::showtext_auto()


# Data ----

tuesdata <- tidytuesdayR::tt_load(2026, week = 6)
schedule <- tuesdata$schedule

head(schedule)

schedule |> 
  distinct(discipline_name)

schedule |> 
  distinct(event_code)

schedule |> 
  rowid_to_column() |> 
  filter(is_medal_event == TRUE) |> 
  ggplot(aes(x = start_datetime_local, xend = end_datetime_local,
             y = discipline_name, yend = discipline_name,
             color = is_medal_event)) +
  geom_segment()
