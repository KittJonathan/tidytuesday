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

# Get the data ----

tuesdata <- tidytuesdayR::tt_load(2026, week = 6)
schedule <- tuesdata$schedule

# Explore the data ----

medals <- schedule |> 
  select(date, discipline_name, event_description, is_medal_event) |> 
  filter(is_medal_event == TRUE | str_detect(event_description, "Medal")) |>
  mutate(gold = case_when(str_detect(event_description, "Bronze") ~ FALSE,
                          .default = TRUE),
         silver = case_when(str_detect(event_description, "Bronze") ~ FALSE,
                          .default = TRUE),
         bronze = case_when(str_detect(event_description, "Gold") ~ FALSE,
                            .default = TRUE)) |>
  # summarise(gold = sum(gold),
  #           silver = sum(silver),
  #           bronze = sum(bronze),
  #           .by = c(date, discipline_name)) |> 
  complete(date, discipline_name, fill = list(gold = 0, silver = 0, bronze = 0)) |> 
  arrange(date, discipline_name)

medals |> 
  filter(discipline_name == "Curling")

schedule |> 
  distinct(event_code)

schedule |> 
  rowid_to_column() |> 
  filter(is_medal_event == TRUE) |> 
  ggplot(aes(x = start_datetime_local, xend = end_datetime_local,
             y = discipline_name, yend = discipline_name,
             color = is_medal_event)) +
  geom_segment()
