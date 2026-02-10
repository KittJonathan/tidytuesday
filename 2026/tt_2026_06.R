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

# Prepare the data ----

medals <- schedule |> 
  filter(is_medal_event == TRUE | str_detect(event_description, "Medal")) |> 
  distinct(discipline_name, event_description, start_datetime_local, end_datetime_local) |>
  mutate(gold = case_when(str_detect(event_description, "Bronze") ~ 0,
                          .default = 1),
         silver = case_when(str_detect(event_description, "Bronze") ~ 0,
                            .default = 1),
         bronze = case_when(str_detect(event_description, "Gold") ~ 0,
                            .default = 1),
         date = date(start_datetime_local)) |> 
  select(date, discipline_name, gold, silver, bronze) |> 
  summarise(gold = sum(gold),
            silver = sum(silver),
            bronze = sum(bronze),
            .by = c(date, discipline_name)) |> 
  complete(date, discipline_name, fill = list(gold = 0, silver = 0, bronze = 0))

total_medals <- medals |> 
  summarise(gold = sum(gold),
            silver = sum(silver),
            bronze = sum(bronze),
            .by = discipline_name) |> 
  arrange(gold)

medals <- medals |> 
  mutate(discipline_name = factor(discipline_name, 
                                  levels = total_medals$discipline_name)) |> 
  mutate(id = consecutive_id(date), .before = date)



# Plot ----

p <- medals |> 
  mutate(gold_x = id - 0.4, silver_x = id, bronze_x = id + 0.4) |> 
  filter(gold + silver + bronze != 0) |> 
  ggplot() +
  geom_point(aes(x = bronze_x, y = discipline_name),
             col = "#A77044", size = 6) +
  geom_point(aes(x = silver_x, y = discipline_name),
             col = "#D7D7D7", size = 6) +
  geom_point(aes(x = gold_x, y = discipline_name),
             col = "#FEE101", size = 6)

# p <- medals |> 
#   ggplot() +
#   geom_vline(xintercept = seq(0.5, 16.5, 1)) +
#   geom_segment(aes(x = 0.5, xend = 16.5, y = 1, yend = 16)) +
#   geom_point(aes(x = id + 0.1, y = discipline_name), 
#              fill = "#A77044", show.legend = FALSE, size = 10, shape = 21,
#              color = "white") +
#   geom_point(aes(x = id, y = discipline_name), 
#              fill = "#D7D7D7", show.legend = FALSE, size = 10, shape = 21,
#              color = "white") +
#   geom_point(aes(x = id - 0.1, y = discipline_name), 
#              fill = "#FEE101", show.legend = FALSE, size = 10, shape = 21,
#              color = "white") +
#   geom_text(aes(x = id - 0.1, y = discipline_name, label = gold))

ggsave("2026/tt_2026_06.png", p, dpi = 320, width = 12, height = 6)

# Plot ----

ggplot() +
  geom_point(data = medals, aes(x = date, y = discipline_name,
                                col = gold))

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
