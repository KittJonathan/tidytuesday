# TidyTuesday challenge
# 2026-W36
# 2026-06-30
# Wreck Inventory of Ireland

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-06-30/readme.md

# Packages ----

library(tidyverse)
library(maps)
library(ggauto)

theme_set(theme_bw())

# Data ----

wreck_inventory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-30/wreck_inventory.csv')

# Explore data ----

glimpse(wreck_inventory)

wreck_inventory |> 
  drop_na(date, latitude, longitude)
  

wrecks <- wreck_inventory |> 
  drop_na(date, longitude, latitude) |> 
  filter(year < 1950)

# Create map ----

worldmap <- map_data(map = "world")

ireland <- worldmap |> 
  filter(region == "Ireland" | (region == "UK" & subregion == "Northern Ireland"))

ggplot() +
  geom_polygon(data = ireland, aes(x = long, y = lat, group = group),
               fill = "blue", color = "blue") +
  geom_point(data = wrecks, 
             aes(x = longitude, y = latitude, color = as.factor(year)),
             alpha = 0.2) +
  coord_fixed()

wrecks |> 
  filter(year >= 1926) |> 
  summarise(total = n(), .by = year) |>
  ggplot() +
  geom_line(aes(x = year, y = total))

wreck_inventory |> 
  drop_na(date, year, longitude, latitude) |> 
  count(year) |> 
  arrange(year)
