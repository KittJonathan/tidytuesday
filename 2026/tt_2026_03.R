# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 3
# 2026-01-20
# Astronomy Picture Of the Day (APOD)

# Load packages ----

library(tidyverse)
library(tidytuesdayR)
# library(patchwork)

# Data ----

tuesdata <- tidytuesdayR::tt_load(2026, week = 3)
apod <- tuesdata$apod

apod <- apod |> 
  mutate(planet = case_when(str_detect(string = title, pattern = "Mercury") ~ "Mercury",
                            str_detect(string = title, pattern = "Venus") ~ "Venus",
                            str_detect(string = title, pattern = "Earth") ~ "Earth",
                            str_detect(string = title, pattern = "Mars") ~ "Mars",
                            str_detect(string = title, pattern = "Jupiter") ~ "Jupiter",
                            str_detect(string = title, pattern = "Saturn") ~ "Saturn",
                            str_detect(string = title, pattern = "Uranus") ~ "Uranus",
                            str_detect(string = title, pattern = "Neptune") ~ "Neptune",
                            .default = NA),
         planet = fct(planet, levels = c("Mercury", "Venus", "Earth", "Mars",
                                         "Jupiter", "Saturn", "Uranus", "Neptune"))
         ) |> 
  drop_na(planet) |> 
  select(date, planet) |> 
  mutate(year = year(date)) |> 
  summarise(total = n(), .by = c(year, planet))
         
# Plot ----

apod |> 
  filter(year >= 2021) |> 
  ggplot(aes(x = year, y = fct_rev(planet), fill = total)) +
  geom_point(aes(colour = total), size = 15)
  geom_tile(width = 0.5, height = 0.5, show.legend = FALSE) +
  geom_text(aes(label = total))
    
