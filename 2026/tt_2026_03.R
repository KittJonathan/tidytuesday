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
                            .default = NA)
  ) |> 
  drop_na(planet)
         
apod |> 
  mutate(year = year(date)) |> 
  count(year, planet) |> 
  complete(year, planet, fill = list(n = 0)) |> 
  ggplot(aes(x = year, y = planet, fill = n)) +
  geom_point()
  

# Plot ----
