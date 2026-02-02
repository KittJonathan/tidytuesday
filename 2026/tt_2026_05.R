# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 5
# 2026-02-03
# Edible Plants Database

# Load packages ----

library(tidyverse)
library(tidytuesdayR)

# Data ----

tuesdata <- tidytuesdayR::tt_load(2026, week = 5)

edible_plants <- tuesdata$edible_plants |> 
  mutate(
    sunlight = case_when(sunlight == "partial shade" ~ "Partial shade",
                         sunlight == "full sun/partial shade/ full shade" ~ "Full sun/partial shade/full shade",
                         .default = sunlight),
    water = case_when(water == "high" ~ "High",
                      water == "Very low" ~ "Very Low",
                      water == "very high" ~ "Very High",
                      .default = water))

ggplot(edible_plants) +
  geom_point(aes(x = sunlight, y = water))

# Plot ----

ggplot(edible_plants) +
  geom_point(aes(x = sunlight,
                 y = temperature_germination))
