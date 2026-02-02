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
edible_plants <- tuesdata$edible_plants

edible_plants |> 
  summarise(min_temp = min(temperature_germination),
            max_temp = max(temperature_germination),
            .by = cultivation)

# Plot ----

ggplot(edible_plants) +
  geom_segment(aes(x = preferred_ph_lower, xend = preferred_ph_upper,
                   y = taxonomic_name, yend = taxonomic_name,
                   color = sunlight))
