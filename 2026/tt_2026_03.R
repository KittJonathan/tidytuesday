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

p <- apod |> 
  filter(year >= 2021) |> 
  ggplot(aes(x = year, y = fct_rev(planet), fill = total)) +
  geom_point(aes(colour = total), size = 15,
             show.legend = FALSE) + 
  geom_text(aes(label = total), size = 10) +
  scale_colour_gradient2(low = "#008080", 
                       high = "#75dad7", midpoint = 12) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.title = element_blank(),
        axis.text = element_text(colour = "white"))
    
ggsave("2026/tt_2026_03.png", p, dpi = 320, height = 6, width = 12)
