# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 3
# 2026-01-20
# Astronomy Picture Of the Day (APOD)

# Load packages ----

library(tidyverse)
library(tidytuesdayR)
library(sysfonts)

font_add_google("Roboto")
font_add_google("Orbitron")
showtext::showtext_auto()
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
  summarise(total = n(), .by = c(year, planet)) |> 
  filter(year >= 2021)
         
# Plot ----

p <- ggplot(apod) +
  geom_point(aes(year, planet, colour = planet), 
             size = 10, show.legend = FALSE) +
  geom_text(aes(x = year, y = planet, label = total),
            size = 10, family = "Orbitron") +
  scale_colour_manual(values = c("#e2dbd8", "#d7c299", "#01c1dc", "#c24417",
                                 "#e0946d", "#865328", "#a5cbf0", "#6094d3")) +
  coord_radial(start = -0.2*pi, end = 0.2*pi, inner.radius = 0.5) +
  labs(title = "Planets of the solar system",
       subtitle = "Number of times each planet was featured as Astronomy Picture Of the Day",
       caption = "#TidyTuesday 2026 W03 | Jonathan Kitt") +
  theme(text = element_text(family = "Orbitron"),
        axis.text = element_text(size = 25, colour = "white"),
        axis.ticks = element_blank(),
        panel.background = element_rect(colour = "black", fill = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2, linetype = "dotted",
                                        colour = "white"),
        plot.background = element_rect(colour = "black", fill = "black"),
        plot.title = element_text(colour = "white", face = "bold",
                                  size = 50, hjust = 0.5,
                                  margin = margin(t = 15, b = 5)),
        plot.subtitle = element_text(colour = "white",
                                     size = 40, hjust = 0.5,
                                     margin = margin(b = -50)),
        plot.caption = element_text(colour = "white",
                                    size = 25, hjust = 0.5,
                                    margin = margin(b = 10)))

ggsave("2026/tt_2026_03.png", p, dpi = 320, height = 6, width = 12)