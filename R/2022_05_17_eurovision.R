# TidyTuesday challenge
# Week : 20
# Date : 2022-05-17
# Eurovision
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-17/readme.md

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)

# Import fonts ----

font_add_google(name = "Nova Flat", family = "nova")
font_add_google(name = "Akronim", family = "akronim")
showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-05-17')
eurovision <- tuesdata$eurovision

# Data wrangling ----

d1 <- eurovision %>%
  filter(section %in% c("grand-final", "final"),
         rank %in% 1:3,
         year >= 1973) %>% 
  select(year, country = artist_country, rank) %>% 
  mutate(rank = factor(rank),
         country = forcats::fct_rev(country))

# Create plot ----

p <- ggplot() +
  geom_point(data = d1,
            mapping = aes(x = year, y = country, fill = rank, colour = rank),
            size = 3, shape = 21,
            show.legend = FALSE) +
  scale_fill_manual(values = c("#fee101", "#a7a7ad", "#a77044")) +
  scale_colour_manual(values = c("#fee101", "#a7a7ad", "#a77044")) +
  labs(
    x = "",
    y = "",
    title = "**EUROVISION CONTEST**",
    subtitle = "Countries ranking
    <span style='color:#fee101;'>**1st**</span>, 
    <span style='color:#a7a7ad;'>**2nd**</span>, and
    <span style='color:#a77044;'>**3rd**</span>
    from 1973 to 2022.
    </span>",
    caption = "Visualisation : Jonathan Kitt | Data source : Eurovision | #TidyTuesday 2022 week 20") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#58508d", colour = "#58508d"),
        plot.background = element_rect(fill = "#58508d", colour = "#58508d"),
        panel.grid.minor.x = element_line(size = 0.05),
        panel.grid.major.x = element_line(size = 0.05),
        panel.grid.major.y = element_line(size = 0.05),
        plot.title = element_markdown(family = "akronim", size = 100, colour = "white", hjust = 0.5,
                                      margin = margin(t = 10)),
        plot.subtitle = element_markdown(family = "akronim", size = 60, colour = "white", hjust = 0.5,
                                         margin = margin(b = 10)),
        plot.caption = element_text(colour = "white", size = 25, hjust = 0.5),
        axis.text = element_text(family = "nova", colour = "white", size = 25))

# Save plot ----

ggsave("figs/2022_05_17_eurovision.png", p, dpi = 320, width = 12, height = 6)
