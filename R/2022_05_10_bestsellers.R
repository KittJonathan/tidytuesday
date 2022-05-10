# TidyTuesday challenge
# Week : 19
# Date : 2022-05-10
# NY Times bestsellers
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-10/readme.md

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(showtext)

# Import fonts ----

font_add_google(name = "Henny Penny", family = "Henny Penny")
font_add_google(name = "Quando", family = "Quando")
showtext_auto()

# Import dataset ----

nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')

# Data wrangling ----

d1 <- nyt_titles %>% 
  filter(year >= 2016) %>% 
  group_by(year) %>% 
  arrange(desc(total_weeks), best_rank) %>% 
  slice(1:5) %>% 
  mutate(cumul_weeks = cumsum(total_weeks)) %>% 
  mutate(lag.value = dplyr::lag(cumul_weeks, n = 1, default = 0)) %>% 
  mutate(idx = factor(1:5)) %>% 
  select(year, title, author, x.start = lag.value, x.end = cumul_weeks, idx)

# Create plot ----

p <- ggplot(data = d1) +
  geom_rect(aes(xmin = x.start, xmax = x.end, 
                ymin = year, ymax = year + 0.7,
                fill = idx, colour = NULL), show.legend = FALSE) +
  scale_fill_manual(values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")) +
  geom_rect(aes(xmin = 100, xmax = 120,
                ymin = 2020.8, ymax = 2021), fill = "white") +
  geom_text(aes(x = 110, y = 2020.9, label = "2020"), colour = "#004c6d", size = 8, family = "Quando") +
  geom_rect(aes(xmin = 100, xmax = 120,
                ymin = 2019.8, ymax = 2020), fill = "white") +
  geom_text(aes(x = 110, y = 2019.9, label = "2019"), colour = "#004c6d", size = 8, family = "Quando") +
  geom_rect(aes(xmin = 100, xmax = 120,
                ymin = 2018.8, ymax = 2019), fill = "white") +
  geom_text(aes(x = 110, y = 2018.9, label = "2018"), colour = "#004c6d", size = 8, family = "Quando") +
  geom_rect(aes(xmin = 100, xmax = 120,
                ymin = 2017.8, ymax = 2018), fill = "white") +
  geom_text(aes(x = 110, y = 2017.9, label = "2017"), colour = "#004c6d", size = 8, family = "Quando") +
  geom_rect(aes(xmin = 100, xmax = 120,
                ymin = 2016.8, ymax = 2017), fill = "white") +
  geom_text(aes(x = 110, y = 2016.9, label = "2016"), colour = "#004c6d", size = 8, family = "Quando") +
  geom_rect(aes(xmin = 0, xmax = 220,
                ymin = 2016, ymax = 2021),
            fill = NA, colour = "white", size = 1.5) +
  geom_segment(aes(x = 0, xend = 220,
                   y = year, yend = year),
               size = 1.5, colour = "white") +
  geom_text(aes(x = x.start + ((x.end - x.start)/2), y = year + 0.45, label = title),
            size = 6.5, family = "Quando", colour = "white") +
  geom_text(aes(x = x.start + ((x.end - x.start)/2), y = year + 0.25, label = author),
            size = 6, family = "Quando", colour = "white") +
  labs(title = "New York Times top 5 best sellers (2016-2020)",
       subtitle = "width represents total weeks on the best sellers list",
       caption = "Visualisation: Jonathan Kitt | Data source: Post 45 | #TidyTuesday 2022 Week 19") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#9dc6e0", colour = "#9dc6e0"),
        plot.background = element_rect(fill = "#9dc6e0", colour = "#9dc6e0"),
        plot.title = element_text(family = "Henny Penny", colour = "#004c6d", hjust = 0.5, size = 50,
                                  margin = margin(t = 20)),
        plot.subtitle = element_text(family = "Henny Penny", colour = "#004c6d", hjust = 0.5, size = 35),
        plot.caption = element_text(colour = "#004c6d", hjust = 0.5, size = 20, margin = margin(b = 10)))

# Save plot ----

ggsave("figs/2022_05_10_bestsellers.png", p, dpi = 320, width = 12, height = 6)