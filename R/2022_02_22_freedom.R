# TidyTuesday challenge
# Date : 2022-02-22
# Freedom in the world
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-22/readme.md

# Load packages ----

library(patchwork)
library(showtext)
library(tidytuesdayR)
library(tidyverse)

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-02-22')
freedom <- tuesdata$freedom

rm(tuesdata)

# Load fonts ----

font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# Number of "free" countries in 2020 per continent ----

continent_status <- freedom %>% 
  filter(year == 2020) %>% 
  select(continent = Region_Name, status = Status) %>% 
  group_by(continent) %>% 
  mutate(free_countries = sum(status == "F"),
         total = n()) %>% 
  filter(row_number() == 1) %>% 
  select(continent, free_countries, total) %>% 
  mutate(free_fraction = free_countries / total) %>% 
  mutate(non_free_fraction = 1 - free_fraction) %>% 
  select(continent, free_fraction, non_free_fraction) %>% 
  pivot_longer(-continent, names_to = "status", values_to = "fraction") %>% 
  mutate(status = str_remove(status, "_fraction")) %>% 
  group_by(continent) %>% 
  mutate(ymin = case_when(status == "non_free" ~ 1 - fraction,
                          TRUE ~ 0),
         ymax = case_when(status == "free" ~ fraction,
                          TRUE ~ 1))


# Plots ----

europe_ring <- continent_status %>% 
  filter(continent == "Europe") %>% 
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, alpha = rev(status))) +
  geom_rect(show.legend = FALSE, fill = "#0081C8") +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void() +
  annotate("text", x = 0.05, y = 0, size = 15, label = "Europe", family = "space",
           colour = "#0081C8")

africa_ring <- continent_status %>% 
  filter(continent == "Africa") %>% 
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, alpha = rev(status))) +
  geom_rect(show.legend = FALSE, fill = "#000000") +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void() +
  annotate("text", x = 0.05, y = 0, size = 15, label = "Africa", family = "space",
           colour = "#000000")

americas_ring <- continent_status %>% 
  filter(continent == "Americas") %>% 
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, alpha = rev(status))) +
  geom_rect(show.legend = FALSE, fill = "#EE334E") +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void() +
  annotate("text", x = 0.05, y = 0, size = 15, label = "Americas", family = "space",
           colour = "#EE334E")

asia_ring <- continent_status %>% 
  filter(continent == "Asia") %>% 
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, alpha = rev(status))) +
  geom_rect(show.legend = FALSE, fill = "#FCB131") +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void() +
  annotate("text", x = 0.05, y = 0, size = 15, label = "Asia", family = "space",
           colour = "#FCB131")

oceania_ring <- continent_status %>% 
  filter(continent == "Oceania") %>% 
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, alpha = rev(status))) +
  geom_rect(show.legend = FALSE, fill = "#00A651") +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void() +
  annotate("text", x = 0.05, y = 0, size = 15, label = "Oceania", family = "space",
           colour = "#00A651")

p <- (europe_ring + africa_ring + americas_ring) / (asia_ring + oceania_ring) +
  plot_layout(widths = c(1, 1, 1, 1, 1)) +
  plot_annotation(title = "Freedom in the world",
                  subtitle = "Proportion of free countries by continent in 2020",
                  caption = "Data : Freedom House | Datavis : Jonathan Kitt",
                  theme = theme(plot.title = element_text(hjust = 0.5, family = "space", size = 60),
                                plot.subtitle = element_text(hjust = 0.5, family = "space", size = 50),
                                plot.caption = element_text(hjust = 0.5, family = "space", size = 20)))

 # Save plot ----

ggsave("figs/2022_02_22_freedom.png", p, dpi = 320, width = 12, height = 6)
