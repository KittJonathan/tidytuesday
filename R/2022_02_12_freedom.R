# TidyTuesday challenge
# Date : 2022-02-12
# Freedom in the world
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-22/readme.md
# Load packages ----

#library(lubridate)
#library(showtext)
library(patchwork)
library(tidytuesdayR)
library(tidyverse)
#library(janitor)

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-02-22')
freedom <- tuesdata$freedom

rm(tuesdata)

illiteracy <- read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv')

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
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax,
                       fill = status)) +
  geom_rect(show.legend = FALSE) +
  scale_fill_manual(values = c("#0081C8", "antiquewhite")) +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void()

africa_ring <- continent_status %>% 
  filter(continent == "Africa") %>% 
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax,
                       fill = status)) +
  geom_rect(show.legend = FALSE) +
  scale_fill_manual(values = c("#000000", "antiquewhite")) +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void()

americas_ring <- continent_status %>% 
  filter(continent == "Americas") %>% 
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax,
                       fill = status)) +
  geom_rect(show.legend = FALSE) +
  scale_fill_manual(values = c("#EE334E", "antiquewhite")) +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void()

asia_ring <- continent_status %>% 
  filter(continent == "Asia") %>% 
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax,
                       fill = status)) +
  geom_rect(show.legend = FALSE) +
  scale_fill_manual(values = c("#FCB131", "antiquewhite")) +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void()

oceania_ring <- continent_status %>% 
  filter(continent == "Oceania") %>% 
  ggplot(mapping = aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax,
                       fill = status)) +
  geom_rect(show.legend = FALSE) +
  scale_fill_manual(values = c("#00A651", "antiquewhite")) +
  coord_polar(theta = "y") +
  xlim(c(0.05, 4)) +
  theme_void()

(europe_ring + africa_ring + americas_ring) / (asia_ring + oceania_ring)

 # Save plot ----

ggsave("figs/2022_02_08_tuskegee_airmen.png", p, dpi = 320, width = 12, height = 6)
