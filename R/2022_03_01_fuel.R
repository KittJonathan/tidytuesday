# TidyTuesday challenge
# Week : 9
# Date : 2022-03-01
# Alternative Fuel Stations
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-01/readme.md

# Load packages ----

#library(patchwork)
library(janitor)
library(showtext)
library(tidytuesdayR)
library(tidyverse)

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-03-01')
stations <- tuesdata$stations

rm(tuesdata)

# Load fonts ----

font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# Clean dataset ----

us_states <- tibble(
  state_name = state.name,
  state_abb = state.abb)

d1 <- stations %>% 
  filter(STATUS_CODE == "E") %>% 
  count(STATE, FUEL_TYPE_CODE) %>% 
  rename(state_abb = STATE, fuel_type = FUEL_TYPE_CODE, total = n) %>% 
  mutate(fuel_type = case_when(fuel_type == "BD" ~ "Biodiesel",
                               fuel_type == "CNG" ~ "Compressed Natural Gas",
                               fuel_type == "ELEC" ~ "Electric",
                               fuel_type == "E85" ~ "Ethanol",
                               fuel_type == "HY" ~ "Hydrogen",
                               fuel_type == "LNG" ~ "Liquified Natural Gas",
                               fuel_type == "LPG" ~ "Propane")) %>% 
  left_join(us_states) %>% 
  mutate(state_name = case_when(state_abb == "DC" ~ "District of Columbia",
                                TRUE ~ state_name)) %>% 
  mutate(state_name = tolower(state_name)) %>% 
  select(state_name, fuel_type, total)

# Create map ----

us_stations <- map_data("state") %>% 
  left_join(stations, by = c("region" = "state_name"))

ggplot(data = us_electric_stations,
       mapping = aes(x = long, y = lat, group = group,
                     fill = total)) +
  geom_polygon()

  geom_point(data = d1,
             mapping = aes(x = longitude, y = latitude,
                           colour = fuel_type))

ggplot(data = us_map,
       mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = 'white')

%>% 
  left_join(states_pilots, 
            by = c("region" = "state_name"))

us_states <- tibble(
  state_name = state.name,
  state_abb = state.abb,
  centroid.x = state.center$x,
  centroid.y = state.center$y)

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
