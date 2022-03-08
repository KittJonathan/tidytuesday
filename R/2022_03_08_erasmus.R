# TidyTuesday challenge
# Week : 10
# Date : 2022-03-08
# Alternative Fuel Stations
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-01/readme.md

# Load packages ----

library(showtext)
library(tidytuesdayR)
library(tidyverse)
library(ggflags)
# library(countrycode)

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tuesdata$erasmus  

rm(tuesdata)

# Load fonts ----

font_add_google(name = "Comfortaa", family = "Comfortaa")
showtext_auto()

# Data wrangling ----

country_codes <- countrycode::codelist %>%
  select(iso2c, country_name = country.name.en)

d1 <- erasmus %>% 
  filter(participant_nationality == "FR",  # keep data for french students
         receiving_country_code != "FR") %>%  # keep mobilities abroad
  select(receiving_country_code, participants) %>%   # remove unwanted columns
  left_join(country_codes, by = c("receiving_country_code" = "iso2c")) %>%  # add sending country name
  mutate(country_name = case_when(receiving_country_code == "EL" ~ "Greece",  # add missing country names 
                                  receiving_country_code == "UK" ~ "United Kingdom",
                                  receiving_country_code == "CZ" ~ "Czech Republic",
                                  TRUE ~ country_name)) %>% 
  group_by(country_name) %>%  # group data by country name
  mutate(total = sum(participants)) %>%   # count total number of participants for each receiving country
  filter(row_number() == 1) %>%  # keep 1 row by receiving country
  arrange(desc(total)) %>%   # arrange data by descending order
  ungroup() %>%  # ungroup data
  mutate(percent = 100 * total / sum(total)) %>%   # calculate ratio
  mutate(country_name = factor(country_name, levels = rev(country_name))) %>%  # set levels %>% 
  head(10)  # keep top 10 destinations

# Create plot ----

p <- ggplot(data = d1,
       aes(x = country_name, y = total, fill = country_name)) +
  geom_bar(width = 0.9, stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = rep(c("#3caea3", "#173f5f"), 5)) +
  #scale_fill_manual(values = rev(rainbow(10))) +
  # scale_fill_manual(values = c("#9999ff", "#7d7fe2", "#6066c6", "#444eaa", "#23388f",
  #                              "#002275", "#000f5c", "#000044", "#00032c", "#000117")) +
  coord_polar(theta = "y", start = 0) +
  xlab("") +
  ylab("") +
  geom_text(aes(x = country_name, y = 0, label = paste0(country_name, " - ", round(percent, digits = 1), " %")),
            hjust = 1.05, family = "Comfortaa", size = 10, colour = rep(c("#173f5f", "#3caea3"), 5)) +
  ggtitle(label = "Where do french students prefer to go ?",
          subtitle = "Top 10 destinations for french ERASMUS participants") +
  ylim(c(0, 250)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#d6ecef", colour = "#d6ecef"),
        panel.background = element_rect(fill = "#d6ecef", colour = "#d6ecef"),
        plot.title = element_text(family = "Comfortaa", size = 60, colour = "#173f5f", hjust = 0.5,
                                  margin = margin(t = 20)),
        plot.subtitle = element_text(family = "Comfortaa", size = 30, colour = "#173f5f", hjust = 0.5))

# Save plot ----

ggsave("figs/2022_03_08_erasmus.png", p, dpi = 320, width = 12, height = 6)

# Create map ----

us_elec_stations <- map_data("state") %>% 
  left_join(stations, by = c("region" = "state_name"))

us_map <- ggplot(data = us_elec_stations,
       mapping = aes(x = long, y = lat, group = group,
                     fill = bin)) +
  geom_polygon(colour = "grey30") +
  ggtitle("Electric car charging stations in the U.S.") +
  scale_fill_manual(values = c("#f3e9d2", "#88d498", "#1a936f", "#114b5f")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#c2c8c5"),
        plot.title = element_text(family = "space", size = 45, hjust = 0.5,
                                  margin = margin(t = 20)),
        legend.title = element_blank(),
        legend.text = element_text(family = "space", size = 25),
        legend.margin = margin(r = 20))

# Save plot ----

ggsave("figs/2022_03_01_fuel.png", us_map, dpi = 320, width = 12, height = 6)
