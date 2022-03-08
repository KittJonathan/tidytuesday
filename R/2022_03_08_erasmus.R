# TidyTuesday challenge
# Week : 10
# Date : 2022-03-08
# Alternative Fuel Stations
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-01/readme.md

# Load packages ----

library(showtext)
library(tidytuesdayR)
library(tidyverse)
library(countrycode)

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tuesdata$erasmus  

rm(tuesdata)

# Load fonts ----

font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# Data wrangling ----

country_codes <- countrycode::codelist %>% 
  select(iso2c, country_name = country.name.en)

countries <- erasmus %>% 
  select(receiving_country_code, participants) %>% 
  left_join(country_codes, by = c("receiving_country_code" = "iso2c"))
  
  
  mutate(receiving_country = case_when(receiving_country_code == "AT" ~ "Austria",
                                       TRUE ~ receiving_country_code))

list_countries <- unique(countries$receiving_country_code)

erasmus %>% 
  filter(receiving_country_code == list_countries[1]) %>% 
  select(receiving_city) %>% 
  distinct()

head(countries)

us_states <- tibble(
  state_name = state.name,
  state_abb = state.abb)

stations <- stations %>% 
  filter(FUEL_TYPE_CODE == "ELEC",
         STATUS_CODE == "E") %>% 
  count(STATE) %>% 
  rename(state_abb = STATE, total = n) %>% 
  left_join(us_states) %>% 
  mutate(state_name = case_when(state_abb == "DC" ~ "District of Columbia",
                                TRUE ~ state_name)) %>% 
  mutate(state_name = tolower(state_name)) %>% 
  select(state_name, total) %>% 
  mutate(bin = case_when(total <= 1000 ~ "0-1000",
                         total > 1000 & total <= 2000 ~ "1000-2000",
                         total > 2000 & total <= 3000 ~ "2000-3000",
                         total > 3000 ~ "3000+"))

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
