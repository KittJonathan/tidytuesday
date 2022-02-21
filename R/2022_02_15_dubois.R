# TidyTuesday challenge
# Date : 2022-02-15
# DuBois Challenge
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-15/readme.md
# Load packages ----

library(lubridate)
library(showtext)
#library(tidytuesdayR)
library(tidyverse)
library(janitor)

# Import dataset ----

illiteracy <- read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv')

# Clean dataset ----

illiteracy <- illiteracy %>% 
  clean_names()

# Create plot ----

ggplot(data = illiteracy) +
  geom_col(mapping = aes(x = year,
                         y = rate))

ggplot(data = illiteracy) +
  geom_col(mapping = aes(x = rate,
                         y = year))


us_states <- tibble(
  state_name = state.name,
  state_abb = state.abb,
  centroid.x = state.center$x,
  centroid.y = state.center$y)

states_pilots <- airmen %>% 
  select(name, state) %>% 
  left_join(us_states, by = c("state" = "state_abb")) %>% 
  mutate(state_name = case_when(
    state == "In" ~ "Indiana",
    state == "DC" ~ "District Of Columbia",
    state == "CN" ~ "Connecticut",
    state == "KN" ~ "Kentucky",
    TRUE ~ state_name)) %>%
  filter(!is.na(state_name)) %>% 
  count(state_name, sort = TRUE) %>% 
  mutate(state_name = tolower(state_name))

# Create plot ----

font_add_google("Poiret One", "Poiret")
showtext_auto()

us_map <- map_data("state") %>% 
  left_join(states_pilots, 
            by = c("region" = "state_name"))

us_states <- us_states %>% 
  mutate(state_name = tolower(state_name))

top5_states <- states_pilots %>% 
  head(5) %>% 
  left_join(us_states, by = c("state_name" = "state_name"))

p <- ggplot() +
  geom_polygon(data = us_map,
               mapping = aes(x = long, y = lat, group = group, fill = n),
               colour = "grey50") +
  scale_fill_steps2(na.value = "white") +
  geom_text(data = top5_states,
            mapping = aes(x = centroid.x, y = centroid.y, label = state_abb),
            colour = "white", family = "Poiret", size = 15) +
  annotate("text", x = -125, y = 30, label = "41% of all pilots came from 5 states :",
           family = "Poiret", size = 12, hjust = 0) +
  annotate("text", x = -125, y = 29, label = "Illinois, New York, Pennsylvania, California & Ohio.",
           family = "Poiret", size = 12, hjust = 0) +
  ggtitle("Where did the Tuskegee Airmen come from in the U.S. ?",
          subtitle = "#TidyTuesday challenge | 2022-02-08 | #TuskegeeAirmenChallenge") + 
  theme_void() +
  theme(plot.title = element_text(family = "Poiret",
                                  hjust = 0.5,
                                  size = 50),
        plot.subtitle = element_text(family = "Poiret",
                                     hjust = 0.5,
                                     size = 35),
        legend.title = element_blank(),
        legend.margin = margin(r = 25),
        legend.text = element_text(family = "Poiret",
                                   size = 20),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"))

# Save plot ----

ggsave("figs/2022_02_08_tuskegee_airmen.png", p, dpi = 320, width = 12, height = 6)
