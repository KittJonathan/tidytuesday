# TidyTuesday challenge
# Week : 10
# Date : 2022-03-08
# EU Student mobility
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-08/readme.md

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(showtext)

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
  coord_polar(theta = "y", start = 0) +
  xlab("") +
  ylab("") +
  labs(caption = "Visualisation : Jonathan Kitt | Data source : Data.Europa | #TidyTuesday 2022 week 10") +
  geom_text(aes(x = country_name, y = 0, label = paste0(country_name, " - ", round(percent, digits = 1), " %")),
            hjust = 1.05, family = "Comfortaa", size = 10, colour = rep(c("#173f5f", "#3caea3"), 5)) +
  ggtitle(label = "Where do french students prefer to go ?",
          subtitle = "Top 10 destinations for french ERASMUS participants (2014-2020") +
  ylim(c(0, 250)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#d6ecef", colour = "#d6ecef"),
        panel.background = element_rect(fill = "#d6ecef", colour = "#d6ecef"),
        plot.title = element_text(family = "Comfortaa", size = 60, colour = "#173f5f", hjust = 0.5,
                                  margin = margin(t = 20)),
        plot.subtitle = element_text(family = "Comfortaa", size = 30, colour = "#173f5f", hjust = 0.5),
        plot.caption = element_text(colour = "#173f5f", size = 20, hjust = 0.5,
                                    margin = margin(b = 20)))

# Save plot ----

ggsave("figs/2022_03_08_erasmus.png", p, dpi = 320, width = 12, height = 6)