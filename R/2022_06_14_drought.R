# TidyTuesday challenge
# Week : 24
# Date : 2022-06-14
# Drought conditions in the US
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-06-14

# Load packages ----

library(showtext)
library(tidytuesdayR)
library(tidyverse)

# Import fonts ----

font_add_google(name = "Bebas Neue", family = "bebas")
font_add_google(name = "Teko", family = "teko")
font_add_google(name = "Righteous", family = "righteous")
showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-06-14')

drought <- tuesdata$drought

# Data wrangling ----

d1 <- drought %>% 
  mutate(date = str_remove(DATE, "d_"),
         ymd = lubridate::ymd(date),
         year = lubridate::year(ymd)) %>% 
  select(year, D0:D4, W0:W4) %>% 
  pivot_longer(cols = -year, names_to = "condition", values_to = "value") %>% 
  group_by(year, condition) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% 
  mutate(mean_value = case_when(condition %in% c("W0", "W1", "W2", "W3", "W4") ~ -mean_value,
                                TRUE ~ mean_value))

# Create plot ----

p <- ggplot(data = filter(d1, year >= 2000),
       mapping = aes(x = year, y = mean_value, fill = condition)) +
  geom_area(show.legend = FALSE) +
  scale_fill_manual(values = c("#ecca00", "#ec9b00", "#ec5300", "#ec2400", "#ec0000",
                               "#b3cde0", "#6497b1", "#005b96", "#03396c", "#011f4b")) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(title = "Drought conditions in the U.S. (2000-2022)",
       subtitle = "Darker colours indicate more severe contitions",
       caption = "Visualisation : Jonathan Kitt | Data source : National Integrated Drought Information System | #TidyTuesday 2022 week 24") +
  geom_text(aes(x = 2002.25, y = 130, label = "2002 North-American drought"),
            family = "righteous", colour = "#ecca00", hjust = 0, size = 15) +
  geom_text(aes(x = 2018.5, y = -225, label = "2019 was the second wettest"),
            family = "righteous", colour = "#b3cde0", hjust = 1, size = 15) +
  geom_text(aes(x = 2018.5, y = -238, label = "year on record behind 1973"),
            family = "righteous", colour = "#b3cde0", hjust = 1, size = 15) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#969892", colour = "#969892"),
        plot.background = element_rect(fill = "#969892", colour = "#969892"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "bebas", colour = "white", size = 100, hjust = 0.5,
                                  margin = margin(t = 10)),
        plot.subtitle = element_text(family = "teko", colour = "white", size = 75, hjust = 0.5,
                                  margin = margin(t = 10, b = 5)),
        plot.caption = element_text(colour = "white", size = 25, hjust = 0.5,
                                    margin = margin(t = 5, b = 5)))

# Save plot ----

ggsave("figs/2022_06_14_drought.png", p2, dpi = 320, width = 12, height = 6)
