# TidyTuesday challenge
# Week : 11
# Date : 2022-03-15
# CRAN/BIOC vignettes
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-15/readme.md

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(showtext)

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-03-15')

# cran <- tuesdata$cran
bioc <- tuesdata$bioc

rm(tuesdata)

# Load fonts ----

font_add_google(name = "Comfortaa", family = "Comfortaa")
showtext_auto()

# Data wrangling ----

head(bioc)

new_pkgs_years <- bioc %>% 
  mutate(year = year(date)) %>% 
  group_by(package) %>% 
  mutate(first_release = min(year)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  count(first_release) %>% 
  filter(first_release > 1970)

time_of_day <- bioc %>% 
  mutate(hour = hour(date))

pkgs_am <- time_of_day %>% 
  filter(hour < 12) %>% 
  count(hour) %>% 
  mutate(hour = fct_inseq(factor(hour)))

pkgs_pm <- time_of_day %>% 
  filter(hour >= 12) %>% 
  count(hour) %>% 
  mutate(hour = fct_inseq(factor(hour)))

ggplot() +
  geom_col(data = pkgs_am,
           aes(x = hour, y = "", fill = n),
           width = 1) +
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 0.5),
               arrow = arrow(length = unit(0.5, "cm"))) +
  coord_polar(theta = "x", start = 0)

ggplot() +
  geom_col(data = pkgs_pm,
           aes(x = hour, y = "", fill = n),
           width = 1) +
  geom_segment(aes(x = 4, xend = 4, y = 0, yend = 0.5),
               arrow = arrow(length = unit(0.5, "cm"))) +
  coord_polar(theta = "x", start = 0)
  
  
  scale_x_continuous(breaks = seq(0, 11, 1))

  geom_label(aes(x = hour, label = hour, hjust = 0)) +

  # geom_bar(stat = "identity") +
  coord_polar(theta = "x", start = 0)

df <- data.frame(x = seq(0,359,20),y = 1)

ggplot(df, aes(x+10,y, hjust=1)) +
  geom_col(colour = "black",fill = "grey") +
  geom_label(aes(x=x+5,label = x)) + 
  scale_x_continuous(breaks = c(0,90,180,270),limits = c(0,360)) +
  coord_polar() 

time_of_day %>% count(hour, sort = T)

plot(bioc_years$first_release, bioc_years$n, type = "b")

cran %>% 
  mutate(month = month(date))

d1 <- cran %>% 
  mutate(ymd = as_date(date))



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