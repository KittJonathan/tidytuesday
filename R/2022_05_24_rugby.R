# TidyTuesday challenge
# Week : 21
# Date : 2022-05-24
# Women's rugby
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-24/readme.md

# Load packages ----

library(patchwork)
library(tidytuesdayR)
library(tidyverse)
library(showtext)
# library(ggtext)

# Import fonts ----

font_add_google(name = "Marvel", family = "marvel")
font_add_google(name = "Ranchers", family = "ranchers")
showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-05-24')
fifteens <- tuesdata$fifteens

# Data wrangling ----

six_nations <- fifteens %>% 
  filter(tournament %in% c("6 Nations", "5 Nations")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  select(year, team_1, team_2, score_1, score_2)

team_1_scores <- six_nations %>% 
  group_by(year, team_1) %>% 
  mutate(pts_f_1 = sum(score_1),
         pts_a_1 = sum(score_2)) %>% 
  select(year, team = team_1, pts_f_1, pts_a_1) %>% 
  filter(row_number() == 1)

team_2_scores <- six_nations %>% 
  group_by(year, team_2) %>% 
  mutate(pts_f_2 = sum(score_2),
         pts_a_2 = sum(score_1)) %>% 
  select(year, team = team_2, pts_f_2, pts_a_2) %>% 
  filter(row_number() == 1)

scores <- team_1_scores %>% 
  left_join(team_2_scores) %>% 
  mutate(pts_for = pts_f_1 + pts_f_2,
         pts_against = pts_a_1 + pts_a_2) %>% 
  mutate(pts_diff = pts_for - pts_against) %>% 
  select(year, team, pts_diff) %>%
  filter(year >= 2007)

england <- scores %>% 
  filter(team == "England")

france <- scores %>% 
  filter(team == "France")

wales <- scores %>% 
  filter(team == "Wales")

ireland <- scores %>% 
  filter(team == "Ireland")

italy <- scores %>% 
  filter(team == "Italy")

scotland <- scores %>% 
  filter(team == "Scotland")

# Create plot ----

p1 <- ggplot(data = england) +
  geom_line(aes(x = year, y = pts_diff),
             colour = "#b80d2f", size = 2) +
  scale_x_continuous(breaks = seq(2007, 2022, 5),
                     labels = seq(2007, 2022, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white", size = 0.25) +
  labs(title = "England") +
  ylim(c(-265, 265)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        plot.title = element_text(family = "ranchers", size = 75,
                                  colour = "#b80d2f", hjust = 0.5, margin = margin(t = 20)))

p2 <- ggplot(data = france) +
  geom_line(aes(x = year, y = pts_diff),
            colour = "#003567", size = 2) +
  scale_x_continuous(breaks = seq(2007, 2022, 5),
                     labels = seq(2007, 2022, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white", size = 0.25) +
  labs(title = "France") +
  ylim(c(-265, 265)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        plot.title = element_text(family = "ranchers", size = 75,
                                  colour = "#003567", hjust = 0.5, margin = margin(t = 20)))

p3 <- ggplot(data = wales) +
  geom_line(aes(x = year, y = pts_diff),
            colour = "#d8252e", size = 2) +
  scale_x_continuous(breaks = seq(2007, 2022, 5),
                     labels = seq(2007, 2022, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white", size = 0.25) +
  labs(title = "Wales") +
  ylim(c(-265, 265)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        plot.title = element_text(family = "ranchers", size = 75,
                                  colour = "#d8252e", hjust = 0.5, margin = margin(t = 20)))

p4 <- ggplot(data = ireland) +
  geom_line(aes(x = year, y = pts_diff),
            colour = "#006642", size = 2) +
  scale_x_continuous(breaks = seq(2007, 2022, 5),
                     labels = seq(2007, 2022, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white", size = 0.25) +
  labs(title = "Ireland") +
  ylim(c(-265, 265)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        plot.title = element_text(family = "ranchers", size = 75,
                                  colour = "#006642", hjust = 0.5, margin = margin(t = 20)))

p5 <- ggplot(data = italy) +
  geom_line(aes(x = year, y = pts_diff),
            colour = "#1460a8", size = 2) +
  scale_x_continuous(breaks = seq(2007, 2022, 5),
                     labels = seq(2007, 2022, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white", size = 0.25) +
  labs(title = "Italy") +
  ylim(c(-265, 265)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        plot.title = element_text(family = "ranchers", size = 75,
                                  colour = "#1460a8", hjust = 0.5, margin = margin(t = 20)))

p6 <- ggplot(data = scotland) +
  geom_line(aes(x = year, y = pts_diff),
            colour = "#144a74", size = 2) +
  scale_x_continuous(breaks = seq(2007, 2022, 5),
                     labels = seq(2007, 2022, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white", size = 0.25) +
  labs(title = "Scotland") +
  ylim(c(-265, 265)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        plot.title = element_text(family = "ranchers", size = 75,
                                  colour = "#144a74", hjust = 0.5, margin = margin(t = 20)))

p <- p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(ncol = 3) +
  plot_annotation(title = "Women's Six Nations",
                  subtitle = "Points differences since 2007",
                  caption = "Visualisation : Jonathan Kitt | Data source : ScrumQueens | #TidyTuesday 2022 week 21",
                  theme = theme(panel.background = element_rect(fill = "black", colour = "white"),
                                plot.background = element_rect(fill = "black", colour = "black"),
                                plot.title = element_text(family = "marvel", size = 125,
                                                          colour = "#b700fd", hjust = 0.5,
                                                          margin = margin(t = 10)),
                                plot.subtitle = element_text(family = "marvel", size = 100,
                                                             colour = "#b700fd", hjust = 0.5),
                                plot.caption = element_text(colour = "white", hjust = 0.5, size = 25)))

# Save plot ----

ggsave("figs/2022_05_24_rugby.png", p, dpi = 320, width = 12, height = 6)
