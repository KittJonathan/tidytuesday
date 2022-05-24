# TidyTuesday challenge
# Week : 21
# Date : 2022-05-24
# Women's rugby
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-05-24/readme.md

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
# library(showtext)
# library(ggtext)

# Import fonts ----

# font_add_google(name = "Nova Flat", family = "nova")
# font_add_google(name = "Akronim", family = "akronim")
# showtext_auto()

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

summary(scores$pts_against)

ggplot(data = scores) +
  geom_line(aes(x = year, y = pts_diff, colour = team)) +
  ylim(c(-265, 265))

ggplot(data = england) +
  geom_point(aes(x = year, y = pts_for), colour = "green") +
  geom_point(aes(x = year, y = -pts_against), colour = "red") +
  geom_line(aes(x = year, y = pts_for), colour = "green") +
  geom_line(aes(x = year, y = -pts_against), colour = "red") +
  ylim(c(-285, 285))

ggplot(data = england) +
  geom_line(aes(x = year, y = pts_for), colour = "green") +
  geom_line(aes(x = year, y = pts_against), colour = "red") +
  ylim(c(-255, 285))

  geom_line(aes(x = year, y = pts_diff, colour = team))

p <- ggplot() +
  geom_point(data = d1,
            mapping = aes(x = year, y = country, fill = rank, colour = rank),
            size = 3, shape = 21,
            show.legend = FALSE) +
  scale_fill_manual(values = c("#fee101", "#a7a7ad", "#a77044")) +
  scale_colour_manual(values = c("#fee101", "#a7a7ad", "#a77044")) +
  labs(
    x = "",
    y = "",
    title = "**EUROVISION CONTEST**",
    subtitle = "Countries ranking
    <span style='color:#fee101;'>**1st**</span>, 
    <span style='color:#a7a7ad;'>**2nd**</span>, and
    <span style='color:#a77044;'>**3rd**</span>
    from 1973 to 2022.
    </span>",
    caption = "Visualisation : Jonathan Kitt | Data source : Eurovision | #TidyTuesday 2022 week 20") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#58508d", colour = "#58508d"),
        plot.background = element_rect(fill = "#58508d", colour = "#58508d"),
        panel.grid.minor.x = element_line(size = 0.05),
        panel.grid.major.x = element_line(size = 0.05),
        panel.grid.major.y = element_line(size = 0.05),
        plot.title = element_markdown(family = "akronim", size = 100, colour = "white", hjust = 0.5,
                                      margin = margin(t = 10)),
        plot.subtitle = element_markdown(family = "akronim", size = 60, colour = "white", hjust = 0.5,
                                         margin = margin(b = 10)),
        plot.caption = element_text(colour = "white", size = 25, hjust = 0.5),
        axis.text = element_text(family = "nova", colour = "white", size = 25))

# Save plot ----

ggsave("figs/2022_05_17_eurovision.png", p, dpi = 320, width = 12, height = 6)
