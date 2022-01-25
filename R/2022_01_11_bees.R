# TidyTuesday challenge
# Date : 2022-01-11
# Bee colonies
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(showtext)

# Import fonts ----

font_add_google("Poiret One", "Poiret")
showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-01-11')
colony <- tuesdata$colony
rm(tuesdata)

# Hex-shaped heatmap of colony loss pct ----

colony_loss <- colony %>% 
  select(state, year, months, colony_lost_pct) %>% 
  mutate(year = factor(year,
                       levels = unique(year)),
         months = factor(months,
                         levels = unique(months))) %>% 
  arrange(state, year, months) %>% 
  mutate(months_nbs = case_when(months == "January-March" ~ "01",
                                months == "April-June" ~ "04",
                                months == "July-September" ~ "07",
                                months == "October-December" ~ "10")) %>% 
  mutate(date = paste(year, months_nbs, sep = "-")) %>% 
  select(state, date, colony_lost_pct) %>% 
  mutate(state = factor(state, levels = rev(sort(unique(state))))) %>% 
  mutate(bin = case_when(colony_lost_pct >= 0 & colony_lost_pct < 10 ~ "0-10",
                         colony_lost_pct >= 10 & colony_lost_pct < 20 ~ "10-20",
                         colony_lost_pct >= 20 & colony_lost_pct < 30 ~ "20-30",
                         colony_lost_pct >= 30 & colony_lost_pct < 40 ~ "30-40",
                         colony_lost_pct >= 40 & colony_lost_pct < 50 ~ "40-50",
                         colony_lost_pct >= 50 ~ "50+"))

p <- ggplot(data = colony_loss,
       mapping = aes(x = date, y = rev(state), fill = bin)) +
  geom_tile(colour = "black") +
  labs(x = "", y = "") +
  ggtitle("Bee colony loss % in U.S. States") +
  scale_fill_manual(values = c("#FFF1DA", "#FED781", "#FFB800",
                               "#EB7D02", "#C75D00", "#6F3C03"),
                    na.value = "grey90",
                    guide = guide_legend(label.position = "left",
                                         nrow = 1, keywidth = 2)) +
  theme_minimal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(family = "Poiret", size = 25, colour = "black",
                                   margin = margin(r = -0.4, unit = "cm")),
        legend.spacing.x = unit(0.5, "cm"),
        plot.background = element_rect(fill = "gold"),
        plot.title = element_text(family = "Poiret", hjust = 0.5, size = 50,
                                  margin = margin(10, 0, 10, 0)),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text.x  = element_text(family = "Poiret", size = 20, colour = "black"),
        axis.text.y = element_text(family = "Poiret", size = 25, colour = "black"))

ggsave("figs/2022_01_11_bees.png", p, dpi = 320, width = 12, height = 6) 
