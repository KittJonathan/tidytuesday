# TidyTuesday challenge
# Week : 12
# Date : 2022-03-22
# Baby names
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-22/readme.md

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
# library(lubridate)
library(showtext)
library(patchwork)

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-03-22')

babynames <- tuesdata$babynames

rm(tuesdata)

# Load fonts ----

font_add_google(name = "Kranky", family = "Kranky")
showtext_auto()

# Data wrangling ----

d1 <- babynames %>% 
  mutate(code = paste(sex, name, sep = "_")) %>% 
  filter(year %in% 1998:2017) %>% 
  group_by(code) %>% 
  mutate(number_years = length(year)) %>% 
  ungroup() %>% 
  filter(number_years == 20) %>% 
  mutate(prop_var = var(prop)) %>% 
  select(year, code, sex:prop, prop_var)

top10_f <- d1 %>% 
  filter(sex == "F") %>% 
  group_by(code) %>% 
  arrange(desc(prop_var)) %>% 
  head(10) %>% 
  pull(code)

top10_m <- d1 %>% 
  filter(sex == "M") %>% 
  group_by(code) %>% 
  arrange(desc(prop_var)) %>% 
  head(10) %>% 
  pull(code)

table_f <- d1 %>% 
  filter(code %in% top10_f) %>% 
  arrange(desc(prop_var)) %>% 
  mutate(name = factor(name, levels = rev(str_remove(top10_f, "F_"))))

table_m <- d1 %>% 
  filter(code %in% top10_m) %>% 
  arrange(desc(prop_var)) %>% 
  mutate(name = factor(name, levels = rev(str_remove(top10_m, "M_"))))

# Create plot ----

p1 <- ggplot(table_f) +
  geom_tile(aes(x = year, y = name, fill = prop),
            colour = "grey40", lwd = 1, linetype = 1,
            show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "#45adab") +
  scale_x_continuous(breaks = seq(1998, 2017, 4)) +
  coord_fixed() +
  ggtitle("Female") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#9de0ad", colour = "#9de0ad"),
        plot.background = element_rect(fill = "#9de0ad", colour = "#9de0ad"),
        plot.title = element_text(family = "Kranky", size = 50, hjust = 0.5),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Kranky", size = 35, colour = "black"))

p2 <- ggplot(table_m) +
  geom_tile(aes(x = year, y = name, fill = prop),
            colour = "grey40", lwd = 1, linetype = 1,
            show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "#45adab") +
  scale_x_continuous(breaks = seq(1998, 2017, 4)) +
  coord_fixed() +
  ggtitle("Male") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#9de0ad", colour = "#9de0ad"),
        plot.background = element_rect(fill = "#9de0ad", colour = "#9de0ad"),
        plot.title = element_text(family = "Kranky", size = 50, hjust = 0.5),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Kranky", size = 35, colour = "black"))

p <- p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "The rise and fall of popular names",
    subtitle = "(darker shades indicate higher proportions)",
    caption = "Visualisation: Jonathan Kitt | Data source: H. Wickham | #TidyTuesday 2022 Week 12",
    theme = theme(plot.background = element_rect(fill = "#9de0ad", colour = "#9de0ad"),
                  panel.background = element_rect(fill = "#9de0ad", colour = "#9de0ad"),
                  plot.title = element_text(family = "Kranky", colour = "black", size = 120, hjust = 0.5,
                                            margin = margin(t = 5, b = 0)),
                  plot.subtitle = element_text(family = "Kranky", colour = "black", size = 80, hjust = 0.5,
                                            margin = margin(t = 0, b = 40)),
                  plot.caption = element_text(colour = "black", hjust = 0.5, size = 25,
                                              margin = margin(t = 40))))

# Save plot ----

ggsave("figs/2022_03_22_babynames.png", p, dpi = 320, width = 12, height = 6)
