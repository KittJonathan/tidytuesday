# TidyTuesday challenge
# Week : 29
# Date : 2022-07-19
# Technology adoption over time
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-06-14

# Load packages ----

library(showtext)
library(ggtext)
library(tidytuesdayR)
library(tidyverse)

# Import fonts ----

font_add_google(name = "Jura", family = "Jura")
showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-07-19')

technology <- tuesdata$technology

rm(tuesdata)

# Data wrangling ----

d1 <- technology %>% 
  filter(iso3c == "FRA",
         year %in% 2000:2020,
         group == "Production",
         category == "Energy",
         grepl("TWH", label)) %>% 
  group_by(year) %>% 
  mutate(total_prod_twh = max(value)) %>% 
  ungroup() %>% 
  mutate(share_pct = 100 * value / total_prod_twh) %>% 
  filter(variable != "elecprod") %>% 
  mutate(label = str_remove(label, "Electricity from ")) %>% 
  filter(variable != "elec_renew_other") %>% 
  mutate(label = str_remove_all(label, " \\(TWH\\)")) %>% 
  filter(label %in% c("wind", "solar", "hydro"))

# Create plot ----

(p <- ggplot() +
   geom_line(data = d1,
                aes(x = year, y = share_pct, colour = label),
             size = 1.5, show.legend = FALSE) +
   scale_colour_manual(values = c("solar" = "#f6be00",
                                  "hydro" = "#b3cde0",
                                  "wind" = "#35a79c")) +
   scale_y_continuous(sec.axis = dup_axis(),
                      labels = paste(seq(0, 15, 5), "%", sep = " ")) +
   labs(title = "Share of <span style = 'color: #b3cde0;'>hydroelectric</span>, <span style = 'color: #35a79c;'>wind</span> and <span style = 'color: #f6be00;'>solar energies</span>",
        subtitle = "in France between 2000 and 2020",
        caption = "Visualisation : Jonathan Kitt | Data source : data.nber.org | #TidyTuesday 2022 week 29") +
   theme_light() +
   theme(panel.background = element_rect(fill = "#011f4b", colour = NA),
         panel.grid.major = element_line(colour = "#005b96", linetype = "dashed"),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         plot.background = element_rect(fill = "#011f4b", colour = NA),
         plot.title = element_markdown(family = "Jura", size = 50, colour = "white", hjust = 0.5, margin = margin(t = 15, b = 5)),
         plot.subtitle = element_text(family = "Jura", size = 40, colour = "white", hjust = 0.5, margin = margin(b = 25)),
         plot.caption = element_text(family = "Jura", size = 25, colour = "white", hjust = 0.5, margin = margin(t = 25)),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         axis.text = element_text(family = "Jura", colour = "#005b96", size = 30))
)

# Save plot ----

ggsave("figs/2022_07_19.png", p, dpi = 320, width = 12, height = 6)
