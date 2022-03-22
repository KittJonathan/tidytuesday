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

font_add_google(name = "Kodchasan", family = "Kodchasan")
showtext_auto()

# Data wrangling ----

new_pkgs_years <- bioc %>% 
  mutate(year = year(date)) %>% 
  group_by(package) %>% 
  mutate(first_release = min(year)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  count(first_release) %>% 
  filter(first_release > 1970)

time_of_day <- bioc %>% 
  mutate(hour = hour(date)) %>% 
  count(hour)

# Create plot ----

p1 <- ggplot(data = new_pkgs_years) +
  geom_point(aes(x = first_release, y = n),
             colour = "#b8d943", size = 4) +
  geom_line(aes(x = first_release, y = n),
            colour = "#b8d943", size = 0.75) +
  scale_x_continuous(breaks = seq(2001, 2021, 5)) +
  ggtitle(label = "Number of new packages released each year") +
  theme_minimal() +
  theme(panel.background = element_rect(colour = "#0194b5", fill = "#0194b5"),
        plot.background = element_rect(colour = "#0194b5", fill = "#0194b5"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey70"),
        axis.title = element_blank(),
        axis.text = element_text(colour = "white", family = "Kodchasan", size = 25),
        plot.title = element_text(family = "Kodchasan", colour = "white", size = 50,
                                  margin = margin(t = 0, b = 0)))

p2 <- ggplot(data = time_of_day) +
  geom_rect(aes(xmin = hour, xmax = hour + 1,
                ymin = 0, ymax = n),
            fill = "#b8d943", colour = "#0194b5") +
  scale_x_continuous(breaks = seq(0, 24, 1),
                     labels = c("12 pm", "1 am", "2 am", "3 am",
                                "4 am", "5 am", "6 am", "7 am", "8 am",
                                "9 am", "10 am", "11 am", "12 am",
                                "1 pm", "2 pm", "3 pm", "4 pm", "5 pm",
                                "6 pm", "7 pm", "8 pm", "9 pm", "10 pm", "11 pm", "12 pm")) +
  ggtitle(label = "Number of releases by time of day") +
  theme_minimal() +
  theme(panel.background = element_rect(colour = "#0194b5", fill = "#0194b5"),
        plot.background = element_rect(colour = "#0194b5", fill = "#0194b5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70"),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(colour = "white", family = "Kodchasan", size = 25),
        plot.title = element_text(family = "Kodchasan", colour = "white", size = 50,
                                  margin = margin(t = 0, b = 0)))
 
p <- p1 + p2 +
  plot_layout(ncol = 1) +
  plot_annotation(
    title = "Bioconductor packages",
    caption = "Visualisation: Jonathan Kitt | Data source: Robert M Flight | #TidyTuesday 2022 Week 11",
    theme = theme(plot.background = element_rect(fill = "#0194b5", colour = "#0194b5"),
                  panel.background = element_rect(fill = "#0194b5", colour = "#0194b5"),
                  plot.title = element_text(family = "Kodchasan", colour = "white", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 20)),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25,
                                              margin = margin(t = 20))))

# Save plot ----

ggsave("figs/2022_03_15_vignettes.png", p, dpi = 320, width = 12, height = 6)
