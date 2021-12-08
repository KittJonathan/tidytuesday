# TidyTuesday challenge
# Date : 2021-12-07
# World Spider Database
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-12-07/readme.md

# Install packages ----

# If needed, uncomment lines to install

#install.packages("tidytuesdayR")
#install.packages("tidyverse")
#install.packages("patchwork")
#install.packages("showtext")

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(patchwork)

# Import fonts ----

showtext_auto()
font_add_google("Poiret One")

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2021-12-07')
spiders <- tuesdata$spiders

rm(tuesdata)

# Plot number of species discovered each year ----

p1 <- spiders %>% 
  mutate(name = paste(genus, species, sep = " ")) %>% 
  select(year, species = name) %>% 
  count(year) %>% 
  ggplot() +
    geom_smooth(mapping = aes(x = year, y = n),
                se = FALSE,
                colour = "#2e8de1", size = 2) +
    labs(x = "", y = "") +
    labs(caption = "#TidyTuesday 2021-12-07 | World Spiders Database") +
    ggtitle(label = "Number of spider species discovered by year") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#09131C"),
          panel.grid.minor = element_blank(),
          panel.grid = element_line(colour = "grey30"),
          plot.title = element_text(family = "Poiret One",
                               size = 45,
                               hjust = 0.5,
                               vjust = 1,
                               colour = "white"),
          axis.text = element_text(family = "Poiret One",
                                   size = 25,
                                   colour = "white"),
          plot.caption = element_text(colour = "white",
                                      family = "Poiret One",
                                      size = 20,
                                      hjust = 0.5))

# Extract data for 10 most prevalent spider families ----

p2 <- spiders %>% 
  count(family, sort = TRUE) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(family = fct_reorder(family, n)) %>% 
  ggplot() +
    geom_col(mapping = aes(x = family, y = n, fill = n),
             show.legend = FALSE) +
    scale_fill_gradient2(low = "#b65eba",
                         high = "#2e8de1") +
    coord_flip() +
    labs(x = "", y = "") +
    ggtitle(label = "10 most prevalent spider families") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#09131C"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_line(colour = "grey30"),
        plot.title = element_text(family = "Poiret One",
                                  size = 45,
                                  hjust = 0.5,
                                  vjust = 1,
                                  colour = "white"),
        axis.text = element_text(family = "Poiret One",
                                 size = 25,
                                 colour = "white"))
    
# Assemble plots and save ----

plot <- p1 + p2

ggsave("figs/2021_12_07_spiders.png", dpi = 320, width = 12, height = 6)
