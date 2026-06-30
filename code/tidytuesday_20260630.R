# TidyTuesday challenge
# 2026-W36
# 2026-06-30
# Wreck Inventory of Ireland

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-06-30/readme.md
# https://github.com/ericpante/marmap

# Packages ----

library(tidyverse)
library(maps)
library(patchwork)
# library(marmap)
# library(sf)
# library(rnaturalearth)

# library(ggauto)

# Data ----

# 🚢 Shipwrecks

wreck_inventory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-30/wreck_inventory.csv')

wrecks <- wreck_inventory |> 
  # Keep rows when year, longitude are latitude have no NAs
  drop_na(year, longitude, latitude) |> 
  # Keep years between 1900 & 1950
  filter(between(year, 1900, 1950)) |> 
  # Select variables
  select(wreck_no, year, longitude, latitude) |> 
  # Add WWI/WWII info
  mutate(war = case_when(between(year, 1914, 1918) ~ "World War I",
                         between(year, 1939, 1945) ~ "World War II"))

# Plot ----

# Map

world <- rnaturalearth::ne_coastline(scale = "large", returnclass = "sf")

ggplot() +
  geom_sf(data = world, colour = "#96ffea", linewidth = 0.4) +
  geom_point(data = drop_na(wrecks, war), 
             aes(x = longitude, y = latitude, color = war),
             size = 0.4,
             show.legend = FALSE) +
  scale_colour_manual(
    values = c("#93003a", "#ffffe0")
  ) +
  coord_sf(xlim = c(-25, -3),
           ylim = c(46, 58), expand = FALSE) +
  labs(title = "Shipwrecks in Ireland during World War I & World War II") +
  # theme_void() +
  theme(
    plot.background = element_rect(fill = "#000496", colour = "#000496"),
    panel.background = element_rect(fill = "#000496", colour = "#000496")
  )

# Timeline

wrecks |> 
  summarise(total = n(), .by = year) |> 
  arrange(year) |> 
  ggplot() +
  geom_line(aes(x = year, y = total)) +
  annotate(
    geom = "rect",
    xmin = 1914,
    xmax = 1918,
    ymin = 0,
    ymax = Inf,
    fill = "#93003a",
    alpha = 0.5
  ) +
  annotate(
    geom = "rect",
    xmin = 1939,
    xmax = 1945,
    ymin = 0,
    ymax = Inf,
    fill = "#ffffe0",
    alpha = 0.5
  ) +
  labs(x = "",
       y = "Total number of shipwrecks") +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "#000496", colour = "#000496"),
    panel.background = element_rect(fill = "#000496", colour = "#000496")
  )


