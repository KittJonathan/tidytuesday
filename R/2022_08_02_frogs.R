# TidyTuesday challenge
# Week : 31
# Date : 2022-08-02
# Oregon spotted frog
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-08-02

# Load packages ----

library(showtext)
library(osmdata)
library(sf)
library(tidyverse)

# Import fonts ----

font_add_google(name = "Abel", family = "Abel")
showtext_auto()

# Import dataset ----

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frog.csv')

# Data wrangling ----

# To convert UTM coords to lat long : https://stackoverflow.com/questions/67106215/sf-from-utm-to-latitude-longitude

utm_coords <- frogs %>% 
  select(UTME_83, UTMN_83)

longlat <- st_as_sf(x = utm_coords,
                    coords = c("UTME_83", "UTMN_83"),
                    crs = "+proj=utm +zone=10") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  as_tibble()

list_long <- list()
list_lat <- list()

for (i in 1:nrow(longlat)) {
  list_long[[i]] <- longlat$geometry[[i]][1]
  list_lat[[i]] <- longlat$geometry[[i]][2]
}

frogs <- frogs %>% 
  mutate(long = unlist(list_long),
         lat = unlist(list_lat)) %>% 
  select(long, lat, Habitat = HabType)

rm(i, list_lat, list_long, longlat, utm_coords)

# Create plot ----

# Get data from OSM for Crane Prairie Reservoir

water <- opq(bbox = c(-121.83, 43.76, -121.76, 43.815)) %>% 
  add_osm_feature(key = "natural",
                  value = "water") %>% 
  osmdata_sf()

river <- opq(bbox = c(-121.83, 43.76, -121.76, 43.815)) %>% 
  add_osm_feature(key = "waterway",
                  value = "river") %>% 
  osmdata_sf()

(p <- ggplot() +
  geom_sf(data = water$osm_polygons,
          inherit.aes = FALSE,
          fill = "#cae9f5", colour = "#cae9f5") +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          colour = "#cae9f5", size = 1.5) +
  geom_point(data = frogs,
             aes(x = long, y = lat, colour = Habitat),
             size = 3, alpha = 0.5) +
  scale_colour_manual(values = c("Pond" = "#e69f00",
                                 "Reservoir" = "#999999",
                                 "River" = "#0072b2")) +
  coord_sf(xlim = c(-121.83, -121.76),
           ylim = c(43.76, 43.815),
           expand = TRUE) +
  labs(title = "Oregon spotted frogs",
       subtitle = "311 frogs observed at Crane Prairie Reservoir between September 12th and November 29th, 2018",
       caption = "Visualisation : Jonathan Kitt | Data source : USGS | #TidyTuesday 2022 week 31") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#f0f8ff", colour = NA),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#f0f8ff", colour = NA),
        plot.title = element_text(family = "Abel", colour = "#006400",
                                  size = 75, hjust = 0.5, margin = margin(t = 20, b = 10)),
        plot.subtitle = element_text(family = "Abel", colour = "#006400",
                                     size = 50, hjust = 0.5, margin = margin(b = 20)),
        plot.caption = element_text(family = "Abel", colour = "#006400",
                                     size = 20, hjust = 0.5, margin = margin(t = 20, b = 5)),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(family = "Abel", colour = "#006400", size = 40,
                                    margin = margin(b = -10)),
        legend.text = element_text(family = "Abel", colour = "#006400", size = 35,
                                   margin = margin(l = -20))) +
    guides(colour = guide_legend(override.aes = list(alpha = 1,
                                                     size = 3)))
)

# Save plot ----

ggsave("figs/2022_08_02_frogs.png", p, dpi = 320, width = 12, height = 6)
