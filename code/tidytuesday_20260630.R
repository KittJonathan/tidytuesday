# TidyTuesday challenge
# 2026-W36
# 2026-06-30
# Wreck Inventory of Ireland

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-06-30/readme.md
# https://github.com/ericpante/marmap

# Packages ----

library(tidyverse)
library(patchwork)
library(marmap)
library(sf)
library(rnaturalearth)
# library(maps)
# library(ggauto)

# Data ----

# 🚢 Shipwrecks

wreck_inventory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-30/wreck_inventory.csv')

wrecks <- wreck_inventory |> 
  drop_na(year, longitude, latitude) |> 
  filter(between(year, 1900, 1950)) |> 
  select(wreck_no, year, longitude, latitude) |> 
  mutate(war = case_when(between(year, 1914, 1918) ~ "World War I",
                         between(year, 1939, 1945) ~ "World War II"))

# 🌍 Bathymetric data - {marmap} package

bathy <- getNOAA.bathy(lon1 = min(wrecks$longitude) - 1, 
                     lon2 = max(wrecks$longitude) + 1, 
                     lat1 = min(wrecks$latitude) - 1, 
                     lat2 = max(wrecks$latitude) + 1, 
                     res = 1, 
                     keep = TRUE)

bathy <- as.xyz(bathy)

# Plots 

ggplot() +
  geom_tile(data = bathy,
            aes(x = V1, y = V2, fill = V3),
            show.legend = FALSE) +
  geom_point(data = drop_na(wrecks, war),
             aes(x = longitude, y = latitude,
                 color = war)) +
  scale_fill_gradient(low = "#0A3D62",
                      high = "#2874A6") +
  scale_color_manual(values = c("#b000ff", "#ffff00")) +
  theme_void()




# Import country data
country <- ne_countries(scale = "medium", returnclass = "sf")

# Plot using ggplot and sf
ggplot() + 
  geom_sf(data = country) +
  geom_tile(data = bathy, aes(x = V1, y = V2, fill = V3)) +
  geom_contour(data = bathy, 
               aes(x = V1, y = V2, z = V3),
               binwidth = 100, color = "grey85", size = 0.1) +
  geom_contour(data = bathy, 
               aes(x = V1, y = V2, z = V3),
               breaks = -200, color = "grey85", size = 0.5) +
  geom_sf(data = country) +
  coord_sf(xlim = c(-12, -5), 
           ylim = c(35, 44)) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  theme_minimal()

ireland <- countries110 |> 
  filter(SOVEREIGNT == "Ireland")

ireland_hi <- ne_countries(
  country    = "Ireland",
  scale      = "large",
  returnclass = "sf"
)

ggplot(ireland) +
  geom_sf(fill = "#2E8B57", colour = "white") +
  labs(title = "Ireland") +
  theme_minimal()

# Data ----

wreck_inventory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-30/wreck_inventory.csv')

wreck_inventory |> 
  drop_na(longitude, latitude) |> 
  summarise(long_min = min(longitude),
            long_max = max(longitude),
            lat_min = min(latitude),
            lat_max = max(latitude))

bat <- getNOAA.bathy(-25, -5, 45, 58, res = 1, keep = TRUE)
bat_xyz <- as.xyz(bat)

country <- ne_countries(scale = "medium", returnclass = "sf")

p <- ggplot() + 
  geom_tile(data = bat_xyz, aes(x = V1, y = V2, fill = V3)) +
  geom_sf(data = country) +
  geom_point(data = wreck_inventory, 
             aes(x = longitude, y = latitude)) +
  coord_sf(xlim = c(-25, -5), 
           ylim = c(45, 58), expand = F) 


ggsave("figs/tt_2026_06_30.png", dpi = 320, height = 6, width = 12)

# Explore data ----

glimpse(wreck_inventory)

wreck_inventory |> 
  drop_na(date, latitude, longitude)
  

wrecks <- wreck_inventory |> 
  drop_na(date, longitude, latitude) |> 
  filter(year < 1950)

# Create map ----

worldmap <- map_data(map = "world")

ireland <- worldmap |> 
  filter(region == "Ireland" | (region == "UK" & subregion == "Northern Ireland"))

ggplot() +
  geom_polygon(data = ireland, aes(x = long, y = lat, group = group),
               fill = "blue", color = "blue") +
  geom_point(data = wrecks, 
             aes(x = longitude, y = latitude, color = as.factor(year)),
             alpha = 0.2) +
  coord_fixed()

wrecks |> 
  filter(year >= 1926) |> 
  summarise(total = n(), .by = year) |>
  ggplot() +
  geom_line(aes(x = year, y = total))

wreck_inventory |> 
  drop_na(date, year, longitude, latitude) |> 
  count(year) |> 
  arrange(year)
