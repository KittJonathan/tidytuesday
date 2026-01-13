# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 2
# 2026-01-13
# The languages of Africa

# Load packages ----

library(tidyverse)
library(tidytuesdayR)
library(rnaturalearth)
library(patchwork)

# Import data ----

tuesdata <- tidytuesdayR::tt_load(2026, week = 2)
lang <- tuesdata$africa

# List of countries qualified for the 2025 Africa Cup of Nations
# https://en.wikipedia.org/wiki/2025_Africa_Cup_of_Nations

teams <- c("Morocco", "Burkina Faso", "Cameroon", "Algeria",
           "Congo", "Senegal", "Egypt", "Angola",
           "Equatorial Guinea", "Ivory Coast", "Gabon", "Uganda",
           "South Africa", "Tunisia", "Nigeria", "Mali",
           "Zambia", "Zimbabwe", "Comoros", "Sudan",
           "Benin", "Tanzania", "Botswana", "Mozambique")

# Explore data ----

# Most spoken family language for the 24 countries
lang_fam <- lang |> 
  filter(country %in% teams) |> 
  summarise(total = sum(native_speakers), .by = c(country, family)) |> 
  slice_max(order_by = total, n = 1, by = country) |> 
  select(region = country, family)

# Create map: 6 most spoken language families ----

# Map of Africa
afr <- map_data("world") |> 
  # Rename Democratic Republic of the Congo -> Congo & Swaziland -> Eswatini
  mutate(region = case_when(region == "Democratic Republic of the Congo" ~ "Congo",
                            region == "Swaziland" ~ "Eswatini",
                            .default = region)) |> 
  filter(region %in% lang$country) |> 
  left_join(lang_fam) |> 
  filter(!is.na(family))

# Get complete data of African continent for map background
afr_cont <- ne_countries(scale = "large", 
                         continent = "africa", 
                         returnclass = "sf")

# Plot ----

p <- ggplot() +
  geom_sf(data = afr_cont, colour = "#1d2d46", fill = "#1d2d46") +
  geom_polygon(data = afr, 
               aes(x = long, y = lat, group = group, fill = family),
               colour = "#1d2d46", linewidth = 0.2) +
  scale_fill_manual(values = c("Indo-European" = "#e69f00",
                               "Afroasiatic" = "#009e73",
                               "Niger–Congo" = "#0072b2")) +
  labs(title = "Most spoken family languages",
       subtitle = "For the 24 countries taking part in the\n2025 African Cup of Nations",
       caption = "#TidyTuesday 2026 W02 | Jonathan Kitt") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        plot.title = element_text(colour = "white", hjust = 0.5, size = 18, face = "bold",
                                  margin = margin(t = 10)),
        plot.subtitle = element_text(colour = "white", hjust = 0.5,
                                     margin = margin(t = 5, b = 20), size = 14),
        plot.caption = element_text(colour = "white", hjust = 0.5,
                                    size = 8, margin = margin(b = 10)),
        legend.text = element_text(colour = "white"),
        legend.position = "top")

ggsave("2026/tt_2026_02.png", p, dpi = 320, width = 12, height = 6)
