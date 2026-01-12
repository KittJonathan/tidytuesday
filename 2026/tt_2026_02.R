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

# Data ----

tuesdata <- tidytuesdayR::tt_load(2026, week = 2)
lang <- tuesdata$africa

afr <- map_data("world") |> 
  # Rename Democratic Republic of the Congo -> Congo & Swaziland -> Eswatini
  mutate(region = case_when(region == "Democratic Republic of the Congo" ~ "Congo",
                            region == "Swaziland" ~ "Eswatini",
                            .default = region)) |> 
  # Keep countries that are in the lang dataset
  filter(region %in% lang$country)

# Get complete data of African continent for map background
afr_cont <- ne_countries(scale = "large", 
                         continent = "africa", 
                         returnclass = "sf")

# Extract list of language families
lang_fam <- lang |> 
  distinct(family) |> 
  arrange(family) |> 
  pull()

# Number of languages spoken in each country
# lang_count <- lang |> 
#   count(country) |> 
#   rename(region = country)

# afr |> 
#   left_join(lang_count) |> 
#   ggplot() +
#   geom_sf(colour = "white", fill = "white") +
#   geom_polygon(aes(x = long, y = lat, group = group, fill = n),
#                colour = "white") +
#   coord_fixed()
  

# Plot ----

lang_maps <- list()

# i <- lang_fam[1]

for (i in 1:length(lang_fam)) {
  
  afr_sub <- afr |> 
    filter(region %in% lang$country[lang$family == lang_fam[i]])
  
  total_native_speakers <- lang |> 
    filter(family == lang_fam[i]) |> 
    summarise(total_native_speakers = sum(native_speakers)) |> 
    pull(total_native_speakers)
  
  lang_maps[[i]] <- ggplot() +
    geom_sf(data = afr_cont, colour = "white", fill = "white") +
    geom_polygon(data = afr_sub,
                 aes(x = long, y = lat, group = group),
                 colour = "red", fill = "red") +
    labs(title = glue::glue(lang_fam[i], total_native_speakers)) +
    coord_sf(xlim = c(-25, 60)) +
    theme_void()
  
}

names(lang_maps) <- paste0("p", 1:17)

list2env(lang_maps, envir = globalenv())

patchwork::wrap_plots(lang_maps)



lang |> 
  summarise(total_native_speakers = sum(native_speakers),
            .by = family) |> 
  arrange(total_native_speakers)

ggplot() +
  geom_sf(data = afr_cont, colour = "white", fill = "white") +
  geom_polygon(data = filter(afr, region == "Lesotho"),
               aes(x = long, y = lat, group = group),
               fill = "blue")

africa |> 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_fixed()

africa_map_countries <- sort(unique(africa_map$region))
africa_countries <- sort(unique(africa$country))
