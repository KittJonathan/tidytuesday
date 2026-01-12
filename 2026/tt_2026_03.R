# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 3
# 2026-01-20
# Astronomy Picture Of the Day (APOD)

# Load packages ----

library(tidyverse)
library(tidytuesdayR)
# library(patchwork)

# Data ----

tuesdata <- tidytuesdayR::tt_load(2026, week = 3)
apod <- tuesdata$apod

apod |> 
  mutate(planet = case_when(str_detect(string = title, pattern = "Mercury") ~ "Mercury",
                            str_detect(string = title, pattern = "Venus") ~ "Venus",
                            str_detect(string = title, pattern = "Earth") ~ "Earth",
                            str_detect(string = title, pattern = "Mars") ~ "Mars",
                            str_detect(string = title, pattern = "Jupiter") ~ "Jupiter",
                            str_detect(string = title, pattern = "Saturn") ~ "Saturn",
                            str_detect(string = title, pattern = "Uranus") ~ "Uranus",
                            str_detect(string = title, pattern = "Neptune") ~ "Neptune",
                            .default = NA)
  ) |> 
  drop_na(planet) |> 
  filter(year(date) >= 2020) |> 
  ggplot(aes(x = date, y = 1)) +
  geom_point() +
  facet_wrap(~planet, ncol = 4)

  drop_na(planet)
  mutate(mercury = str_detect(string = title, pattern = "Mercury"),
         venus = str_detect(string = title, pattern = "Venus"),
         earth = str_detect(string = title, pattern = "Earth"),
         mars = str_detect(string = title, pattern = "Mars"),
         jupiter = str_detect(string = title, pattern = "Jupiter"),
         saturn = str_detect(string = title, pattern = "Saturn"),
         uranus = str_detect(string = title, pattern = "Uranus"),
         neptune = str_detect(string = title, pattern = "Neptune")
         )
  

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
