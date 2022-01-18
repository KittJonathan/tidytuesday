# TidyTuesday challenge
# Date : 2022-01-18
# Chocolate bars
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-18/readme.md

# Links ----

# https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
#library(showtext)

# Import fonts ----

#font_add_google("Poiret One", "Poiret")
#showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate
rm(tuesdata)

# Data wrangling ----

head(chocolate)

# Add a column with id index
chocolate <- chocolate %>% 
  dplyr::mutate(id = 1:nrow(chocolate)) %>% 
  dplyr::select(id, everything())

# Extract ingredients information
ingredients <- chocolate %>% 
  dplyr::select(id, ingredients) %>% 
  dplyr::mutate(nb_ingredients = parse_number(ingredients),
                list_ingredients = str_remove(ingredients, "[0-9]-")) %>% 
  tidyr::separate(list_ingredients, paste0("ingr", 1:7), ",") %>% 
  dplyr::select(-ingredients) %>% 
  tidyr::pivot_longer(!c(id, nb_ingredients),
                      names_to = "ingredients",
                      values_drop_na = TRUE) %>% 
  dplyr::select(id, nb_ingredients, ingredients = value) %>% 
  dplyr::mutate(ingredients = str_remove(ingredients, " ")) %>% 
  dplyr::mutate(ingredients = case_when(ingredients == "B" ~ "beans",
                                        ingredients == "S" ~ "sugar",
                                        ingredients == "S*" ~ "sweetener",
                                        ingredients == "C" ~ "cocoa_butter",
                                        ingredients == "V" ~ "vanilla",
                                        ingredients == "L" ~ "lecithin",
                                        ingredients == "Sa" ~ "Salt",
                                        TRUE ~ ingredients))

head(ingredients)

# World tile grid map ----

world_tile_grid <- read_csv("data/worldtilegrid.csv")

ggplot() +
  geom_rect(data = world_tile_grid,
            mapping = aes(xmin = x, xmax = x + 1,
                          ymin = y, ymax = y + 1,
                          fill = region),
            colour = "white") +
  geom_text(data = world_tile_grid,
            mapping = aes(x = x,
                          y = y,
                          label = alpha.2),
            colour = "white",
            alpha = 0.5,
            nudge_x = 0.5,
            nudge_y = -0.5,
            size = 3) +
  scale_x_continuous(breaks = seq(1, 29, 1)) +
  #scale_y_continuous(breaks = seq(1, 24, 1)) +
  scale_y_reverse(breaks = seq(1, 24, 1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black"))

ggplot(data = world_tile_grid,
       mapping = aes(x = x,
                     y = y)) +
  geom_text(mapping = aes(label = alpha.2))


# Save figs ----
ggsave("figs/2022_01_11_bees.png", p, dpi = 320, width = 12, height = 6) 