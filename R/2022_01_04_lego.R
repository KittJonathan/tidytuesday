# TidyTuesday challenge
# Date : 2022-01-04
# Bring Your Own Data
# Lego database from Kaggle
# https://www.kaggle.com/rtatman/lego-database

# Examples ----

# https://medium.com/analytics-vidhya/exploratory-analysis-on-lego-dataset-8967e37fc5dd
# https://mode.com/blog/lego-data-analysis/

# Install packages ----

# If needed, uncomment lines to install

#install.packages("tidyverse")

# Load packages ----

library(tidyverse)

# Import datasets ----

colors <- read_csv("data/kaggle_lego/colors.csv")
inventories <- read_csv("data/kaggle_lego/inventories.csv")
inventory_parts <- read_csv("data/kaggle_lego/inventory_parts.csv")
inventory_sets <- read_csv("data/kaggle_lego/inventory_sets.csv")
part_categories <- read_csv("data/kaggle_lego/part_categories.csv")
parts <- read_csv("data/kaggle_lego/parts.csv")
sets <- read_csv("data/kaggle_lego/sets.csv")
themes <- read_csv("data/kaggle_lego/themes.csv")

# Prepare and clean data ----

# Find Middle-Earth theme names and ids
themes %>% 
  filter(str_detect(name, "Hobbit|Lord"))  # parent ids : 561, 562, 566

middle_earth_themes <- themes %>% 
  filter(parent_id %in% c(561, 562, 566)) %>% 
  select(theme_id = id, theme_name = name)
  
# Extract sets with Middle-Earth themes
middle_earth_sets <- sets %>% 
  filter(theme_id %in% middle_earth_themes$theme_id) %>% 
  left_join(middle_earth_themes)

# Extract inventories from Middle-Earth themed sets
middle_earth_inventories <- inventories %>% 
  filter(set_num %in% middle_earth_sets$set_num) %>% 
  left_join(middle_earth_sets)

# Extract inventory parts from Middle-Earth themes sets
middle_earth_inventory_parts <- inventory_parts %>% 
  filter(inventory_id %in% middle_earth_inventories$id) %>% 
  select(id = inventory_id, everything()) %>% 
  left_join(middle_earth_inventories) %>% 
  select(year, theme_name, set_name = name, num_parts, color_id)

# Extract colors for Middle-Earth themed sets
middle_earth_colors <- colors %>% 
  filter(id %in% middle_earth_inventory_parts$color_id) %>% 
  select(color_id = id, hex = rgb)

# Create dataset for plots
d1 <- middle_earth_inventory_parts %>% 
  left_join(middle_earth_colors) %>% 
  mutate(hex = paste0("#", hex)) %>% 
  select(year:num_parts, hex) %>% 
  arrange(year, set_name)

# Clear working environment
rm(colors, inventories, inventory_parts, inventory_sets,
   middle_earth_colors, middle_earth_inventories, middle_earth_inventory_parts,
   middle_earth_sets, middle_earth_themes, part_categories, parts, sets, themes)

# Extract color palette for each movie ----

# Count number of occurences of each color by movie
col_count <- d1 %>% 
  filter(!theme_name %in% c("The Lord of the Rings", "The Hobbit")) %>% 
  count(theme_name, hex) %>% 
  group_by(theme_name) %>% 
  top_n(n = 5) %>% 
  arrange(theme_name, desc(n)) %>% 
  ungroup() %>% 
  mutate(x = rep(seq(0, 20, 5), times = 6),
         xend = rep(seq(5, 25, 5), times = 6),
         y = case_when(theme_name == "An Unexpected Journey" ~ 5,
                       theme_name == "The Desolation of Smaug" ~ 4,
                       theme_name == "The Battle of the Five Armies" ~ 3,
                       theme_name == "The Fellowship of the Ring"  ~ 2,
                       theme_name == "The Two Towers" ~ 1,
                       theme_name == "The Return of the King" ~ 0))

# Create plot ----

p <- ggplot(data = col_count) +
  geom_segment(aes(x = x, xend = xend,
                   y = y, yend = y),
               colour = col_count$hex,
               size = 15) +
  geom_text(mapping = aes(x = 0, y = y + 0.45,
                          label = theme_name,
                          hjust = 0),
            size = 6) +
  ggtitle("Colour palettes for Middle-Earth themed Lego sets") +
  theme_void() +
  theme(plot.title = element_text(size = 25, hjust = 0.5))

ggsave("figs/2022_01_04_lego.png", p, dpi = 320, width = 12, height = 6)