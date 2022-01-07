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

ggplot(data = col_count) +
  geom_segment(aes(x = x, xend = xend,
                   y = y, yend = y),
               colour = col_count$hex,
               size = 20) +
  geom_text(mapping = aes(x = 0, y = y + 0.5,
                          label = theme_name,
                          hjust = 0))


test <- col_count %>% 
  filter(theme_name == "An Unexpected Journey")

test <- col_count %>% 
  filter(theme_name == "The Two Towers")

ggplot(data = test) +
  geom_segment(aes(x = 0, xend = 5,
                   y = 0, yend = 0),
               colour = test$hex[1],
               size = 10) +
  geom_segment(aes(x = 5, xend = 10,
                   y = 0, yend = 0),
               colour = test$hex[2],
               size = 10) +
geom_segment(aes(x = 10, xend = 15,
                 y = 0, yend = 0),
             colour = test$hex[3],
             size = 10) +
  geom_segment(aes(x = 15, xend = 20,
                   y = 0, yend = 0),
               colour = test$hex[4],
               size = 10) +
  geom_segment(aes(x = 20, xend = 25,
                   y = 0, yend = 0),
               colour = test$hex[5],
               size = 10)
  

ggplot(data = col_count,
       mapping = aes(x = x,
                     y = y)) +
  geom_point(aes(size = n),
             colour = hex)


# Subset Middle-Earth colors
colors <- colors %>% 
  dplyr::filter(id %in% inventory_parts$color_id)


# Extract colors for Weetabix Castle ----

colors <- colors %>% 
  select(color_id = id,
         everything())

sets %>% 
  head(1)  # set_num 00-1

inventories %>% 
  filter(set_num == "00-1")  # id 5574

inventory_parts %>% 
  filter(inventory_id == "5574") %>% 
  distinct(color_id) %>% 
  left_join(colors)

part_categories <- part_categories %>% 
  select(part_cat_id = id,
         name)

inventory_parts %>% 
  filter(inventory_id == "5574") %>% 
  left_join(part_categories)

# Explore Weetabix castle data ----

weetabix_castle_set <- sets %>% 
  dplyr::filter(set_num == "00-1")

# colors match the pictures found on internet

themes %>% 
  dplyr::filter(id == weetabix_castle_set$theme_id)
# theme id 414 : Castle
# parent_id 411 : Legoland

weetabix_castle_inventory <- inventories %>% 
  dplyr::filter(set_num == weetabix_castle_set$set_num)

weetabix_castle_inventory_sets <- inventory_sets %>% 
  dplyr::filter(set_num == weetabix_castle_set$set_num)

# Clean datasets ----

head(themes, 2)
summary(themes$parent_id)

parent_themes <- themes %>% 
  dplyr::filter(is.na(parent_id))

# Change column names so they match between datasets
# Add "#" in front of hex codes in colors

colors <- colors %>% 
  dplyr::select(color_id = id,
                color_name = name,
                color_hex = rgb,
                color_trans = is_trans) %>% 
  dplyr::mutate(color_hex = paste0("#", color_hex))

inventories <- inventories %>% 
  dplyr::select(inventory_id = id,
                version_number = version,
                set_number = set_num)

inventory_parts <- inventory_parts %>% 
  dplyr::select(inventory_id,
                part_number = part_num,
                quantity,
                is_spare)

inventory_sets <- inventory_sets %>% 
  dplyr::select(inventory_id,
                set_number = set_num,
                quantity)

parts <- parts %>% 
  dplyr::select(part_num,
                part_name = name,
                part_cat_id)

part_categories <- part_categories %>% 
  dplyr::select(part_cat_id = id,)



# Clean lyrics dataset
lyrics <- lyrics %>%
  dplyr::select(album_name,
                track_number,
                track_name = song_name,
                line_number:section_artist)

# Clean tracks dataset
tracks <- tracks %>% 
  dplyr::select(album_name,
                year = album_release_year,
                track_number,
                track_name,
                danceability,
                energy,
                loudness,
                speechiness:tempo,
                duration_ms,
                key_name,
                mode_name) %>% 
  dplyr::arrange(year, track_number) %>% 
  dplyr::mutate(id = 1:31) %>%  # add index 
  dplyr::select(id, everything())

# extract list of ids with matching tracks 
idx <- tracks %>% 
  dplyr::select(album_name, track_number, id)

# add id to lyrics dataset
lyrics <- lyrics %>% 
  dplyr::left_join(idx) %>% 
  dplyr::select(id, everything())

rm(idx)

# Clean track names in lyrics dataset
clean_names <- tracks %>% 
  dplyr::select(id, clean_name = track_name)

lyrics <- lyrics %>% 
  dplyr::left_join(clean_names) %>% 
  dplyr::select(id:track_number,
                track_name = clean_name,
                line_number:section_artist)

rm(clean_names)

# Sentiment analysis ----

#https://peerchristensen.netlify.app/post/fair-is-foul-and-foul-is-fair-a-tidytext-entiment-analysis-of-shakespeare-s-tragedies/

sentiments <- lyrics %>% 
  dplyr::group_by(id, album_name, track_name) %>% 
  tidytext::unnest_tokens(word, line) %>% 
  dplyr::inner_join(tidytext::get_sentiments("bing")) %>% 
  dplyr::count(sentiment) %>% 
  tidyr::spread(sentiment, n, fill = 0) %>% 
  dplyr::mutate(ratio = (positive - negative) / (positive + negative)) %>% 
  dplyr::select(id:track_name, ratio) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(track_name = forcats::fct_reorder(track_name, rev(id))) %>% 
  dplyr::mutate(type = ifelse(ratio < 0, "negative", "positive"))

sentiments_plot <- ggplot(data = sentiments) +
  geom_point(aes(x = ratio, y = track_name,
                 colour = type),
             size = 2,
             show.legend = FALSE) +
  geom_segment(aes(x = 0, xend = ratio,
                   y = rev(id), yend = rev(id),
                   colour = type), size = 0.5,
               show.legend = FALSE) +
  scale_colour_manual(values = c("negative" = "red3",
                                 "positive" = "green4")) +
  geom_vline(xintercept = 0) +
  xlim(c(-1, 1)) +
  xlab("") +
  ylab("") +
  labs(caption = "#TidyTuesday 2021-12-14 | Spice Girls Data by @jacquietran") +
  ggtitle(label = "Sentiment analysis of Spice Girls songs",
          subtitle = "Positive to negative ratio based on bing database") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "antiquewhite1"),
        plot.title = element_text(family = "Quicksand",
                                  hjust = 0.5, size = 45,
                                  margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(family = "Quicksand",
                                      hjust = 0.5, size = 35,
                                     margin = margin(0, 0, 30, 0)),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = 'grey70', linetype = "dashed"),
        axis.text = element_text(family = "Quicksand", size = 25),
        plot.caption = element_text(family = "Quicksand",
                                    size = 20,
                                    hjust = 0.5))

ggsave("figs/2021_12_14_spice.png", dpi = 320, width = 12, height = 6)
