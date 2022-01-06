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

# Import fonts ----

showtext_auto()
font_add_google("Quicksand")

# Import datasets ----

colors <- readr::read_csv("data/kaggle_lego/colors.csv")
inventories <- readr::read_csv("data/kaggle_lego/inventories.csv")
inventory_parts <- readr::read_csv("data/kaggle_lego/inventory_parts.csv")
inventory_sets <- readr::read_csv("data/kaggle_lego/inventory_sets.csv")
part_categories <- readr::read_csv("data/kaggle_lego/part_categories.csv")
parts <- readr::read_csv("data/kaggle_lego/parts.csv")
sets <- readr::read_csv("data/kaggle_lego/sets.csv")
themes <- readr::read_csv("data/kaggle_lego/themes.csv")

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
