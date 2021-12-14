# TidyTuesday challenge
# Date : 2021-12-14
# Spice Girls data
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-12-14/readme.md

# Install packages ----

# If needed, uncomment lines to install

#install.packages("tidytuesdayR")
#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("textdata")
#install.packages("stopwords")
#install.packages("showtext")

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(textdata)
library(showtext)


# Import fonts ----

showtext_auto()
font_add_google("Quicksand")

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2021-12-14')

lyrics <- tuesdata$lyrics
tracks <- tuesdata$studio_album_tracks
# I won't be looking at the related_artists dataset

rm(tuesdata)

# Clean datasets ----

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
