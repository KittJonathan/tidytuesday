# TidyTuesday challenge
# Date : 2021-12-14
# Spice Girls data
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-12-14/readme.md
# https://peerchristensen.netlify.app/post/fair-is-foul-and-foul-is-fair-a-tidytext-entiment-analysis-of-shakespeare-s-tragedies/

# Install packages ----

# If needed, uncomment lines to install

#install.packages("tidytuesdayR")
#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("textdata")
#install.packages("stopwords")
#install.packages("wordcloud2")
#install.packages("ggwordcloud")
#install.packages("patchwork")
#install.packages("showtext")
#remotes::install_github("johnmackintosh/popthemes")

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(textdata)
library(stopwords)
library(wordcloud2)
library(showtext)
library(webshot)
library(popthemes)
library(patchwork)

# Import fonts ----

showtext_auto()
#font_add_google("Poiret One")
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
  ggtitle(label = "Sentiment analysis on Spice Girls songs",
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

# Comparing minor and major modes ----

modes <- tracks %>% 
  dplyr::group_by(mode_name) %>% 
  dplyr::summarise(mean_danceability = mean(danceability),
                   mean_energy = mean(energy),
                   mean_tempo = mean(tempo))

modes  

head(tracks)

characteristics <- tracks %>% 
  dplyr::select(id, danceability:tempo) %>% 
  tidyr::pivot_longer(!id)

ggplot(data = characteristics,
       mapping = aes(x = id, y = name, fill = value)) +
  geom_tile()
  

# Word clouds ----





  #scale_radius(range = c(0, 20), limits = c(0, NA))

wasting_my_time_words <- lyrics %>% 
  dplyr::filter(track_name == "Wasting My Time") %>% 
  tidytext::unnest_tokens(word, line) %>% 
  dplyr::anti_join(tidytext::get_stopwords()) %>% 
  dplyr::count(word, sort = T) %>% 
  head(10) %>% 
  ggplot(aes(label = word,
             size = n,
             colour = factor(sample.int(11, nrow(.), replace = TRUE)))) +
    ggwordcloud::geom_text_wordcloud_area() +
    scale_size_area(max_size = 50) +
    popthemes::scale_colour_spice() +
  ggtitle(label = "10 most used words in Wasting My Time") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "antiquewhite1"),
        plot.title = element_text(family = "Quicksand",
                                  hjust = 0.5, size = 25,
                                  margin = margin(0, 0, 10, 0)),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = 'grey50'),
        axis.text = element_text(family = "Quicksand", size = 15))

wasting_my_time_words

viva_forever_words <- lyrics %>% 
  dplyr::filter(track_name == "Viva Forever") %>% 
  tidytext::unnest_tokens(word, line) %>% 
  dplyr::anti_join(tidytext::get_stopwords()) %>% 
  dplyr::count(word, sort = T) %>% 
  head(10) %>% 
  ggplot(aes(label = word,
             size = n,
             colour = factor(sample.int(11, nrow(.), replace = TRUE)))) +
  ggwordcloud::geom_text_wordcloud_area() +
  scale_size_area(max_size = 50) +
  popthemes::scale_colour_spice() +
  ggtitle(label = "10 most used words in Viva Forever") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "antiquewhite1"),
        plot.title = element_text(family = "Quicksand",
                                  hjust = 0.5, size = 25,
                                  margin = margin(0, 0, 10, 0)),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = 'grey50'),
        axis.text = element_text(family = "Quicksand", size = 15))

viva_forever_words

spice_girls <- sentiments_plot / (viva_forever_words + wasting_my_time_words)
  
spice_girls

ggsave("figs/2021_12_14_spice.png", dpi = 320, width = 12, height = 6)

# Testing tidytext package ----

bing_sentiments <- lyrics %>% 
  group_by(album_name, track_number) %>% 


valence <- tracks %>% 
  select(album_name, track_number, valence) %>% 
  left_join(sentiments)


head(lyrics)

lyrics %>% map_dbl(n_distinct)



sentiments <- get_sentiments("nrc")

wannabe_sentiments <- words %>% 
  inner_join(sentiments) %>% 
  count(word, sentiment, sort = T)

# Plot number of species discovered each year ----

p1 <- spiders %>% 
  mutate(name = paste(genus, species, sep = " ")) %>% 
  select(year, species = name) %>% 
  count(year) %>% 
  ggplot() +
    geom_smooth(mapping = aes(x = year, y = n),
                se = FALSE,
                colour = "#2e8de1", size = 2) +
    labs(x = "", y = "") +
    labs(caption = "#TidyTuesday 2021-12-07 | World Spiders Database") +
    ggtitle(label = "Number of spider species discovered by year") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#09131C"),
          panel.grid.minor = element_blank(),
          panel.grid = element_line(colour = "grey30"),
          plot.title = element_text(family = "Poiret One",
                               size = 45,
                               hjust = 0.5,
                               vjust = 1,
                               colour = "white"),
          axis.text = element_text(family = "Poiret One",
                                   size = 25,
                                   colour = "white"),
          plot.caption = element_text(colour = "white",
                                      family = "Poiret One",
                                      size = 20,
                                      hjust = 0.5))

# Extract data for 10 most prevalent spider families ----

p2 <- spiders %>% 
  count(family, sort = TRUE) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(family = fct_reorder(family, n)) %>% 
  ggplot() +
    geom_col(mapping = aes(x = family, y = n, fill = n),
             show.legend = FALSE) +
    scale_fill_gradient2(low = "#b65eba",
                         high = "#2e8de1") +
    coord_flip() +
    labs(x = "", y = "") +
    ggtitle(label = "10 most prevalent spider families") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#09131C"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_line(colour = "grey30"),
        plot.title = element_text(family = "Poiret One",
                                  size = 45,
                                  hjust = 0.5,
                                  vjust = 1,
                                  colour = "white"),
        axis.text = element_text(family = "Poiret One",
                                 size = 25,
                                 colour = "white"))
    
# Assemble plots and save ----

plot <- p1 + p2

ggsave("figs/2021_12_07_spiders.png", dpi = 320, width = 12, height = 6)
