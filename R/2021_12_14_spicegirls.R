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
#install.packages("patchwork")
#install.packages("showtext")

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(textdata)
library(stopwords)
library(wordcloud2)
#library(showtext)
#library(patchwork)

# Import fonts ----

showtext_auto()
font_add_google("Poiret One")

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2021-12-14')

lyrics <- tuesdata$lyrics
tracks <- tuesdata$studio_album_tracks

rm(tuesdata)

# Clean datasets ----

lyrics <- lyrics %>%
  dplyr::select(album_name,
                track_number,
                track_name = song_name,
                line_number:section_artist)

tracks <- tracks %>% 
  select(album_name,
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
  dplyr::arrange(year, track_number)
  
# Testing tidytext package ----

bing_sentiments <- lyrics %>% 
  group_by(album_name, track_number) %>% 
  unnest_tokens(word, line) %>% 
  #anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment_score = (positive - negative) / (positive + negative)) %>% 
  select(album_name, track_number, sentiment_score)

nrc_sentiments <- lyrics %>% 
  group_by(album_name, track_number) %>% 
  unnest_tokens(word, line) %>% 
  #anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(sentiment) %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~(album_name))
  

modes <- tracks %>% 
  select(album_name, track_number, mode_name)

sentiment_mode <- sentiments %>% 
  left_join(modes)

valence <- tracks %>% 
  select(album_name, track_number, valence) %>% 
  left_join(sentiments)


head(lyrics)

lyrics %>% map_dbl(n_distinct)

wannabe_lyrics <- lyrics %>% 
  filter(song_name == "Wannabe") %>% 
  select(line)

words <- wannabe_lyrics %>% 
  unnest_tokens(word, line) %>% 
  select(word)

clean_words <- words %>% 
  anti_join(get_stopwords()) %>% 
  count(word, sort = T) %>% 
  top_n(150) %>% 
  wordcloud2(size = .7)

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
