# TidyTuesday challenge
# Date : 2022-01-25
# Board games
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-25/readme.md

# Load packages ----

library(patchwork)
library(showtext)
library(tidytuesdayR)
library(tidyverse)


# Import fonts ----

font_add_google("Bangers", "bangers")
#font_add_google("Dancing Script", "Dance")
font_add_google("Poiret One", "poiret")
#showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-01-25')
details <- tuesdata$details
ratings <- tuesdata$ratings
rm(tuesdata)

# Data wrangling ----

# Clean details dataset : 
# 1) keep games published from 1950 onwards
# 1) count number of expansions per game
# 2) select columns

details <- details %>% 
  filter(yearpublished >= 1950) %>% 
  mutate(nb_expansions = ifelse(is.na(boardgameexpansion), 0,
                                lengths(str_split(boardgameexpansion, ",")))) %>% 
  select(id, name = primary, year = yearpublished, owned, nb_expansions,
         min_players = minplayers, max_players = maxplayers, playing_time = playingtime,
         min_play_time = minplaytime, max_play_time = maxplaytime,
         min_age = minage, category = boardgamecategory,
         mechanic = boardgamemechanic,
         designer = boardgamedesigner, artist = boardgameartist)

# Clean ratings dataset : select columns

ratings <- ratings %>% 
  filter(id %in% details$id) %>% 
  select(id, rank:users_rated, thumbnail)

# Create categories dataset :
# 1) remove games w/o category
# 2) select columns
# 3) remove "[" and "]" from strings
# 4) count number of categories per game
# 5) split categories into separate columns
# 6) transform into long format using pivot_longer

categories <- details %>% 
  filter(!is.na(category)) %>% 
  select(id, category) %>% 
  mutate(category = str_sub(category, 2, -2)) %>% 
  mutate(nb_categories = ifelse(is.na(category), 0,
                                lengths(str_split(category, ",")))) %>% 
  separate(category, paste0("cat", 1:max(.$nb_categories)), ",") %>% 
  select(-nb_categories) %>% 
  pivot_longer(!id, names_to = "category", values_drop_na = TRUE) %>% 
  select(id, category = value) %>% 
  mutate(category = parse_character(category, trim_ws = TRUE)) %>% 
  mutate(category = str_sub(category, 2, -2))

# Create mechanics dataset :
# 1) remove games w/o mechanic
# 2) select columns
# 3) remove "[" and "]" from strings
# 4) count number of mechanics per game
# 5) split mechanics into separate columns
# 6) transform into long format using pivot_longer

mechanics <- details %>% 
  filter(!is.na(category)) %>% 
  select(id, mechanic) %>% 
  mutate(mechanic = str_sub(mechanic, 2, -2)) %>% 
  mutate(nb_mechanics = ifelse(is.na(mechanic), 0,
                                lengths(str_split(mechanic, ",")))) %>% 
  separate(mechanic, paste0("mech", 1:max(.$nb_mechanics)), ",") %>% 
  select(-nb_mechanics) %>% 
  pivot_longer(!id, names_to = "mechanic", values_drop_na = TRUE) %>% 
  select(id, mechanic = value) %>% 
  mutate(mechanic = parse_character(mechanic, trim_ws = TRUE)) %>% 
  mutate(mechanic = str_sub(mechanic, 2, -2))

# Create designers dataset :
# 1) remove games w/o designer
# 2) select columns
# 3) remove "[" and "]" from strings
# 4) count number of designers per game
# 5) split designers into separate columns
# 6) transform into long format using pivot_longer

designers <- details %>% 
  filter(!is.na(designer)) %>% 
  select(id, designer) %>% 
  mutate(designer = str_sub(designer, 2, -2)) %>% 
  mutate(nb_designers = ifelse(is.na(designer), 0,
                               lengths(str_split(designer, ",")))) %>% 
  separate(designer, paste0("designer", 1:max(.$nb_designers)), ",") %>% 
  select(-nb_designers) %>% 
  pivot_longer(!id, names_to = "designer", values_drop_na = TRUE) %>% 
  select(id, designer = value) %>% 
  mutate(designer = parse_character(designer, trim_ws = TRUE)) %>% 
  mutate(designer = str_sub(designer, 2, -2)) %>% 
  filter(designer != "(Uncredited)")

# Create artists dataset :
# 1) remove games w/o artist
# 2) select columns
# 3) remove "[" and "]" from strings
# 4) count number of artists per game
# 5) split artists into separate columns
# 6) transform into long format using pivot_longer

artists <- details %>% 
  filter(!is.na(artist)) %>% 
  select(id, artist) %>% 
  mutate(artist = str_sub(artist, 2, -2)) %>% 
  mutate(nb_artists = ifelse(is.na(artist), 0,
                               lengths(str_split(artist, ",")))) %>% 
  separate(artist, paste0("designer", 1:max(.$nb_artists)), ",") %>% 
  select(-nb_artists) %>% 
  pivot_longer(!id, names_to = "artist", values_drop_na = TRUE) %>% 
  select(id, artist = value) %>% 
  mutate(artist = parse_character(artist, trim_ws = TRUE)) %>% 
  mutate(artist = str_sub(artist, 2, -2)) %>% 
  filter(artist != "(Uncredited)")

# Finish cleaning details dataset :
# 1) count number of categories, mechanics, designers & artists for each game
# 1) remove category, mechanic, designer & artist columns
# 2) add columns with counts for each of the removed columns

count_categories <- categories %>% 
  count(id) %>% 
  select(id, nb_categories = n)

count_mechanics <- mechanics %>% 
  count(id) %>% 
  select(id, nb_mechanics = n)

details <- details %>% 
  select(id:min_age) %>% 
  left_join(count_categories) %>% 
  left_join(count_mechanics)

# Join details and ratings
games <- details %>% 
  left_join(ratings)

# Clear global environment
rm(count_categories, count_mechanics,
   details, ratings)

# Does the number of mechanics in a game influence its minimum age ? ----

d1 <- games %>% 
  select(id, name, min_age, nb_mechanics) %>% 
  filter(!is.na(nb_mechanics)) %>% 
  group_by(nb_mechanics) %>% 
  summarise(mean = mean(min_age))

p1 <- ggplot(d1, aes(x = nb_mechanics, y = mean)) +
  geom_smooth(se = FALSE, size = 3, colour = "#004868") +
  ggtitle("Complexity") +
  labs(x = "Number of mechanics", y = "Minimum age") +
  xlim(c(1, 22)) +
  theme_minimal() +
  theme(axis.title.x = element_text(family = "poiret", colour = "white",size = 25, margin = margin(c(20, 0, 20, 0))),
        axis.title.y = element_text(family = "poiret", colour = "white", size = 25, margin = margin(c(0, 20, 0, 20))),
        axis.text = element_text(family = "poiret", colour = "white", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey30"),
        plot.title = element_text(family = "bangers", colour = "white", size = 35, hjust = 0.5,
                                  margin = margin(c(20, 0, 25, 0))),
        plot.background = element_rect(fill = "#292929", colour = NA),
        panel.background = element_rect(fill = "#292929", colour = NA))


# Does the play time influence a game's ratings ? ----

d2 <- games %>% 
  select(id, name, playing_time, average, bayes_average) %>% 
  group_by(playing_time) %>% 
  summarise(mean = mean(average)) %>% 
  filter(playing_time <= 300)


p2 <- ggplot(d2, aes(x = playing_time / 60, y = mean)) +
  geom_smooth(se = FALSE, size = 3, colour = "#077643") +
  ggtitle("Playing time") +
  labs(x = "Playing time (in hours)", y = "Average ratings") +
  theme_minimal() +
  theme(axis.title.x = element_text(family = "Dance", colour = "white",size = 25, margin = margin(c(20, 0, 20, 0))),
        axis.title.y = element_text(family = "Dance", colour = "white", size = 25, margin = margin(c(0, 20, 0, 20))),
        axis.text = element_text(family = "Dance", colour = "white", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey30"),
        plot.title = element_text(family = "Dance", colour = "white", size = 35, hjust = 0.5, margin = margin(c(20, 0, 25, 0))),
        plot.background = element_rect(fill = "#292929", colour = NA),
        panel.background = element_rect(fill = "#292929", colour = NA))
  
  
# How many games published each year ? ----

d3 <- games %>%
  count(year) %>% 
  filter(year <= 2021)

p3 <- ggplot(d3, aes(x = year, y = n)) +
  #geom_point(colour = "#c481aa", size = 4) +
  geom_line(colour = "#c481aa") +
  ggtitle("New games") +
  labs(x = "Year", y = "Number of games") +
  theme_minimal() +
  theme(axis.title.x = element_text(family = "Dance", colour = "white",size = 25, margin = margin(c(20, 0, 20, 0))),
        axis.title.y = element_text(family = "Dance", colour = "white", size = 25, margin = margin(c(0, 20, 0, 20))),
        axis.text = element_text(family = "Dance", colour = "white", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey30"),
        plot.title = element_text(family = "Dance", colour = "white", size = 35, hjust = 0.5, margin = margin(c(20, 0, 25, 0))),
        plot.background = element_rect(fill = "#292929", colour = NA),
        panel.background = element_rect(fill = "#292929", colour = NA))

# Create dataviz ----

patchwork <- (p1 +  p2) / (p3 + p3)

dataviz <- patchwork +
  plot_annotation(title = "Board games",
                  theme = theme(plot.title = element_text(size = 40, colour = "white",
                                                          family = "bangers", hjust = 0.5,
                                                          margin = margin(20, 0, 20, 0)),
                                plot.background = element_rect(fill = "#292929", colour = NA)))

# Save dataviz ----

ggsave("figs/2022_01_25_boardgames.png", dataviz,
       width = 1920/72, height = 1080/72, dpi = 72)

# Page title ----

ggplot() +
  ggtitle("Board games") +
  theme_minimal() +
  theme(plot.title = element_text(family = "bangers", colour = "white", size = 35, hjust = 0.5, margin = margin(c(20, 0, 25, 0))),
        plot.background = element_rect(fill = "#292929", colour = NA),
        panel.background = element_rect(fill = "#292929", colour = NA))

p1 + p2

(p1 |  p2) / (p3 | p3)

(p1 +  p2) / (p3 + p3)

# World map of producers ----

world <- map_data("world") %>% 
  dplyr::filter(region != "Antarctica") %>% 
  dplyr::left_join(producers)

labels <- tibble(region = c("Dominican Republic", "Ecuador", "Madagascar", "Peru", "Venezuela"),
                 x = c(-50, -95, 72, -85, -42),
                 y = c(23, -2, -18, -14, 9))

map <- ggplot() +
  geom_polygon(data = world,
               mapping = aes(x = long,
                             y = lat,
                             group = group,
                             fill = bin),
               colour = "grey80") +
  coord_fixed(1.3) +
  geom_text(data = labels, 
            mapping = aes(x = x, 
                          y = y,
                          label = region),
            family = "Poiret", colour = "black", size = 15) +
  scale_fill_manual(values = c("#faf8ec", "#c28954", "#8f5431",
                               "#603217", "#420c00", "#120a08"),
                    na.value = "#b39f80", na.translate = FALSE,
                    guide = guide_legend(nrow = 1, margin = margin(0, 0, 30, 0))) +
  ggtitle(label = "Top exporters of cocoa beans",
          subtitle = "colour indicates number of destination countries") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "#b39f80", colour = NA),
        plot.background = element_rect(fill = "#b39f80"),
        plot.title = element_text(family = "Poiret", hjust = 0.5,
                                  colour = "white", size = 60,
                                  margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(family = "Poiret", hjust = 0.5,
                                     colour = "white", size = 50),
        legend.title = element_blank(),
        legend.text = element_text(family = "Poiret", colour = "black",
                                   size = 35, margin = margin(l = -0.6, unit = "cm")),
        legend.spacing.x = unit(0.75, "cm"),
        legend.position = "bottom")

ggsave("figs/2022_01_18_chocolate_map.png", map, dpi = 320, width = 12, height = 6)





# Words describing low and high rated bars ----

word_count <- characteristics %>% 
  dplyr::count(characteristic, sort = T) %>% 
  dplyr::filter(n > 100)

word_levels <- word_count$characteristic

word_ratings <- characteristics %>% 
  dplyr::filter(characteristic %in% word_count$characteristic) %>% 
  dplyr::group_by(characteristic) %>% 
  dplyr::summarise(min_rating = min(rating),
                   max_rating = max(rating)) %>% 
  dplyr::mutate(characteristic = factor(characteristic,
                                        levels = word_levels))

ggplot() +
  geom_segment(data = word_ratings,
               mapping = aes(x = min_rating, xend = max_rating,
                             y = characteristic, yend = characteristic))

  geom_segment(data = ratings,
               mapping = aes(x = min_rating, xend = max_rating,
                             y = characteristic, yend = characteristic))

# Save figs ----
