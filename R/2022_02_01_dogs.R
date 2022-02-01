# TidyTuesday challenge
# Date : 2022-02-01
# Dog breeds
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-01/readme.md

# Load packages ----

#library(patchwork)
#library(showtext)
library(tidytuesdayR)
library(tidyverse)


# Import fonts ----

#font_add_google("Bangers", "bangers")
#font_add_google("Poiret One", "poiret")

# Import datasets ----

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

# Explore datasets ----

glimpse(breed_rank)
glimpse(breed_traits)
glimpse(trait_description)

head(breed_rank)
head(breed_traits)
head(trait_description)

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

# New games ----

d1 <- games %>% 
  filter(year >= 1950 & year <= 2021) %>% 
  count(year)
  

p1 <- ggplot(d1, aes(x = year, y = n)) +
  geom_line(colour = "firebrick", size = 2) +
  ggtitle("New games") +
  labs(x = "Year", y = "Number of games") +
  theme_minimal() +
  theme(axis.title.x = element_text(family = "poiret", colour = "white",size = 25, margin = margin(c(20, 0, 20, 0))),
        axis.title.y = element_text(family = "poiret", colour = "white", size = 25, margin = margin(c(0, 20, 0, 20))),
        axis.text = element_text(family = "poiret", colour = "white", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey30"),
        plot.title = element_text(family = "bangers", colour = "white", size = 25, hjust = 0.5, margin = margin(c(20, 0, 25, 0))),
        plot.background = element_rect(fill = "#292929", colour = NA),
        panel.background = element_rect(fill = "#292929", colour = NA))

# Game categories ----

d2 <- categories %>% 
  count(category, sort = TRUE) %>% 
  head(5) %>% 
  mutate(pct = round(n / sum(n) * 100)) %>% 
  mutate(category = fct_reorder(category, n))

p2 <- ggplot(d2, mapping = aes(x = pct, y = category, fill = category)) +
  geom_col(show.legend = FALSE) +
  ggtitle("Top 5 categories (%)") +
  scale_fill_manual(values = c("#836AA6", "#F299B1",
                               "#F2E291", "#A67C2E", "#F29985")) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "poiret", size = 20, colour = "white"),
        axis.text.y = element_text(margin = margin(0, -5, 0, 10)),
        panel.background = element_rect(fill = "#292929", colour = NA),
        plot.background = element_rect(fill = "#292929", colour = NA),
        plot.title = element_text(family = "bangers", colour = "white", size = 25, hjust = 0.5, margin = margin(c(20, 0, 25, 0))),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey30"))

# Game complexity ----

d3 <- games %>% 
  select(id, name, min_age, nb_mechanics) %>% 
  filter(!is.na(nb_mechanics)) %>% 
  group_by(nb_mechanics) %>% 
  summarise(mean = mean(min_age))

p3 <- ggplot(d3, aes(x = nb_mechanics, y = mean)) +
  geom_smooth(se = FALSE, size = 2, colour = "royalblue2") +
  ggtitle("Complexity") +
  labs(x = "Number of mechanics", y = "Minimum age") +
  xlim(c(1, 22)) +
  scale_y_continuous(breaks = c(9, 11, 13)) +
  theme_minimal() +
  theme(axis.title.x = element_text(family = "poiret", colour = "white",size = 25, margin = margin(c(20, 0, 20, 0))),
        axis.title.y = element_text(family = "poiret", colour = "white", size = 25, margin = margin(c(0, 20, 0, 20))),
        axis.text = element_text(family = "poiret", colour = "white", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey30"),
        plot.title = element_text(family = "bangers", colour = "white", size = 25, hjust = 0.5,
                                  margin = margin(c(20, 0, 25, 0))),
        plot.background = element_rect(fill = "#292929", colour = NA),
        panel.background = element_rect(fill = "#292929", colour = NA))


# Game duration ----

d4 <- games %>% 
  select(id, name, playing_time, average, bayes_average) %>% 
  group_by(playing_time) %>% 
  summarise(mean = mean(average)) %>% 
  filter(playing_time <= 300)


p4 <- ggplot(d4, aes(x = playing_time / 60, y = mean)) +
  geom_smooth(se = FALSE, size = 3, colour = "#077643") +
  ggtitle("Duration") +
  labs(x = "Hours", y = "Rating") +
  scale_y_continuous(breaks = c(6.5, 7)) +
  theme_minimal() +
  theme(axis.title.x = element_text(family = "poiret", colour = "white",size = 25, margin = margin(c(20, 0, 20, 0))),
        axis.title.y = element_text(family = "poiret", colour = "white", size = 25, margin = margin(c(0, 20, 0, 20))),
        axis.text = element_text(family = "poiret", colour = "white", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey30"),
        plot.title = element_text(family = "bangers", colour = "white", size = 25, hjust = 0.5, margin = margin(c(20, 0, 25, 0))),
        plot.background = element_rect(fill = "#292929", colour = NA),
        panel.background = element_rect(fill = "#292929", colour = NA))

# Create dataviz ----

board_games <- (p1 + p2) / (p3 + p4) +
  plot_annotation(title = "Board games since 1950",
                  caption = "Source : Board Game Geek, Graphic : Jonathan Kitt",
                  theme = theme(plot.title = element_text(size = 50, colour = "white",
                                                          family = "bangers", hjust = 0.5,
                                                          margin = margin(20, 0, 25, 0)),
                                plot.caption = element_text(size = 15, colour = "white", hjust = 1,
                                                            family = "poiret"),
                                plot.background = element_rect(fill = "#292929", colour = NA))) 

ggsave("figs/2022_01_25_boardgames.png", board_games,
       width = 1920/72, height = 1080/72, dpi = 72)