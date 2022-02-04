# TidyTuesday challenge
# Date : 2022-02-01
# Dog breeds
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-01/readme.md

# Load packages ----

library(gt)
library(gtExtras)
library(tidytuesdayR)
library(tidyverse)

# Import datasets ----

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
breed_rank <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

# Data wrangling ----

# Clean ranks and calculate rank evolution since 2013

ranks <- breed_rank %>% 
  select(Breed, 
         Image,
         rank_2013 = '2013 Rank',
         rank_2014 = '2014 Rank',
         rank_2015 = '2015 Rank',
         rank_2016 = '2016 Rank',
         rank_2017 = '2017 Rank',
         rank_2018 = '2018 Rank',
         rank_2019 = '2019 Rank',
         rank_2020 = '2020 Rank') %>% 
  pivot_longer(cols = -c(Breed, Image), names_to = "year", values_to = "rank") %>% 
  mutate(year = str_remove(year, "rank_")) %>% 
  mutate(year = factor(year, levels = 2013:2020)) %>% 
  group_by(Breed) %>% 
  filter(year == 2020) %>% 
  select(rank, Breed, Image)

# Clean trait groups dataset with scores 

traits <- breed_traits %>% 
  mutate(rank = 1:nrow(.)) %>% 
  select(-c(`Coat Type`, `Coat Length`)) %>% 
  pivot_longer(cols = -c(rank, Breed), names_to = "trait", values_to = "value") %>% 
  mutate(trait_group = case_when(trait %in% c("Affectionate With Family",
                                              "Good With Young Children",
                                              "Good With Other Dogs") ~ "Family Life",
                                 trait %in% c("Shedding Level",
                                              "Coat Grooming Frequency",
                                              "Drooling Level") ~ "Physical",
                                 trait %in% c("Openness To Strangers",
                                              "Playfulness Level",
                                              "Watchdog/Protective Nature",
                                              "Adaptability Level") ~ "Social",
                                 trait %in% c("Trainability Level",
                                              "Energy Level",
                                              "Barking Level",
                                              "Mental Stimulation Needs") ~ "Personality")) %>% 
  select(rank, Breed, trait_group, trait, value) %>% 
  mutate(Breed = factor(Breed, levels = unique(Breed)),
         trait_group = factor(trait_group, levels = unique(trait_group)),
         trait = factor(trait, levels = unique(trait))) %>% 
  group_by(rank, trait_group) %>% 
  mutate(trait_group_score = sum(value),
         n = n()) %>% 
  mutate(score_100 = trait_group_score * 100 / (n * 5)) %>% 
  select(rank:trait_group, trait_group_score = score_100) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pivot_wider(names_from = trait_group, values_from = trait_group_score)

# Extract coat type and coat length

coat <- breed_traits %>% 
  mutate(rank = 1:nrow(.)) %>% 
  select(rank, Breed, `Coat Type`, `Coat Length`)

# Join datasets and add group information from AKC website 
# Keep Top 10 breeds for 2020

dog_breeds <- ranks %>% 
  left_join(coat, by = "rank") %>% 
  left_join(traits, by = "rank") %>% 
  filter(rank <= 10) %>% 
  mutate(group = c("Sporting", "Non-Sporting", "Herding", "Sporting",
                   "Non-Sporting", "Non-Sporting", "Hound", "Working",
                   "Sporting", "Hound")) %>% 
  select(rank, Image, Breed, group, `Coat Type`, `Coat Length`,
         `Family Life`, Physical, Social, Personality)

# Clean global environment

rm(breed_rank, breed_traits, coat, ranks, traits)

# Create table for top 10 breeds in 2020 ----

table <- dog_breeds %>% 
  gt() %>% 
  cols_label(rank = "RANK",
             Image = "",
             Breed = "BREED",
             `Coat Type` = "COAT TYPE",
             `Coat Length` = "COAT LENGTH",
             `Family Life` = "FAMILY LIFE",
             Physical = "PHYSICAL",
             Social = "SOCIAL",
             Personality = "PERSONALITY") %>% 
  gt_img_rows(Image) %>% 
  gt_color_rows(rank, palette = "ggsci::blue_material") %>% 
  gt_merge_stack(col1 = Breed, col2 = group) %>% 
  gt_plt_bar_pct(column = `Family Life`, scaled = TRUE,
                 fill = "blue", background = "lightblue") %>% 
  gt_plt_bar_pct(column = Physical, scaled = TRUE,
                 fill = "green", background = "lightgreen") %>% 
  gt_plt_bar_pct(column = Social, scaled = TRUE,
                 fill = "purple", background = "#e1e1e1") %>% 
  gt_plt_bar_pct(column = Personality, scaled = TRUE,
                 fill = "goldenrod", background = "gold") %>% 
  cols_align(align = "center") %>% 
  cols_width(Image ~ px(75),
             `Coat Type` ~ px(150),
             `Coat Length` ~ px(150),
             `Family Life` ~ px(100),
             Physical ~ px(100),
             Social ~ px(100),
             Personality ~ px(100)) %>% 
  tab_header(title = "Top 10 Dog Breeds in 2020") %>% 
  opt_align_table_header(align = "center") %>% 
  tab_source_note(source_note = "Source: American Kennel Club") %>% 
  tab_source_note(source_note = "@KittJonathan")

table