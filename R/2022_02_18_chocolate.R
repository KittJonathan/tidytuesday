# TidyTuesday challenge
# Date : 2022-01-18
# Chocolate bars
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-18/readme.md

# Links ----

# https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html

# Load packages ----

#library(ggwordcloud)
library(showtext)
library(tidytuesdayR)
library(tidyverse)


# Import fonts ----

font_add_google("Poiret One", "Poiret")
showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate
rm(tuesdata)

# Data wrangling ----

# Clean chocolate dataset

chocolate <- chocolate %>% 
  tibble::add_column(id = 1:nrow(.), .before = 1) %>%   # add an id column
  dplyr::mutate(company_country = case_when(company_location == "Amsterdam" ~ "Netherlands",
                                            company_location %in% c("Sao Tome", "Sao Tome & Principe") ~ "Sao Tome and Principe",
                                            company_location %in% c("Scotland", "U.K.", "Wales") ~ "UK",
                                            company_location == "St. Lucia" ~ "Saint Lucia",
                                            company_location == "St.Vincent-Grenadines" ~ "Saint Vincent",
                                            company_location == "U.A.E." ~ "United Arab Emirates",
                                            company_location == "U.S.A." ~ "USA",
                                            TRUE ~ company_location),
                bean_country = case_when(country_of_bean_origin == "Congo" ~ "Republic of Congo",
                                         country_of_bean_origin %in% c("Sulawesi", "Sumatra") ~ "Indonesia",
                                         country_of_bean_origin == "DR Congo" ~ "Democratic Republic of the Congo",
                                         country_of_bean_origin %in% c("Sao Tome", "Sao Tome & Principe", "Principe") ~ "Sao Tome and Principe",
                                         country_of_bean_origin == "U.S.A." ~ "USA",
                                         country_of_bean_origin == "Burma" ~ "Myanmar",
                                         country_of_bean_origin == "St. Lucia" ~ "Saint Lucia",
                                         country_of_bean_origin == "St.Vincent-Grenadines" ~ "Saint Vincent",
                                         TRUE ~ country_of_bean_origin),
                cocoa_percent = parse_number(cocoa_percent)) %>% 
  dplyr::select(id, company_manufacturer, company_country, review_date, bean_country,
                cocoa_percent:rating)

# Producers
  
producers <- chocolate %>% 
    dplyr::select(bean_country, company_country) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(bean_country) %>% 
    dplyr::count(sort = TRUE) %>% 
    dplyr::mutate(bin = case_when(n < 5 ~ "< 5",
                                  n >= 5 & n < 10 ~ "5-9",
                                  n >= 10 & n < 15 ~ "10-14",
                                  n >= 15 & n < 20 ~ "15-19",
                                  n >= 20 & n < 25 ~ "20-24",
                                  n >= 25 ~ "25 +")) %>%
    dplyr::select(region = bean_country, nb_dest = n, bin) %>% 
    dplyr::filter(region != "Blend") %>% 
    dplyr::mutate(bin = factor(bin,
                               levels = c("NA", "< 5", "5-9", "10-14",
                                          "15-19", "20-24", "25 +")))

# Ingredients 

unique_ids <- chocolate %>% 
  dplyr::select(id, company_country:cocoa_percent, rating, ingredients)

ingredients <- unique_ids %>% 
  dplyr::mutate(nb_ingr = parse_number(ingredients),
                list_ingr = str_remove(ingredients, "[0-9]-")) %>% 
  dplyr::mutate(list_ingr = str_remove(list_ingr, " ")) %>% 
  tidyr::separate(list_ingr, paste0("ingr", 1:7), ",") %>% 
  dplyr::select(-ingredients) %>% 
  tidyr::pivot_longer(!(c(id:nb_ingr)),
                        names_to = "ingr_number",
                        values_to = "ingredients",
                        values_drop_na = TRUE) %>% 
  dplyr::select(-ingr_number) %>% 
  dplyr::mutate(ingredients = case_when(ingredients == "B" ~ "beans",
                                        ingredients == "S" ~ "sugar",
                                        ingredients == "S*" ~ "sweetener",
                                        ingredients == "C" ~ "cocoa butter",
                                        ingredients == "V" ~ "vanilla",
                                        ingredients == "L" ~ "lecithin",
                                        ingredients == "Sa" ~ "salt",
                                        TRUE ~ ingredients))

ingr_ratings <- ingredients %>% 
  dplyr::group_by(nb_ingr) %>% 
  dplyr::summarise(median_rating = mean(rating))

ingr_ids <- unique(ingredients$id)

missing_ids <- unique_ids %>% 
  dplyr::filter(!id %in% ingr_ids) %>% 
  dplyr::mutate(nb_ingr = NA,
                ingredients = NA) %>% 
  dplyr::select(id:rating, nb_ingr, ingredients)

ingredients <- rbind(ingredients, missing_ids) %>% 
  dplyr::arrange(id)

rm(missing_ids, unique_ids, ingr_ids)

# Characteristics

characteristics <- chocolate %>% 
  dplyr::select(id, company_country:cocoa_percent, rating,
                characteristics = most_memorable_characteristics) %>% 
  tidyr::separate(characteristics, paste0("word", 1:5), ",") %>% 
  tidyr::pivot_longer(!(c(id:rating)),
                      names_to = "word_number",
                      values_to = "word",
                      values_drop_na = TRUE) %>% 
  dplyr::mutate(word = str_trim(word)) %>% 
  dplyr::select(id:rating, characteristic = word)

# World map of producers ----

world <- map_data("world") %>% 
  dplyr::filter(region != "Antarctica") %>% 
  dplyr::left_join(producers)

labels <- tibble(id = 1:5,
                 x = c(50, -80, -85, -58, -66),
                 y = c(-26, -14, -2, 10, 22))

labels <- tibble(region = c("Dominican Republic", "Ecuador", "Madagascar", "Peru", "Venezuela"),
                 x = c(-50, -95, 70, -85, -42),
                 y = c(23, -2, -26, -14, 9))

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
                          label = id),
            family = "Poiret", colour = "black", size = 12) +
  scale_fill_manual(values = c("#faf8ec", "#c28954", "#8f5431",
                               "#603217", "#420c00", "#120a08"),
                    na.value = "#b39f80", na.translate = FALSE,
                    guide = guide_legend(nrow = 1, margin = margin(0, 0, 30, 0))) +
  annotate("text", x = -160, y = -30, label = "1 - Madagascar", size = 12, family = "Poiret", hjust = 0) +
  annotate("text", x = -160, y = -35, label = "2 - Peru", size = 12, family = "Poiret", hjust = 0) +
  annotate("text", x = -160, y = -40, label = "3 - Ecuador", size = 12, family = "Poiret", hjust = 0) +
  annotate("text", x = -160, y = -45, label = "4 - Venezuela", size = 12, family = "Poiret", hjust = 0) +
  annotate("text", x = -160, y = -50, label = "5 - Dominican Republic", size = 12, family = "Poiret", hjust = 0) +
  ggtitle(label = "Top exporters of cocoa beans",
          subtitle = "colour indicates number of destination countries") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "#b39f80", colour = NA),
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

rm(labels, world)

ggsave("figs/2022_01_18_chocolate_map.png", map, dpi = 320, width = 12, height = 6)





# Ratings by description words ----

word_count <- characteristics %>% 
  dplyr::count(characteristic, sort = TRUE) %>% 
  dplyr::filter(n > 100) %>% 
  dplyr::select(word = characteristic, count = n) %>% 
  dplyr::mutate(word = fct_reorder(word, desc(count)))

word_ratings <- characteristics %>% 
  dplyr::filter(characteristic %in% word_count$word) %>% 
  dplyr::group_by(characteristic) %>% 
  dplyr::summarise(min_rating = min(rating),
                   median_rating = median(rating),
                   mean_rating = mean(rating),
                   max_rating = max(rating)) %>% 
  dplyr::select(word = characteristic, min_rating:max_rating) %>% 
  dplyr::mutate(word = factor(word, levels = levels(word_count$word))) %>% 
  dplyr::arrange(word) %>% 
  tibble::add_column(id = nrow(.):1, .before = 1)

add_column(id = 1:nrow(.), .before = 1)

ggplot() +
  geom_segment(data = word_ratings,
               mapping = aes(x = min_rating, xend = max_rating,
                             y = word, yend = word),
               colour = "#faf8ec", size = 1.5) +
  geom_point(data = word_ratings,
             mapping = aes(x = min_rating, y = word),
             colour = "#c28954", size = 5) +
  geom_point(data = word_ratings,
             mapping = aes(x = max_rating, y = word),
             colour = "#420c00", size = 5) +
  scale_y_discrete(limits = rev) +
  geom_segment(data = word_ratings,
               mapping = aes(x = mean_rating, xend = mean_rating,
                             y = id - 0.2, yend = id + 0.2),
               size = 1, colour = "#603217") +
  ggtitle(label = "Ratings for words most used in chocolate bars reviews ",
          subtitle = "words shown are present in at least 100 reviews") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80", linetype = "dotted"),
        panel.background = element_rect(fill = "#b39f80", colour = NA),
        plot.background = element_rect(fill = "#b39f80", colour = NA),
        plot.title = element_text(family = "Poiret", hjust = 0.5,
                                  colour = "white", size = 50,
                                  margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(family = "Poiret", hjust = 0.5,
                                     colour = "white", size = 30,
                                     margin = margin(5, 0, 10, 0)),
        axis.text.x = element_text(family = "Poiret", colour = "white",
                                   size = 15),
        axis.text.y = element_text(family = "Poiret", colour = "white",
                                   size = 20))

theme(panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_rect(fill = "#b39f80", colour = NA),
      plot.background = element_rect(fill = "#b39f80", colour = NA),
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




# Save figs ----
