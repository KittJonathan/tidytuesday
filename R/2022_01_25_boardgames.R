# TidyTuesday challenge
# Date : 2022-01-25
# Board games
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-25/readme.md

# Load packages ----

#library(showtext)
library(tidytuesdayR)
library(tidyverse)


# Import fonts ----

#font_add_google("Poiret One", "Poiret")
#showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-01-25')
details <- tuesdata$details
ratings <- tuesdata$ratings
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
    dplyr::select(region = bean_country, bin) %>% 
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

ratings_nb_ingr <- ingredients %>% 
  dplyr::group_by(nb_ingr) %>% 
  dplyr::summarise(mean_rating = mean(rating)) %>% 
  dplyr::filter(!is.na(nb_ingr))

ggplot(data = ratings_nb_ingr) +
  geom_col(mapping = aes(x = nb_ingr,
                         y = mean_rating)) +
  theme_minimal()

ggplot() +
  geom_smooth(data = ingredients,
              mapping = aes(x = rating,
                            y = rating))

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

highest_rating_words <- characteristics %>% 
  dplyr::filter(rating >= 3.0) %>% 
  dplyr::count(characteristic, sort = T) %>% 
  dplyr::filter(characteristic != "cocoa") %>% 
  dplyr::top_n(10) %>% 
  dplyr::mutate(size = 10:1)

lowest_rating_words <- characteristics %>% 
  dplyr::filter(rating <= 2) %>% 
  dplyr::count(characteristic, sort = T) %>% 
  dplyr::filter(!characteristic %in% c("cocoa", "very bitter")) %>% 
  dplyr::top_n(10)

ggplot(data = highest_rating_words,
       mapping = aes(label = characteristic, size = size)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 40) +
  theme_minimal() +
  theme(l = element_text(family = "Poiret"))
  

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
