# TidyTuesday challenge
# Date : 2022-01-18
# Chocolate bars
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-18/readme.md

# Links ----

# https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
#library(showtext)

# Import fonts ----

#font_add_google("Poiret One", "Poiret")
#showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate
rm(tuesdata)

# Data wrangling ----

# Clean chocolate dataset

chocolate_clean <- chocolate %>% 
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
  
  dplyr::mutate(cocoa_percent = parse_number(cocoa_percent)) %>%   # extract numeric value
  dplyr::select(id, company_location, country_of_bean_origin,
                cocoa_percent:most_memorable_characteristics, review_date, rating)
  
# Extract companies and beans countries
  
countries <- chocolate_clean %>% 
  dplyr::select(bean_country, company_country) %>% 
  dplyr::filter(bean_country != company_country) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(bean_country, company_country)


# Extract companies and beans countries
countries <- chocolate %>% 
  dplyr::select(company_country = company_location,
                bean_country = country_of_bean_origin) %>% 
  dplyr::arrange(company_country, bean_country) %>% 
  dplyr::mutate(company_country = case_when(company_country == "Amsterdam" ~ "Netherlands",
                                            company_country %in% c("Sao Tome", "Sao Tome & Principe") ~ "Sao Tome and Principe",
                                            company_country %in% c("Scotland", "U.K.", "Wales") ~ "UK",
                                            company_country == "St. Lucia" ~ "Saint Lucia",
                                            company_country == "St.Vincent-Grenadines" ~ "Saint Vincent",
                                            company_country == "U.A.E." ~ "United Arab Emirates",
                                            company_country == "U.S.A." ~ "USA",
                                            TRUE ~ company_country)) %>% 
  dplyr::mutate(bean_country = case_when(bean_country == "Congo" ~ "Republic of Congo",
                                         bean_country %in% c("Sulawesi", "Sumatra") ~ "Indonesia",
                                         bean_country == "DR Congo" ~ "Democratic Republic of the Congo",
                                         bean_country %in% c("Sao Tome", "Sao Tome & Principe", "Principe") ~ "Sao Tome and Principe",
                                         bean_country == "U.S.A" ~ "USA",
                                         bean_country == "Burma" ~ "Myanmar",
                                         bean_country == "St. Lucia" ~ "Saint Lucia",
                                         bean_country == "St.Vincent-Grenadines" ~ "Saint Vincent",
                                         TRUE ~ bean_country))

# Extract ingredients information
ingredients <- chocolate %>% 
  dplyr::select(id, ingredients) %>% 
  dplyr::mutate(nb_ingredients = parse_number(ingredients),
                list_ingredients = str_remove(ingredients, "[0-9]-")) %>% 
  tidyr::separate(list_ingredients, paste0("ingr", 1:7), ",") %>% 
  dplyr::select(-ingredients) %>% 
  tidyr::pivot_longer(!c(id, nb_ingredients),
                      names_to = "ingredients",
                      values_drop_na = TRUE) %>% 
  dplyr::select(id, nb_ingredients, ingredients = value) %>% 
  dplyr::mutate(ingredients = str_remove(ingredients, " ")) %>% 
  dplyr::mutate(ingredients = case_when(ingredients == "B" ~ "beans",
                                        ingredients == "S" ~ "sugar",
                                        ingredients == "S*" ~ "sweetener",
                                        ingredients == "C" ~ "cocoa_butter",
                                        ingredients == "V" ~ "vanilla",
                                        ingredients == "L" ~ "lecithin",
                                        ingredients == "Sa" ~ "Salt",
                                        TRUE ~ ingredients))

head(ingredients)

# World map ----

world <- map_data("world") %>% 
  filter(region != "Antarctica")

regions <- unique(world$region)

regions_diff <- !unique(countries$company_country) %in% regions
regions_diff <- unique(countries$company_country)[regions_diff]

# World tile grid map ----

world_tile_grid <- read_csv("data/worldtilegrid.csv")

ggplot() +
  geom_rect(data = world_tile_grid,
            mapping = aes(xmin = x, xmax = x + 1,
                          ymin = y, ymax = y + 1,
                          fill = region),
            colour = "white") +
  geom_text(data = world_tile_grid,
            mapping = aes(x = x,
                          y = y,
                          label = alpha.2),
            colour = "white",
            alpha = 0.5,
            nudge_x = 0.5,
            nudge_y = -0.5,
            size = 3) +
  scale_x_continuous(breaks = seq(1, 29, 1)) +
  #scale_y_continuous(breaks = seq(1, 24, 1)) +
  scale_y_reverse(breaks = seq(1, 24, 1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "black"))

ggplot(data = world_tile_grid,
       mapping = aes(x = x,
                     y = y)) +
  geom_text(mapping = aes(label = alpha.2))


# Save figs ----
ggsave("figs/2022_01_11_bees.png", p, dpi = 320, width = 12, height = 6) 