# TidyTuesday challenge
# Date : 2022-02-12
# Freedom in the world
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-22/readme.md
# Load packages ----

#library(lubridate)
#library(showtext)
library(tidytuesdayR)
library(tidyverse)
#library(janitor)

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-02-22')
freedom <- tuesdata$freedom

rm(tuesdata)

illiteracy <- read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv')

# Number of "free" countries in 2020 per continent ----

continent_status <- freedom %>% 
  filter(year == 2020) %>% 
  select(continent = Region_Name, status = Status) %>% 
  group_by(continent) %>% 
  mutate(free_countries = sum(status == "F"),
         total = n()) %>% 
  filter(row_number() == 1) %>% 
  select(continent, free_countries, total)

  summarise(total = n())

  group_by(Region_Name) %>% 
  mutate(total = nrow(.))
  
  filter(year == 2020) %>% 
  count(Region_Name, Status) %>% 
  group_by(Region_Name) %>% 
  mutate(status_pct = 100 * Status / sum(n))

# Map ----

world <- map_data("world") %>% 
  dplyr::filter(region != "Antarctica")

# Save plot ----

ggsave("figs/2022_02_08_tuskegee_airmen.png", p, dpi = 320, width = 12, height = 6)
