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
  select(continent, free_countries, total) %>% 
  mutate(free_fraction = free_countries / total) %>% 
  mutate(non_free_fraction = 1 - free_fraction) %>% 
  select(continent, free_fraction, non_free_fraction) %>% 
  pivot_longer(-continent, names_to = "status", values_to = "fraction") %>% 
  mutate(status = str_remove(status, "_fraction")) %>% 
  group_by(continent) %>% 
  mutate(ymin = case_when(status == "non_free" ~ 1 - fraction,
                          TRUE ~ 0),
         ymax = case_when(status == "free" ~ fraction,
                          TRUE ~ 1))


# Plot ----

europe <- continent_status %>% 
  filter(continent == "Europe")

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


# Map ----

world <- map_data("world") %>% 
  dplyr::filter(region != "Antarctica")

# Save plot ----

ggsave("figs/2022_02_08_tuskegee_airmen.png", p, dpi = 320, width = 12, height = 6)
