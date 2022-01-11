# TidyTuesday challenge
# Date : 2022-01-11
# Bee colonies
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md

# URLs ----

# https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
# https://rud.is/b/2015/05/14/geojson-hexagonal-statebins-in-r/

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
library(geojsonio)
library(broom)
library(rgeos)
library(gpclib)
library(maptools)
#library(RColorBrewer)
#library(rgdal)
#library(maps)
#library(showtext)
#library(ggrepel)
#library(gt)
#library(gtExtras)
#library(ggflags)

# Import fonts ----

#font_add_google("Poiret One", "Poiret")
#showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-01-11')
colony <- tuesdata$colony
stressor <- tuesdata$stressor

# Create empty hex map of US states ----

# Turn on the license
maptools::gpclibPermit()

us <- rgdal::readOGR("data/us_states_hexgrid.geojson")
plot(us)

us_map <- ggplot2::fortify(us, region = "iso3166_2")

ggplot(data=us_map, aes(map_id=id, x=long, y=lat)) + 
  geom_map(map=us_map, color="black", fill="white")

# Download the Hexagones boundaries at geojson format here:
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Show it
plot(spdf)

# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")



# Clean data ----

matches_cleaned <- matches %>% 
  rename(wickets_team1 = wickets_team2,
         wickets_team2 = wickets_team,
         balls_remaining = ball_remaining) %>% 
  mutate(balls_remaining = as.integer(str_extract(balls_remaining, "[0-9]+"))) %>% 
  mutate(match_date = str_remove(match_date, ",")) %>% 
  separate(match_date, " ", into = c("month", "day", "year")) %>% 
  separate(day, "-", into = c("day_start", "day_end")) %>% 
  mutate(day_end = case_when(is.na(day_end) ~ day_start,
                             TRUE ~ day_end)) %>% 
  mutate(month = match(month, month.abb)) %>% 
  mutate(match_date_start = lubridate::ymd(paste0(year, "-", month, "-", day_start)),
         match_date_end = lubridate::ymd(paste0(year, "-", month, "-", day_end))) %>% 
  select(match_id:ground_country, match_date_start, match_date_end)

rm(matches, tuesdata)
  
# Extract data from ICC World Cup 1999 ----

icc_wc_1999 <- matches_cleaned %>% 
  filter(series == "ICC World Cup",
         lubridate::year(match_date_start) == 1999)

rm(matches_cleaned)

# Create table of venues ----

venues <- icc_wc_1999 %>%
  distinct(venue) %>% 
  mutate(longitude = c(-0.1727, -0.164167, -3.1008, -2.226925, -2.584156,
                       0.469167, 1.091, -1.142686, -0.871206, -3.191389,
                       -1.560706, -6.207153, -0.115, -1.582222, -3.213,
                       -1.409986, -1.132161, 4.849058, -1.461103, -1.902489,
                       -2.286761),
         latitude = c(51.5294, 50.83, 51.019, 52.189225, 51.477225,
                      51.731667, 51.267, 52.607814, 52.248158, 51.487222,
                      54.849644, 53.368047, 51.483611, 53.816944, 55.961,
                      50.919494, 52.936883, 52.319444, 52.927506, 52.455814,
                      53.456347),
         city = sub('.*,\\s*', '', venue)) %>% 
  mutate(city = sub('.*,\\s*', '', venue)) %>% 
  mutate(city = case_when(city == "London" ~ venue,
                          TRUE ~ city)) %>% 
  mutate(city = str_remove(city, "Kennington ")) %>% 
  arrange(latitude, longitude) %>% 
  rowid_to_column("index") %>% 
  mutate(label = paste(index, city, sep = " - "),
         label_x = 2,
         label_y = seq(53, 57, 0.2))

# Create map of venues ----

northern_europe <- maps::map("world",
                             fill = TRUE,
                             plot = FALSE) %>% 
  maptools::pruneMap(xlim = c(-12, 6),
                     ylim = c(50, 57))

venues_map <- ggplot() +
  geom_polygon(data = northern_europe,
               mapping = aes(x = long, y = lat, group = group),
               fill = "#09131C", colour = "gray30") +
  coord_map() +
  geom_rect(mapping = aes(xmin = 0, xmax = 6,
                          ymin = 50, ymax = 50.5),
            fill = "#09131C", colour = "#09131C") +
  geom_rect(mapping = aes(xmin = 5.5, xmax = 6,
                          ymin = 50, ymax = 57),
            fill = "#09131C", colour = "#09131C") +
  geom_rect(mapping = aes(xmin = -12, xmax = 6,
                          ymin = 56.8, ymax = 57),
            fill = "#09131C", colour = "#09131C") +
  geom_point(data = venues,
             mapping = aes(x = longitude,
                           y = latitude,
                           alpha = .5),
             colour = "#0a9396", fill = "#005f73",
             size = 4,
             show.legend = FALSE) +
  geom_text_repel(data = venues,
                  mapping = aes(x = longitude, y = latitude, label = index),
                  family = "Poiret", size = 8,
                  colour = "white") +
  geom_text(data = venues, mapping = aes(x = label_x, y = label_y, label = label),
            size = 8, colour = "white", family = "Poiret", hjust = 0) +
  ggtitle(label = "ICC 1999 World Cup Venues") +
  labs(caption = "source : https://en.wikipedia.org/wiki/1999_Cricket_World_Cup") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#09131C"),
        plot.title = element_text(family = "Poiret", size = 50, hjust = 0.5, colour = "white"),
        plot.caption = element_text(family = "Poiret", size = 25, hjust = 0.5, vjust = 5, colour = "white"))

ggsave("figs/venues.png", dpi = 320, width = 12, height = 6)

# Create summary table of results ----

team1 <- icc_wc_1999 %>% 
  select(match_id, Team = team1, Score = score_team1, Wickets = wickets_team1, Winner = winner)

team2 <- icc_wc_1999 %>% 
  select(match_id, Team = team2, Score = score_team1, Wickets = wickets_team1, Winner = winner)

results <- rbind(team1, team2) %>% 
  mutate(Win = ifelse(Team == Winner, 1, 0),
         Draw = ifelse(Winner == "Match tied", 1, 0),
         Loss = ifelse(Team != Winner & Winner != "Match tied", 1, 0)) %>% 
  select(-Winner) %>% 
  arrange(match_id) %>% 
  group_by(Team) %>% 
  mutate("Matches" = n(),
         "Wins" = sum(Win),
         "Draws" = sum(Draw),
         "Losses" = sum(Loss),
         "Total Points" = sum(Score),
         "Total Wickets" = sum(Wickets)) %>% 
  mutate(Win = case_when(Draw == 1 ~ 0.5,
                         TRUE ~ Win))

outcomes <- results %>%
  arrange(match_id) %>%
  group_by(Team) %>% 
  summarise(Outcomes = list(Win), .groups = "drop")%>%
  select(Team, Outcomes)

teams_stats <- results %>% 
  filter(row_number() == 1) %>% 
  select(Team, Matches:`Total Wickets`) %>% 
  ungroup() %>% 
  left_join(outcomes) %>% 
  mutate(flag = case_when(
    Team == "Sri Lanka" ~ "https://flagicons.lipis.dev/flags/4x3/lk.svg",
    Team == "England" ~ "https://flagicons.lipis.dev/flags/4x3/gb-eng.svg",
    Team == "India" ~ "https://flagicons.lipis.dev/flags/4x3/in.svg",
    Team == "South Africa" ~ "https://flagicons.lipis.dev/flags/4x3/za.svg",
    Team == "Kenya" ~ "https://flagicons.lipis.dev/flags/4x3/ke.svg",
    Team == "Zimbabwe" ~ "https://flagicons.lipis.dev/flags/4x3/zw.svg",
    Team == "Scotland" ~ "https://flagicons.lipis.dev/flags/4x3/gb-sct.svg",
    Team == "Australia" ~ "https://flagicons.lipis.dev/flags/4x3/au.svg",
    Team == "Pakistan" ~ "https://flagicons.lipis.dev/flags/4x3/pk.svg",
    Team == "West Indies" ~ "https://upload.wikimedia.org/wikipedia/commons/1/18/WestIndiesCricketFlagPre1999.svg",
    Team == "Bangladesh" ~ "https://flagicons.lipis.dev/flags/4x3/bd.svg" ,
    Team == "New Zealand" ~ "https://flagicons.lipis.dev/flags/4x3/nz.svg"
  )) %>% 
  select(flag, everything())

rm(outcomes, results, team1, team2)

teams <- teams_stats %>% 
  arrange(desc(Matches), desc(Wins), desc(`Total Points`)) %>% 
  gt() %>% 
  gt_img_rows(flag) %>% 
  gt_plt_winloss(Outcomes,
                 colors = c("Green", "Red", "Grey"),
                 width = 50) %>% 
  cols_align(align = "center") %>% 
  cols_label(flag = "") %>% 
  tab_footnote("win (green), draw (grey) or loss (red)",
               locations = cells_column_labels(columns = Outcomes)) %>% 
  tab_header("ICC WORLD CUP 1999 TEAMS") %>% 
  tab_source_note("Data from ESPN Cricinfo by way of Hassanasir") %>% 
  tab_options(heading.title.font.size = 35,
              table.font.size = 20,
              column_labels.font.size = 25)

gtsave(teams, "figs/teams.png")