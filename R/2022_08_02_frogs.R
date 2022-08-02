# TidyTuesday challenge
# Week : 31
# Date : 2022-08-02
# Oregon spotted frog
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-08-02
# https://www.latlong.net/lat-long-utm.html
# https://stackoverflow.com/questions/67106215/sf-from-utm-to-latitude-longitude

# Load packages ----

# library(showtext)
# library(ggtext)
library(osmdata)
library(sf)
library(tidyverse)

# Import dataset ----

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frog.csv')

# Data wrangling ----

# To convert UTM coords to lat long : https://stackoverflow.com/questions/67106215/sf-from-utm-to-latitude-longitude

utm_coords <- frogs %>% 
  select(UTME_83, UTMN_83)

longlat <- st_as_sf(x = utm_coords,
                    coords = c("UTME_83", "UTMN_83"),
                    crs = "+proj=utm +zone=10") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  as_tibble()

list_long <- list()
list_lat <- list()

for (i in 1:nrow(longlat)) {
  list_long[[i]] <- longlat$geometry[[i]][1]
  list_lat[[i]] <- longlat$geometry[[i]][2]
}

longlat_coords <- tibble(
  long = unlist(list_long),
  lat = unlist(list_lat)
)



ggplot(longlat_coords,
       aes(x = long, y = lat)) +
  geom_point()

  mutate(geometry = str_remove_all(geometry, "("))
  separate(col = geometry, into = c("long", "lat"), sep = " ")

head(longlat)

  separate()


d1 <- frogs %>% 
  mutate()

utm_coords <- frogs %>% 
  select(UTME_83, UTMN_83)

coords_sf <- st_as_sf(x = utm_coords,
                      coords = c("UTME_83", "UTMN_83"),
                      crs = "+proj=utm +zone=10")

sfc <- st_transform(coords_sf, crs = "+proj=longlat +datum=WGS84")

sfc_tibble <- as_tibble(sfc)
sfc_tibble$geometry

# Map of Oregon ----

water <- opq("Oregon") %>% 
  add_osm_feature(key = "natural",
                  value = "water") %>% 
  osmdata_sf()

ggplot() +
  geom_sf(data = water$osm_polygons,
            inherit.aes = FALSE,
            colour = "blue", fill = "blue") +



# Import fonts ----

# font_add_google(name = "Jura", family = "Jura")
# showtext_auto()

# Import dataset ----

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frog.csv')

# Data wrangling ----

d1 <- frogs %>% 
  select(habitat = HabType,
         date = SurveyDate) %>% 
  mutate(date2 = lubridate::mdy(date)) %>% 
  mutate(week = lubridate::week(date2)) %>% 
  count(week, habitat)

%>% 
  filter(between(week, 38, 46))

reservoir <- frogs %>% 
  filter(HabType == "Reservoir")

# Create plot ----

ggplot(data = frogs,
       aes(x = UTME_83,
           y = UTMN_83)) +
  geom_point(aes(colour = Subsite))

ggplot(data = d1,
       aes(x = week, y = habitat)) +
  geom_density_ridges(aes(fill = habitat),
                      colour = "white", show.legend = FALSE) +
  scale_x_continuous(breaks = seq(37, 48, 1)) +
  scale_fill_manual(values = c("River" = "#000080",
                               "Reservoir" = "#3fe0d0",
                               "Pond" = "#008ecc")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#0b6623", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "#008631", linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "#0b6623", colour = NA),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white", margin = margin(l = 20)),
        axis.title.x = element_text(colour = "white", margin = margin(t = 20)),
        axis.title.y = element_blank())

ggplot(data = d1) +
  geom_tile(aes(x = date, y = habitat, fill = n),
            colour = "#033500", size = 0.25) +
  scale_fill_gradient(low = "#cefad0", high = "#008631") +
  # scale_x_continuous(breaks = seq(38, 46, 1)) +
  labs(x = "Week", y = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#033500", colour = NA),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#033500", colour = NA),
        axis.text = element_text(colour = "white"),
        axis.title.x = element_text(colour = "white"))

ggplot(d1,
       aes(x = week, y = n, colour = habitat)) +
  geom_line()

d1

%>% 
  count(date2, habitat)

ggplot() +
  geom_tile(data = d1,
            aes(x = week, y = habitat, fill = n))

  separate(col = date, into = c("month", "day", "year"), sep = "/") %>% 
  mutate(date = lubridate::ym(paste(year, month, sep = "-"))) %>% 
  count(date, habitat)

head(d1)

  mutate(year = lubridate::mdy(date))

%>% 
  mutate(date = lubridate::mdy(date))

%>% 
  mutate(date = lubridate::ym(date))

head(d1)
  

d1 <- technology %>% 
  filter(iso3c == "FRA",
         year %in% 2000:2020,
         group == "Production",
         category == "Energy",
         grepl("TWH", label)) %>% 
  group_by(year) %>% 
  mutate(total_prod_twh = max(value)) %>% 
  ungroup() %>% 
  mutate(share_pct = 100 * value / total_prod_twh) %>% 
  filter(variable != "elecprod") %>% 
  mutate(label = str_remove(label, "Electricity from ")) %>% 
  filter(variable != "elec_renew_other") %>% 
  mutate(label = str_remove_all(label, " \\(TWH\\)")) %>% 
  filter(label %in% c("wind", "solar", "hydro"))

# Create plot ----

(p <- ggplot() +
   geom_line(data = d1,
                aes(x = year, y = share_pct, colour = label),
             size = 1.5, show.legend = FALSE) +
   scale_colour_manual(values = c("solar" = "#f6be00",
                                  "hydro" = "#b3cde0",
                                  "wind" = "#35a79c")) +
   scale_y_continuous(sec.axis = dup_axis(),
                      labels = paste(seq(0, 15, 5), "%", sep = " ")) +
   labs(title = "Share of <span style = 'color: #b3cde0;'>hydroelectric</span>, <span style = 'color: #35a79c;'>wind</span> and <span style = 'color: #f6be00;'>solar energies</span>",
        subtitle = "in France between 2000 and 2020",
        caption = "Visualisation : Jonathan Kitt | Data source : data.nber.org | #TidyTuesday 2022 week 29") +
   theme_light() +
   theme(panel.background = element_rect(fill = "#011f4b", colour = NA),
         panel.grid.major = element_line(colour = "#005b96", linetype = "dashed"),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         plot.background = element_rect(fill = "#011f4b", colour = NA),
         plot.title = element_markdown(family = "Jura", size = 50, colour = "white", hjust = 0.5, margin = margin(t = 15, b = 5)),
         plot.subtitle = element_text(family = "Jura", size = 40, colour = "white", hjust = 0.5, margin = margin(b = 25)),
         plot.caption = element_text(family = "Jura", size = 25, colour = "white", hjust = 0.5, margin = margin(t = 25)),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         axis.text = element_text(family = "Jura", colour = "#005b96", size = 30))
)

# Save plot ----

ggsave("figs/2022_07_19.png", p, dpi = 320, width = 12, height = 6)
