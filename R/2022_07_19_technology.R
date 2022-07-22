# TidyTuesday challenge
# Week : 29
# Date : 2022-07-19
# Technology adoption over time
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-06-14

# Load packages ----

# library(showtext)
library(tidytuesdayR)
library(tidyverse)

# Import fonts ----

font_add_google(name = "Bebas Neue", family = "bebas")
font_add_google(name = "Teko", family = "teko")
font_add_google(name = "Righteous", family = "righteous")
showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-07-19')

technology <- tuesdata$technology

rm(tuesdata)

# Data wrangling ----

solar_global <- technology %>% 
  filter(year %in% c(2000, 2020),
         group == "Production",
         category == "Energy",
         grepl("TWH", label)) %>% 
  group_by(iso3c, year) %>% 
  mutate(total_prod_twh = max(value)) %>% 
  ungroup() %>% 
  filter(label == "Electricity from solar (TWH)") %>% 
  add_count(iso3c) %>% 
  filter(n == 2) %>% 
  mutate(share_pct = 100 * value / total_prod_twh) %>% 
  select(country = iso3c, year, share_pct) %>% 
  pivot_wider(id_cols = country,
              names_from = year,
              values_from = share_pct) %>% 
  rename(share_pct_2000 = `2000`,
         share_pct_2020 = `2020`) %>% 
  filter(!is.na(share_pct_2000), !is.na(share_pct_2020))

solar_france <- technology %>% 
  filter(iso3c == "FRA",
         year %in% 2000:2020,
         group == "Production",
         category == "Energy",
         grepl("TWH", label)) %>% 
  group_by(year) %>% 
  mutate(total_prod_twh = max(value)) %>% 
  ungroup() %>% 
  mutate(share_pct = 100 * value / total_prod_twh) %>% 
  filter(label == "Electricity from solar (TWH)") %>% 
  select(country = iso3c, year, share_pct)

head(solar_france)

ggplot(data = solar_france) +
  geom_point(aes(x = year, y = share_pct)) +
  geom_line(aes(x = year, y = share_pct))

%>% 
  group_by(iso3c, year) %>% 
  mutate(total_prod_twh = max(value)) %>% 
  ungroup() %>% 
  filter(label == "Electricity from solar (TWH)") %>% 
  add_count(iso3c) %>% 
  filter(n == 2) %>% 
  mutate(share_pct = 100 * value / total_prod_twh) %>% 
  select(country = iso3c, year, share_pct) %>% 
  pivot_wider(id_cols = country,
              names_from = year,
              values_from = share_pct) %>% 
  rename(share_pct_2000 = `2000`,
         share_pct_2020 = `2020`) %>% 
  filter(!is.na(share_pct_2000), !is.na(share_pct_2020))

wind_global <- technology %>% 
  filter(year %in% c(2000, 2020),
         group == "Production",
         category == "Energy",
         grepl("TWH", label)) %>% 
  group_by(iso3c, year) %>% 
  mutate(total_prod_twh = max(value)) %>% 
  ungroup() %>% 
  filter(label == "Electricity from wind (TWH)") %>% 
  add_count(iso3c) %>% 
  filter(n == 2) %>% 
  mutate(share_pct = 100 * value / total_prod_twh) %>% 
  select(country = iso3c, year, share_pct) %>% 
  pivot_wider(id_cols = country,
              names_from = year,
              values_from = share_pct) %>% 
  rename(share_pct_2000 = `2000`,
         share_pct_2020 = `2020`) %>% 
  filter(!is.na(share_pct_2000), !is.na(share_pct_2020))
    
head(solar_global) 

ggplot(data = wind_global) +
  geom_segment(aes(x = 0, xend = 1,
                   y = share_pct_2000, yend = share_pct_2020))

  
  filter(label == "Electricity from solar (TWH)",
         year %in% c(2000, 2020)) %>% 
  add_count(iso3c) %>% 
  filter(n == 2)

head(solar_global)

solar_global <- technology %>% 
  filter(year %in% c(2000, 2020),
         group == "Production",
         category == "Energy",
         grepl("TWH", label)) %>% 
  mutate(label = case_when(variable == "Electricity from solar (TWH)" ~ "Solar",
                           label == "Electricity from wind (TWH)" ~ "Wind",
                           TRUE ~ as.character(label))) %>% 
  select(energy = label, country = iso3c, year, prod_twh = value)

combi_count <- global %>% 
  count(energy, country)

head(global)

%>% 
  filter(!is.na(prod_twh)) %>% 
  group_by(energy, country, year) %>% 
  mutate(total_prod_twh = prod_twh[energy == "Gross output of electric energy (TWH)"])

head(global)

global <- technology %>% 
  filter(year %in% 2000:2020,
         group == "Production",
         category == "Energy",
         grepl("TWH", label)) %>% 
  group_by(year) %>% 
  mutate(total_prod = value[label == "Gross output of electric energy (TWH)"]) %>% 
  ungroup() %>% 
  mutate(share_pct = 100 * value / total_prod) %>% 
  filter(variable %in% c("elec_solar", "elec_wind")) %>% 
  select(energy_type = label, country = iso3c, year, share_pct) %>% 
  mutate(energy_type = case_when(energy_type == "Electricity from solar (TWH)" ~ "Solar",
                                 energy_type == "Electricity from wind (TWH)" ~ "Wind")) %>% 
  pivot_wider(id_cols = energy_type:country,
              names_from = year,
              values_from = share_pct)

%>% 
  filter(!is.na(`2020`))

ggplot() +
  geom_segment(data = filter(d1, energy_type == "Solar"),
            aes(x = 0, xend = 1,
                y = `2000`, yend = `2020`),
            colour = "grey") +
  geom_segment(data = filter(d1, energy_type == "Solar", country == "FRA"),
               aes(x = 0, xend = 1,
                   y = `2000`, yend = `2020`),
               colour = "blue")

ggplot() +
  geom_segment(data = filter(d1, energy_type == "Wind"),
               aes(x = 0, xend = 1,
                   y = `2000`, yend = `2020`),
               colour = "grey") +
  geom_segment(data = filter(d1, energy_type == "Wind", country == "FRA"),
               aes(x = 0, xend = 1,
                   y = `2000`, yend = `2020`),
               colour = "blue")
  
  

d1 <- energies %>% 
  group_by(iso3c, year) %>% 
  mutate(total_prod = value[label == "Gross output of electric energy (TWH)"])





g20_energ <- technology %>% 
  filter(iso3c %in% g20, year %in% 2000:2020,
         group == "Production", category == "Energy",
         grepl("TWH", label), variable != "elecprod")

ggplot() +
  geom_tile(data = g20_energ,
            aes(x = year, y = label, fill = value))

energies <- unique(g20_energ$label)

ggplot() +
  geom_line(data = filter(energy, label == energies[1]),
            aes(x = year, y = value, colour = iso3c),
            show.legend = FALSE) +
  scale_colour_manual(values = rep("grey", 208)) +
  geom_line(data = filter(energy, label == energies[1], iso3c == "FRA"),
            aes(x = year, y = value),
            colour = "blue")

ggplot() +
  geom_line(data = filter(g20_energ, label == energies[4]),
            aes(x = year, y = value, colour = iso3c),
            show.legend = FALSE) +
  scale_colour_manual(values = rep("grey", 209)) +
  geom_line(data = filter(g20_energ, label == energies[4], iso3c == "FRA"),
            aes(x = year, y = value),
            colour = "blue")

unique(energy$label)

energy_fr <- technology %>% 
  filter(iso3c == "FRA", group == "Production", category == "Energy",
         grepl("TWH", label), variable != "elecprod")

ggplot() +
  geom_line(data = energy_fr,
            aes(x = year, y = value, colour = label),
            show.legend = FALSE)


d1 <- technology %>% 
  filter(iso3c == "FRA", group == "Consumption", category == "Communications")

ggplot() +
  geom_line(data = filter(d1, label == "People with internet access"),
            aes(x = year, y = value, colour = label))

# Testing plots ----

cable <- technology %>% 
  filter(group == "Consumption" & category == "Communications") %>% 
  filter(label == "Households that subscribe to cable" & iso3c == "FRA")

cellular <- technology %>% 
  filter(group == "Consumption" & category == "Communications") %>% 
  filter(label == "Cellular subscriptions" & iso3c == "FRA")

computers <- technology %>% 
  filter(group == "Consumption" & category == "Communications") %>% 
  filter(label == "Personal computers" & iso3c == "FRA")

newspapers <- technology %>% 
  filter(group == "Consumption" & category == "Communications") %>% 
  filter(label == "Newspaper copies circulated daily" & iso3c == "FRA") 

internet_access <- technology %>% 
  filter(group == "Consumption" & category == "Communications") %>% 
  filter(label == "People with internet access" & iso3c == "FRA") 

plot(cable$year, cable$value)
plot(cellular$year, cellular$value)
plot(computers$year, computers$value)
plot(internet_access$year, internet_access$value, type = "b")



# Data wrangling ----

d1 <- technology %>% 
  filter(label %in% c("Personal computers", "People with internet access")) %>% 
  count(year, label)

ggplot(d1, aes(x = year, y = n)) + 
  geom_point(aes(colour = label)) +
  geom_line(aes(colour = label))

#####

d1 <- drought %>% 
  mutate(date = str_remove(DATE, "d_"),
         ymd = lubridate::ymd(date),
         year = lubridate::year(ymd)) %>% 
  select(year, D0:D4, W0:W4) %>% 
  pivot_longer(cols = -year, names_to = "condition", values_to = "value") %>% 
  group_by(year, condition) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% 
  mutate(mean_value = case_when(condition %in% c("W0", "W1", "W2", "W3", "W4") ~ -mean_value,
                                TRUE ~ mean_value))

# Create plot ----

p <- ggplot(data = filter(d1, year >= 2000),
       mapping = aes(x = year, y = mean_value, fill = condition)) +
  geom_area(show.legend = FALSE) +
  scale_fill_manual(values = c("#ecca00", "#ec9b00", "#ec5300", "#ec2400", "#ec0000",
                               "#b3cde0", "#6497b1", "#005b96", "#03396c", "#011f4b")) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(title = "Drought conditions in the U.S. (2000-2022)",
       subtitle = "Darker colours indicate more severe contitions",
       caption = "Visualisation : Jonathan Kitt | Data source : National Integrated Drought Information System | #TidyTuesday 2022 week 24") +
  geom_text(aes(x = 2002.25, y = 130, label = "2002 North-American drought"),
            family = "righteous", colour = "#ecca00", hjust = 0, size = 15) +
  geom_text(aes(x = 2018.5, y = -225, label = "2019 was the second wettest"),
            family = "righteous", colour = "#b3cde0", hjust = 1, size = 15) +
  geom_text(aes(x = 2018.5, y = -238, label = "year on record behind 1973"),
            family = "righteous", colour = "#b3cde0", hjust = 1, size = 15) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#969892", colour = "#969892"),
        plot.background = element_rect(fill = "#969892", colour = "#969892"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "bebas", colour = "white", size = 100, hjust = 0.5,
                                  margin = margin(t = 10)),
        plot.subtitle = element_text(family = "teko", colour = "white", size = 75, hjust = 0.5,
                                  margin = margin(t = 10, b = 5)),
        plot.caption = element_text(colour = "white", size = 25, hjust = 0.5,
                                    margin = margin(t = 5, b = 5)))

# Save plot ----

ggsave("figs/2022_06_14_drought.png", p2, dpi = 320, width = 12, height = 6)
