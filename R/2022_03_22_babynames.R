# TidyTuesday challenge
# Week : 12
# Date : 2022-03-22
# Baby names
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-22/readme.md

# Load packages ----

library(tidytuesdayR)
library(tidyverse)
# library(lubridate)
library(showtext)
library(patchwork)

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-03-22')

babynames <- tuesdata$babynames

rm(tuesdata)

# Load fonts ----

# font_add_google(name = "Kodchasan", family = "Kodchasan")
# showtext_auto()

# Data wrangling ----

babynames <- babynames %>% 
  mutate(code = paste(sex, name, sep = "_")) %>% 
  select(year, code, everything())

prop_change <-  babynames %>% 
  filter(year >= 2012) %>% 
  group_by(code) %>% 
  mutate(number_years = length(year)) %>% 
  ungroup() %>% 
  filter(number_years == 5) %>% 
  group_by(code) %>% 
  mutate(prop_change = prop - prop[year == 2012])

top5_increase_f <- prop_change %>% 
  filter(sex == "F", year == 2017) %>% 
  arrange(desc(prop_change)) %>% 
  head(5) %>% 
  ungroup() %>% 
  select(code) %>% 
  left_join(prop_change)

top5_decrease_f <- prop_change %>% 
  filter(sex == "F", year == 2017) %>% 
  arrange(prop_change) %>% 
  head(5) %>% 
  ungroup() %>% 
  select(code) %>% 
  left_join(prop_change)

prop_change_f <- rbind(top5_increase_f, top5_decrease_f) %>% 
  mutate(name = factor(name, levels = c("Charlotte", "Harper", "Amelia", "Aria", "Scarlett",
                                        "Ashley", "Emily", "Alyssa", "Alexis", "Madison")))

ggplot(prop_change_f, aes(x = year, y = prop_change, fill = name)) +
  geom_area(stat = "identity")

  scale_x_continuous(breaks = seq(2008, 2017, 1)) +
  scale_fill_manual(values = c("#62021e", "#a42153", "#c5558e", "#d483ac", "#e6a6c6",
                               "#62021e", "#a42153", "#c5558e", "#d483ac", "#e6a6c6"))

ggplot() +
  geom_line(data = prop_change_f,
            aes(x = year, y = prop_change, colour = name)) +
  geom_text(data = prop_change_f %>% filter(year == 2017),
            aes(x = 2018, y = prop_change, label = name))

  scale_x_continuous(breaks = seq(2008, 2017, 1)) 
  scale_colour_manual(values = c("#62021e", "#a42153", "#c5558e", "#d483ac", "#e6a6c6")


prop_diff_last_ten_years <-

top5_decrease_f <- prop_diff_last_ten_years %>% 
  filter(sex == "F", year == 2017) %>% 
  arrange(prop_change_vs_2008) %>% 
  head(5)



top10_f <- rbind(top5_decrease_f, top5_increase_f)

test <- prop_diff_last_ten_years %>% 
  filter(code %in% top5_decrease_f$code)

test2 <- prop_diff_last_ten_years %>% 
  filter(code %in% top5_increase_f$code)

test3 <- prop_diff_last_ten_years %>% 
  filter(code %in% top10_f$code)

ggplot() +
  geom_area(test3, aes(x = year, y = prop_change_vs_2008, fill = name)) +
  geom_line(test3, aes(x = year, y = prop_change_vs_2008), colour = "white")

ggplot(test3, aes(x = year, y = prop_change_vs_2008, fill = name)) +
  geom_area()


prop_diff_last_ten_years

top5_f_2017 <- d1 %>% 
  filter(sex == "F", year == 2017) %>% 
  arrange(desc(prop)) %>% 
  head(5) %>% 
  pull(code)

test <- d1 %>% 
  filter(code %in% top5_f_2017)

ggplot(test, aes(x = year, y = prop, colour = name)) +
  geom_smooth(se = FALSE)

ggplot(test) +
  geom_area(aes(x = year, y = prop, fill = name)) +
  xlim(1880, 2017)


###

babynames_clean <- babynames %>% 
  mutate(code = paste(sex, name, sep = "_")) %>% 
  select(year, code, everything())

names_1968_2017 <- babynames_clean %>% 
  filter(year %in% 1968:2017) %>% 
  count(code) %>% 
  filter(n == 50)

d1 <- babynames_clean %>% 
  filter(year %in% 1968:2017, code %in% names_1968_2017$code) %>% 
  arrange(code, year) %>% 
  mutate(prop_diff = prop - prop[year == 1968]) %>% 
  filter(year == 2017) %>% 
  select(code, sex, name, prop_diff)

top5_m_increase <- d1 %>% 
  filter(sex == "M") %>% 
  arrange(desc(prop_diff)) %>% 
  head(5)

%>% 
  group_by(code, sex, name) %>% 
  summarise(change = prop[which.max(year) - prop[which.min(year)]])

top5_decrease_m <- d1 %>% 
  filter(sex == "M") %>% 
  arrange(change) %>% 
  head(5)

top5_increase_f <- d1 %>% 
  filter(sex == "F") %>% 
  arrange(desc(change)) %>% 
  head(5)

test_f <- babynames_clean %>% 
  filter(code %in% top5_increase_f$code,
         year %in% 1918:2017)

ggplot(test_f, aes(x = year, y = prop, colour = name)) +
  geom_line()

  group_by(code) %>% 
  mutate(prop_diff = prop - lag(prop)) %>% 
  mutate(prop_diff = replace_na(prop_diff, 0)) %>% 
  mutate(nb_increase_years = length(which(prop_diff > 0)),
         nb_decrease_years = length(which(prop_diff < 0)))

d1_f <- d1 %>% 
  filter(nb_increase_years == 10 |  nb_decrease_years == 10)

test <- d1 %>% 
  filter(code == "F_Aaron") %>% 
  mutate(prop_diff = prop - lag(prop)) %>% 
  mutate(prop_diff = replace_na(prop_diff, 0))



d1 <- babynames_clean %>% 
  filter(code %in% names_1968_2017$code,
         year %in% 1968:2017)

prop_diff <- d1 %>% 
  filter(year %in% c(1968, 2017)) %>% 
  arrange(code, year) %>% 
  group_by(code) %>% 
  mutate(diff_prop = diff(prop)) %>% 
  filter(row_number() == 1)

top10_increase_m <- prop_diff %>% 
  filter(sex == "M") %>% 
  arrange(desc(diff_prop)) %>% 
  head(10)

test <- d1 %>% 
  filter(code %in% top10_increase_m$code)

ggplot(test, aes(x = year, y = n)) +
  geom_point(aes(colour = name))

d1 <- babynames_clean %>% 
  filter(year %in% c(1917, 2017)) %>% 
  count(code) %>% 
  filter(n == 2) %>% 
  left_join(babynames_clean, by = "code") %>% 
  filter(year %in% 1917:2017)

d2 <- babynames %>% 
  

  group_by(sex, name) %>% 
  mutate(prop_diff = .$prop[.$year == 2017] - .$prop[.$year == 1880])

top10_2017_m <- babynames %>% 
  filter(year == 2017, sex == "M") %>% 
  arrange(desc(prop)) %>% 
  head(10) %>% 
  pull(name)

top10_2017_f <- babynames %>% 
  filter(year == 2017, sex == "F") %>% 
  arrange(desc(prop)) %>% 
  head(10) %>% 
  pull(name)

babynames_m <- babynames %>% 
  filter(name %in% top10_2017_m)

ggplot(babynames_m, aes(x = year, y = n)) +
  geom_line(aes(colour = name))

name_count <- d1 %>% 
  count(sex, name, n) %>% 
  mutate(total = n * nn) %>% 
  group_by(sex, name) %>% 
  filter(row_number() == 1) %>% 
  select(sex, name, total)

top10_m <- name_count %>% 
  filter(sex == "M") %>% 
  arrange(desc(total)) %>% 
  head(10)

d1_m <- d1 %>% 
  filter(sex == "M", name %in% top10_m$name)

ggplot(data = d1_m) +
  geom_line(aes(x = year, y = n, colour = name))

ggplot(d1_m) +
  geom_area(aes(x = year, y = prop, fill = name))
  

head(top10_m)

new_pkgs_years <- bioc %>% 
  mutate(year = year(date)) %>% 
  group_by(package) %>% 
  mutate(first_release = min(year)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  count(first_release) %>% 
  filter(first_release > 1970)

time_of_day <- bioc %>% 
  mutate(hour = hour(date)) %>% 
  count(hour)

# Create plot ----

p1 <- ggplot(data = new_pkgs_years) +
  geom_point(aes(x = first_release, y = n),
             colour = "#b8d943", size = 4) +
  geom_line(aes(x = first_release, y = n),
            colour = "#b8d943", size = 0.75) +
  scale_x_continuous(breaks = seq(2001, 2021, 5)) +
  ggtitle(label = "Number of new packages released each year") +
  theme_minimal() +
  theme(panel.background = element_rect(colour = "#0194b5", fill = "#0194b5"),
        plot.background = element_rect(colour = "#0194b5", fill = "#0194b5"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey70"),
        axis.title = element_blank(),
        axis.text = element_text(colour = "white", family = "Kodchasan", size = 25),
        plot.title = element_text(family = "Kodchasan", colour = "white", size = 50,
                                  margin = margin(t = 0, b = 0)))

p2 <- ggplot(data = time_of_day) +
  geom_rect(aes(xmin = hour, xmax = hour + 1,
                ymin = 0, ymax = n),
            fill = "#b8d943", colour = "#0194b5") +
  scale_x_continuous(breaks = seq(0, 24, 1),
                     labels = c("12 pm", "1 am", "2 am", "3 am",
                                "4 am", "5 am", "6 am", "7 am", "8 am",
                                "9 am", "10 am", "11 am", "12 am",
                                "1 pm", "2 pm", "3 pm", "4 pm", "5 pm",
                                "6 pm", "7 pm", "8 pm", "9 pm", "10 pm", "11 pm", "12 pm")) +
  ggtitle(label = "Number of releases by time of day") +
  theme_minimal() +
  theme(panel.background = element_rect(colour = "#0194b5", fill = "#0194b5"),
        plot.background = element_rect(colour = "#0194b5", fill = "#0194b5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70"),
        panel.grid.major.x = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(colour = "white", family = "Kodchasan", size = 25),
        plot.title = element_text(family = "Kodchasan", colour = "white", size = 50,
                                  margin = margin(t = 0, b = 0)))
 
p <- p1 + p2 +
  plot_layout(ncol = 1) +
  plot_annotation(
    title = "Bioconductor packages",
    caption = "Visualisation: Jonathan Kitt | Data source: Robert M Flight | #TidyTuesday 2022 Week 11",
    theme = theme(plot.background = element_rect(fill = "#0194b5", colour = "#0194b5"),
                  panel.background = element_rect(fill = "#0194b5", colour = "#0194b5"),
                  plot.title = element_text(family = "Kodchasan", colour = "white", size = 120, hjust = 0.5,
                                            margin = margin(t = 20, b = 20)),
                  plot.caption = element_text(colour = "white", hjust = 0.5, size = 25,
                                              margin = margin(t = 20))))

# Save plot ----

ggsave("figs/2022_03_15_vignettes.png", p, dpi = 320, width = 12, height = 6)
