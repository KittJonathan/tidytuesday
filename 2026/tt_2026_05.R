# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 5
# 2026-02-03
# Edible Plants Database

# Load packages ----

library(tidyverse)
library(tidytuesdayR)
library(png)
library(grid)
library(sysfonts)

font_add_google("Roboto")
showtext::showtext_auto()


# Data ----

tuesdata <- tidytuesdayR::tt_load(2026, week = 5)

plants <- tuesdata$edible_plants |> 
  mutate(
    sunlight = case_when(sunlight == "partial shade" ~ "Partial shade",
                         sunlight == "full sun/partial shade/ full shade" ~ "Full sun/partial shade/full shade",
                         .default = sunlight),
    water = case_when(water == "high" ~ "High",
                      water == "Very low" ~ "Very Low",
                      water == "very high" ~ "Very High",
                      .default = water)
    ) |> 
  summarise(pH_min = min(preferred_ph_lower),
            pH_max = max(preferred_ph_upper),
            .by = water) |> 
  mutate(water = factor(water, levels = c("Very High", "High", "Medium",
                                          "Low", "Very Low")))

ph <- tibble(
  pH = rep(0:14, each = 5),
  xmin = rep(seq(-0.5, 13.5, 1), each = 5),
  xmax = rep(seq(0.5, 14.5, 1), each = 5),
  ymin = rep(seq(0.6, 4.6, 1), times = 15),
  ymax = rep(seq(1.4, 5.4, 1), times = 15))

# Plot ----

img1 = readPNG("2026/water_droplet.png")

p <- ggplot() +
  geom_rect(data = ph,
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax,
                fill = factor(pH)),
            color = "white",
            show.legend = FALSE) +
  scale_fill_manual(values = c("#e81f27", "#f26724", "#f8c514", "#f7ec1e", "#b4d433",
                               "#82c340", "#4db648", "#34aa4c", "#20b56a", "#0bb9b7",
                               "#4a8eca", "#3853a4", "#5b50a4", "#6743a1", "#462c82")) +
  geom_segment(data = plants,
               aes(x = pH_min, xend = pH_max,
                   y = water, yend = water),
               color = "black", linewidth = 1.2) +
  geom_point(data = plants,
             aes(x = pH_min, y = water),
             shape = 21,
             size = 10,
             fill = "white",
             stroke = 1.2) +
  geom_text(data = plants,
            aes(x = pH_min, y = water, label = pH_min),
            family = "Roboto", size = 10) +
  geom_point(data = plants,
           aes(x = pH_max, y = water),
           shape = 21,
           size = 10,
           fill = "white",
           stroke = 1.2) +
  geom_text(data = plants,
            aes(x = pH_max, y = water, label = pH_max),
            family = "Roboto", size = 10) +
  annotate(geom = "text", x = -3.7, y = 5, label = "Very low", hjust = 1, family = "Roboto", size = 20) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-1.5, xmax=-1, ymin=4.9, ymax=5.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2, xmax=-1.5, ymin=4.9, ymax=5.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2.5, xmax=-2, ymin=4.9, ymax=5.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3, xmax=-2.5, ymin=4.9, ymax=5.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3.5, xmax=-3, ymin=4.9, ymax=5.1) +
  annotate(geom = "text", x = -3.7, y = 4, label = "Low", hjust = 1, family = "Roboto", size = 20) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-1.5, xmax=-1, ymin=3.9, ymax=4.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2, xmax=-1.5, ymin=3.9, ymax=4.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2.5, xmax=-2, ymin=3.9, ymax=4.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3, xmax=-2.5, ymin=3.9, ymax=4.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3.5, xmax=-3, ymin=3.9, ymax=4.1) +
  annotate(geom = "text", x = -3.7, y = 3, label = "Medium", hjust = 1, family = "Roboto", size = 20) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-1.5, xmax=-1, ymin=2.9, ymax=3.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2, xmax=-1.5, ymin=2.9, ymax=3.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2.5, xmax=-2, ymin=2.9, ymax=3.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3, xmax=-2.5, ymin=2.9, ymax=3.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3.5, xmax=-3, ymin=2.9, ymax=3.1) +
  annotate(geom = "text", x = -3.7, y = 2, label = "High", hjust = 1, family = "Roboto", size = 20) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-1.5, xmax=-1, ymin=1.9, ymax=2.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2, xmax=-1.5, ymin=1.9, ymax=2.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2.5, xmax=-2, ymin=1.9, ymax=2.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3, xmax=-2.5, ymin=1.9, ymax=2.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3.5, xmax=-3, ymin=1.9, ymax=2.1) +
  annotate(geom = "text", x = -3.7, y = 1, label = "Very high", hjust = 1, family = "Roboto", size = 20) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-1.5, xmax=-1, ymin=0.9, ymax=1.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2, xmax=-1.5, ymin=0.9, ymax=1.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-2.5, xmax=-2, ymin=0.9, ymax=1.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3, xmax=-2.5, ymin=0.9, ymax=1.1) +
  annotation_custom(rasterGrob(img1, interpolate=TRUE), xmin=-3.5, xmax=-3, ymin=0.9, ymax=1.1) +
  geom_rect(aes(xmin = -3.05, xmax = -0.9, ymin = 4.8, ymax = 5.2),
            color = "white", fill = "white", alpha = 0.8) +
  geom_rect(aes(xmin = -2.55, xmax = -0.9, ymin = 3.8, ymax = 4.2),
            color = "white", fill = "white", alpha = 0.8) +
  geom_rect(aes(xmin = -2.05, xmax = -0.9, ymin = 2.8, ymax = 3.2),
            color = "white", fill = "white", alpha = 0.8) +
  geom_rect(aes(xmin = -1.55, xmax = -0.9, ymin = 1.8, ymax = 2.2),
            color = "white", fill = "white", alpha = 0.8) +
  scale_x_continuous(breaks = 0:14, limits = c(-5, 15)) +
  labs(title = "Plants with extreme water requirements have narrow preferred pH ranges",
       caption = "#TidyTuesday 2026 W05 | Jonathan Kitt | Edible Plants Database") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(family = "Roboto", size = 75, face = "bold", margin = margin(t = 10, b = 5),
                                  hjust = 0.5),
        plot.caption = element_text(family = "Roboto", size = 35, margin = margin(t = 20)),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(family = "Roboto", size = 24, margin = margin(t = -5)),
        axis.title.x = element_blank())

ggsave("2026/tt_2026_05.png", dpi = 320, width = 12, height = 6)
