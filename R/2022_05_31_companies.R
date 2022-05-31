# TidyTuesday challenge
# Week : 22
# Date : 2022-05-31
# Axios-Harris Poll
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-05-31
# https://github.com/ricardo-bion/ggradar
# https://stackoverflow.com/questions/72205539/change-axis-title-text-color-in-ggradar

# Load packages ----

library(ggradar)
library(patchwork)
library(showtext)
library(tidytuesdayR)
library(tidyverse)


# Import fonts ----

font_add_google(name = "Rajdhani", family = "rajdhani")
showtext_auto()

# Import dataset ----

tuesdata <- tidytuesdayR::tt_load('2022-05-31')
poll <- tuesdata$poll
reputation <- tuesdata$reputation

# Data wrangling ----

# Extract GAFAM companies

gafam <- poll %>% 
  filter(company %in% c("Google", "Apple", "Facebook", "Amazon.com", "Microsoft")) %>% 
  group_by(company) %>% 
  filter(row_number() == 1) %>% 
  select(company, rank = `2022_rank`) %>% 
  mutate(company = case_when(company == "Amazon.com" ~ "Amazon",
                             TRUE ~ company))

# Wrangle reputation data

d1 <- reputation %>% 
  filter(company %in% gafam$company) %>% 
  select(company, name, score) %>% 
  mutate(score = score / 100,
         code = rep(LETTERS[1:7], 5),
         company = case_when(company == "Amazon.com" ~ "Amazon",
                             TRUE ~ company)) %>% 
  pivot_wider(id_cols = company,
              names_from = code,
              values_from = score)

legend <- reputation %>% 
  filter(company == "Amazon.com") %>% 
  mutate(code = LETTERS[1:7]) %>% 
  select(code, name) %>% 
  mutate(name = tolower(name)) %>% 
  mutate(name = case_when(name == "p&s" ~ "products/service",
                          TRUE ~ name)) %>% 
  mutate(y.pos = 7:1)

# Create legend ----

p0 <- ggplot() +
  geom_text(data = legend,
            aes(x = 0, y = y.pos, label = paste0(code, " : ", name)),
            hjust = 0, family = "rajdhani", size = 15, colour = "#a8dadc") +
  geom_rect(aes(xmin = -0.25, xmax = 4.5,
                ymin = 0, ymax = 8),
            fill = "#457b9d", colour = "#457b9d", size = 1, alpha = 0.25) +
  xlim(-0.5, 5) +
  ylim(0, 8) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.background = element_rect(fill = "#1d3557", colour = "#1d3557"))

# Create 1st radar plot ----
  
p1 <- d1 %>% 
  filter(company == gafam$company[1]) %>% 
  ggradar(values.radar = c("0", "50", "100"),
          font.radar = "rajdhani",
          grid.line.width = 0.2,
          label.gridline.min = FALSE,
          label.gridline.mid = FALSE,
          label.gridline.max = FALSE,
          gridline.max.colour = "#a8dadc",
          axis.line.colour = "#1d3557",
          axis.label.size = 10,
          fill = TRUE,
          fill.alpha = 0.5,
          group.colours = "#457b9d",
          group.line.width = 1,
          group.point.size = 0,
          background.circle.colour = "#1d3557",
          background.circle.transparency = 100,
          plot.title = paste0(gafam$company[1], " (", gafam$rank[1], "th)")) +
  theme(panel.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.title = element_text(family = "rajdhani", colour = "#a8dadc", hjust = 0.5, vjust = 0.5,
                                  size = 50))

p1$layers[[1]]$aes_params <- c(p1$layers[[1]]$aes_params, colour = "#a8dadc")
p1$layers[[5]]$aes_params <- c(p1$layers[[5]]$aes_params, colour = "#a8dadc")
p1$layers[[6]]$aes_params <- c(p1$layers[[6]]$aes_params, colour = "#a8dadc")

p1 <- p1



# Create 2nd radar plot ----

p2 <- d1 %>% 
  filter(company == gafam$company[2]) %>% 
  ggradar(values.radar = c("0", "50", "100"),
          font.radar = "rajdhani",
          grid.line.width = 0.2,
          label.gridline.min = FALSE,
          label.gridline.mid = FALSE,
          label.gridline.max = FALSE,
          gridline.max.colour = "#a8dadc",
          axis.line.colour = "#1d3557",
          axis.label.size = 10,
          fill = TRUE,
          fill.alpha = 0.5,
          group.colours = "#457b9d",
          group.line.width = 1,
          group.point.size = 0,
          background.circle.colour = "#1d3557",
          background.circle.transparency = 100,
          plot.title = paste0(gafam$company[2], " (", gafam$rank[2], "th)")) +
  theme(panel.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.title = element_text(family = "rajdhani", colour = "#a8dadc", hjust = 0.5, vjust = 0.5,
                                  size = 50))

p2$layers[[1]]$aes_params <- c(p2$layers[[1]]$aes_params, colour = "#a8dadc")
p2$layers[[5]]$aes_params <- c(p2$layers[[5]]$aes_params, colour = "#a8dadc")
p2$layers[[6]]$aes_params <- c(p2$layers[[6]]$aes_params, colour = "#a8dadc")

p2 <- p2

# Create 3rd radar plot ----

p3 <- d1 %>% 
  filter(company == gafam$company[3]) %>% 
  ggradar(values.radar = c("0", "50", "100"),
          font.radar = "rajdhani",
          grid.line.width = 0.2,
          label.gridline.min = FALSE,
          label.gridline.mid = FALSE,
          label.gridline.max = FALSE,
          gridline.max.colour = "#a8dadc",
          axis.line.colour = "#1d3557",
          axis.label.size = 10,
          fill = TRUE,
          fill.alpha = 0.5,
          group.colours = "#457b9d",
          group.line.width = 1,
          group.point.size = 0,
          background.circle.colour = "#1d3557",
          background.circle.transparency = 100,
          plot.title = paste0(gafam$company[3], " (", gafam$rank[3], "th)")) +
  theme(panel.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.title = element_text(family = "rajdhani", colour = "#a8dadc", hjust = 0.5, vjust = 0.5,
                                  size = 50))

p3$layers[[1]]$aes_params <- c(p3$layers[[1]]$aes_params, colour = "#a8dadc")
p3$layers[[5]]$aes_params <- c(p3$layers[[5]]$aes_params, colour = "#a8dadc")
p3$layers[[6]]$aes_params <- c(p3$layers[[6]]$aes_params, colour = "#a8dadc")

p3 <- p3

# Create 4th radar plot ----

p4 <- d1 %>% 
  filter(company == gafam$company[4]) %>% 
  ggradar(values.radar = c("0", "50", "100"),
          font.radar = "rajdhani",
          grid.line.width = 0.2,
          label.gridline.min = FALSE,
          label.gridline.mid = FALSE,
          label.gridline.max = FALSE,
          gridline.max.colour = "#a8dadc",
          axis.line.colour = "#1d3557",
          axis.label.size = 10,
          fill = TRUE,
          fill.alpha = 0.5,
          group.colours = "#457b9d",
          group.line.width = 1,
          group.point.size = 0,
          background.circle.colour = "#1d3557",
          background.circle.transparency = 100,
          plot.title = paste0(gafam$company[4], " (", gafam$rank[4], "th)")) +
  theme(panel.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.title = element_text(family = "rajdhani", colour = "#a8dadc", hjust = 0.5, vjust = 0.5,
                                  size = 50))

p4$layers[[1]]$aes_params <- c(p4$layers[[1]]$aes_params, colour = "#a8dadc")
p4$layers[[5]]$aes_params <- c(p4$layers[[5]]$aes_params, colour = "#a8dadc")
p4$layers[[6]]$aes_params <- c(p4$layers[[6]]$aes_params, colour = "#a8dadc")

p4 <- p4


# Create 5th radar plot ----

p5 <- d1 %>% 
  filter(company == gafam$company[5]) %>% 
  ggradar(values.radar = c("0", "50", "100"),
          font.radar = "rajdhani",
          grid.line.width = 0.2,
          label.gridline.min = FALSE,
          label.gridline.mid = FALSE,
          label.gridline.max = FALSE,
          gridline.max.colour = "#a8dadc",
          axis.line.colour = "#1d3557",
          axis.label.size = 10,
          fill = TRUE,
          fill.alpha = 0.5,
          group.colours = "#457b9d",
          group.line.width = 1,
          group.point.size = 0,
          background.circle.colour = "#1d3557",
          background.circle.transparency = 100,
          plot.title = paste0(gafam$company[5], " (", gafam$rank[5], "th)")) +
  theme(panel.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
        plot.title = element_text(family = "rajdhani", colour = "#a8dadc", hjust = 0.5, vjust = 0.5,
                                  size = 50))

p5$layers[[1]]$aes_params <- c(p5$layers[[1]]$aes_params, colour = "#a8dadc")
p5$layers[[5]]$aes_params <- c(p5$layers[[5]]$aes_params, colour = "#a8dadc")
p5$layers[[6]]$aes_params <- c(p5$layers[[6]]$aes_params, colour = "#a8dadc")

p5 <- p5

# Create patchwork ----

p <- p0 + p1 + p2 + p3 + p4 + p5 +
  plot_layout(ncol = 3) +
  plot_annotation(title = "GAFAM companies reputation",
                  subtitle = "2022 Axios-Harris Poll",
                  caption = "Visualisation : Jonathan Kitt | Data source : Axios-Harris Poll | #TidyTuesday 2022 week 2022",
                  theme = theme(
                    panel.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
                    plot.background = element_rect(fill = "#1d3557", colour = "#1d3557"),
                    plot.title = element_text(family = "rajdhani", colour = "#a8dadc", size = 75, hjust = 0.5,
                                              margin = margin(t = 20)),
                    plot.subtitle = element_text(family = "rajdhani", colour = "#a8dadc", size = 50, hjust = 0.5,
                                                 margin = margin(t = 10, b = 20)),
                    plot.caption = element_text(colour = "#a8dadc", size = 25, hjust = 0.5)))

# Save plot ----

ggsave("figs/2022_05_31_companies.png", p, dpi = 320, width = 12, height = 6)
