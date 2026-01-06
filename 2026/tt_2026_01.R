# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 1
# 2026-01-06
# Bring you own data from 2025!

# Load packages ----

library(tidyverse)

# Data ----

# Github contributions in 2025

git_2025 <- tibble(
  date = seq(from = ymd("2025-01-01"), 
             to = ymd("2025-12-31"), by = "day")
  ) |> 
  mutate(
    contrib = case_when(date %in% c("2025-04-02", "2025-04-11", "2025-04-14", "2025-05-29", "2025-11-07") ~ 1,
                        date %in% c("2025-02-03", "2025-02-25", "2025-03-13", "2025-09-12", "2025-11-10") ~ 2,
                        date %in% c("2025-04-03", "2025-05-22") ~ 3,
                        date == "2025-05-27" ~ 4,
                        date %in% c("2025-05-23", "2025-09-15") ~ 5,
                        date == "2025-01-31" ~ 6,
                        .default = NA),
    weekday = wday(date, label = TRUE, abbr = FALSE, locale = "en"),
    weekday = fct_rev(weekday),
    week = case_when(
      row_number() <= 4 ~ 1,
      .default = (row_number() - 5) %/% 7 + 2)
    )
    # week = week(date),
    # week = case_when(date < "2025-01-01" ~ 0,
    #                  .default = week(date))

git_2025

# Plot ----

p <- git_2025 |> 
  ggplot(aes(x = week, y = weekday)) +
  geom_tile(aes(fill = contrib), width = 0.8, height = 0.8) +
  scale_fill_gradient2(low = "#033a16", mid = "#196d2e", high = "#57d364", midpoint = 3, na.value = "#151b22") +
  annotate(geom = "text", x = -1.5, y = 6, label = "Mon", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = -1.5, y = 4, label = "Wed", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = -1.5, y = 2, label = "Fri", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 0.5, y = 8.2, label = "Jan", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 5.5, y = 8.2, label = "Feb", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 9.5, y = 8.2, label = "Mar", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 14.5, y = 8.2, label = "Apr", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 18.5, y = 8.2, label = "May", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 22.5, y = 8.2, label = "Jun", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 27.5, y = 8.2, label = "Jul", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 31.5, y = 8.2, label = "Aug", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 36.5, y = 8.2, label = "Sep", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 40.5, y = 8.2, label = "Oct", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 44.5, y = 8.2, label = "Nov", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 49.5, y = 8.2, label = "Dec", colour = "white", hjust = 0, size = 5) +
  annotate(geom = "text", x = 0.5, y = 10.5, label = "41 contributions in 2025", 
           colour = "white", hjust = 0, size = 8) +
  annotate(geom = "text", x = 0.5, y = -1.5, label = "#TidyTuesday 2026 W01 | Jonathan Kitt", 
           colour = "white", hjust = 0, size = 4) +
  # labs(caption = "#TidyTuesday 2026 W01 | Jonathan Kitt") +
  coord_fixed(ratio = 1, ylim = c(-2, 11)) +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        # plot.margin = margin(t = 30, b = 30),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(colour = "white"),
        plot.margin = margin(b = -10),
        legend.position = "none")

p

ggsave("2026/tt_2026_01.png", p, dpi = 320, width = 12, height = 6)
