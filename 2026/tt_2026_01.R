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
  date = seq(from = ymd("2024-12-29"), 
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
    weekday = wday(date, label = TRUE, abbr = FALSE, locale = "en", week_start = 1),
    # week = week(date),
    week = case_when(date < "2025-01-01" ~ 0,
                     .default = week(date))
  )

git_2025

# Plot ----

p <- git_2025 |> 
  ggplot(aes(x = week, y = rev(weekday))) +
  geom_tile(aes(fill = contrib), width = 0.8, height = 0.8) +
  scale_fill_gradient2(low = "#033a16", mid = "#196d2e", high = "#57d364", midpoint = 3, na.value = "#151b22") +
  coord_fixed(ratio = 1, ) +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid = element_blank())

p

ggsave("2026/tt_2026_01.png", p, dpi = 320, width = 12, height = 6)
