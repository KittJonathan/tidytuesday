# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 4
# 2026-01-27
# Brazilian Companies

# Load packages ----

library(tidyverse)
library(tidytuesdayR)
# library(ggdist)
# library(sysfonts)

# font_add_google("Roboto")
# font_add_google("Orbitron")
# showtext::showtext_auto()
# library(patchwork)

# Data ----

tuesdata <- tidytuesdayR::tt_load(2026, week = 4)

companies <- tuesdata$companies
legal_nature <- tuesdata$legal_nature
qualifications <- tuesdata$qualifications
size <- tuesdata$size

companies |> 
  ggplot(aes(x = capital_stock,
             y = owner_qualification)) +
  geom_boxplot(outliers = F) +
  scale_x_log10()


companies |> 
  ggplot(aes(x = capital_stock, y = legal_nature)) +
  stat_slabinterval() +
  scale_x_log10()

companies |> 
  slice_max(order_by = capital_stock, n = 10, by = company_size) |> 
  ggplot(aes(x = company_size, y = capital_stock)) +
  geom_boxplot() +
  scale_y_log10()

         
# Plot ----

companies |> 
  summarise(capital_min = min(capital_stock),
            capital_median = median(capital_stock),
            capital_max = max(capital_stock),
            .by = legal_nature) |> 
  mutate(legal_nature = fct_reorder(.f = factor(legal_nature), 
                                    .x = capital_median)) |> 
  ggplot() +
  geom_point(aes(x = capital_median, y = legal_nature)) +
  geom_segment(aes(x = capital_min, xend = capital_max, y = legal_nature, yend = legal_nature)) +
  scale_x_log10()

ggplot(companies,
       aes(x = capital_stock, y = company_size)) +
  geom_boxplot()

companies |> 
  filter(!capital_stock %in% boxplot.stats(capital_stock)$out) |> 
  ggplot(aes(x = capital_stock, y = legal_nature)) +
  geom_boxplot()


ggsave("2026/tt_2026_03.png", p, dpi = 320, height = 6, width = 12)