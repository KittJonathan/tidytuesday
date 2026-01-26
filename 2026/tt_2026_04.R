# TidyTuesday challenge
# https://github.com/rfordatascience/tidytuesday
# 2026 Week 4
# 2026-01-27
# Brazilian Companies

# Load packages ----

library(tidyverse)
library(tidytuesdayR)
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


         
# Plot ----


ggsave("2026/tt_2026_03.png", p, dpi = 320, height = 6, width = 12)