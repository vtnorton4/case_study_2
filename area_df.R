library(tidyverse)
#load data
load("~/shcool/case studies | STA 723/particleAdhesion.RData")
adhesion_original <- x

# remove constants and redundant cols
adhesion_small <- adhesion_original %>%
  select(-c(bins, removed, hamaker, density, workOfAdhesion))

# move "original" counts to rows with RPM 0
adhesion <- adhesion_small %>%
  group_by(site, sample, avgDiameter, type) %>%
  summarise(retained = unique(original), .groups = "drop") %>%
  mutate(rotation = 0) %>%
  rbind(select(adhesion_small, -original))


adhesion_area <- adhesion %>%
  mutate(
    diam = avgDiameter * retained, # total diameter for each size
    area = (avgDiameter)^2 * pi/4 * retained # area
    ) %>%
  group_by(site, sample, rotation, type) %>%
  summarise(
    total_diam = sum(diam),  # sum over sizes
    total_area = sum(area),
    retained = sum(retained),
    .groups = "drop"
  ) %>%
  mutate(   # calculate avg diam & total area
    avg_diam = case_when(retained == 0 ~ 0, TRUE ~ total_diam / retained),
    id = paste0(sample, site)
  )
