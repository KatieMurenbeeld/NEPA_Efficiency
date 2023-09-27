library(tidyverse)


df <- read_csv("data/processed/pals_2009.csv")

longer1yr <- df %>%
  filter(LONGER_1YR == 1) %>%
  group_by(FOREST_ID) %>%
  summarise(total = sum(LONGER_1YR))

less1yr <- df %>%
  filter(LONGER_1YR == 0) %>%
  group_by(FOREST_ID) %>%
  count(LONGER_1YR)

longer2yr <- df %>%
  filter(LONGER_2YR == 1) %>%
  group_by(FOREST_ID) %>%
  summarise(total = sum(LONGER_2YR))

less2yr <- df %>%
  filter(LONGER_2YR == 0) %>%
  group_by(FOREST_ID) %>%
  count(LONGER_2YR)

n_proj <- df %>% count(FOREST_ID)

reg_for <- df %>% group_by(FOREST_ID) %>%
  summarise(region = mean(REGION_ID))
