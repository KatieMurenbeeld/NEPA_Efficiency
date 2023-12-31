library(tidyverse)


df <- read_csv("data/processed/pals_2009.csv")

longer1yr <- df %>%
  filter(LONGER_1YR == 1) %>%
  group_by(FOREST_ID) %>%
  summarise(longer1_count = sum(LONGER_1YR))

less1yr <- df %>%
  filter(LONGER_1YR == 0) %>%
  group_by(FOREST_ID) %>%
  count(LONGER_1YR, name = "less1_count") %>%
  select(-LONGER_1YR)

longer2yr <- df %>%
  filter(LONGER_2YR == 1) %>%
  group_by(FOREST_ID) %>%
  summarise(longer2_count = sum(LONGER_2YR))

less2yr <- df %>%
  filter(LONGER_2YR == 0) %>%
  group_by(FOREST_ID) %>%
  count(LONGER_2YR, name = "less2_count") %>%
  select(-LONGER_2YR)

n_proj <- df %>% count(FOREST_ID)

reg_for <- df %>% group_by(FOREST_ID) %>%
  summarise(region = mean(REGION_ID))

new_df <- left_join(reg_for, n_proj, by = "FOREST_ID") %>%
  left_join(longer1yr, by = "FOREST_ID") %>% 
  left_join(less1yr, by = "FOREST_ID") %>%
  left_join(longer2yr, by = "FOREST_ID") %>%
  left_join(less2yr, by = "FOREST_ID")

## save new_df as a .csv file to "data/processed/"
write_csv(new_df, "data/processed/long_nepa_count.csv")
