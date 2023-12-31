## Combine the survival probability by forest with the USFS Nation Forest Admin boundaries 
library(tidyverse)
library(sf)
library(readr)
library(ggplot2)
library(ggmap)
library(tigris)

# Load the files to combine
forest_surv <- read_csv("data/processed/forest_surv.csv")
region_surv <- read_csv("data/processed/region_surv.csv")
count <- read_csv("data/processed/long_nepa_count.csv")
count$FOREST_ID <- as.character(count$FOREST_ID)
count$FOREST_ID <- paste0("0", count$FOREST_ID)
forest_boundary <- st_read('data/original/S_USA.AdministrativeForest.shp')
forest_boundary <- select(forest_boundary, -c(ADMINFORES, REGION, FORESTNUMB))
region_boundary <- st_read("data/original/S_USA.AdministrativeRegion.shp")
region_boundary <- select(region_boundary, -c(ADMINREGIO))

forest_surv_df <- left_join(forest_boundary, forest_surv,
                       by = c("FORESTORGC" = "FOREST_ID")) %>%
  filter(FORESTORGC != "1004" & FORESTORGC != "1005" & FORESTORGC != "0816")

region_surv_df <- left_join(region_boundary, region_surv,
                       by = c("REGION" = "REGION_ID")) %>%
  filter(REGION != "10")

count_df <- left_join(forest_boundary, count,
                      by = c("FORESTORGC" = "FOREST_ID")) %>%
  filter(FORESTORGC != "1004" & FORESTORGC != "1005" & FORESTORGC != "0816")

count_df <- count_df %>% 
  mutate(greater_1yr_percent = longer1_count / n) %>%
  mutate(greater_2yr_percent = longer2_count / n)


#st_write(joined_df, "data/processed/surv_prob.shp") # I think this file will be too big for github. 
# May have to have this script combine the data and make a map. 

us_states <- states(cb = TRUE) %>%
  filter(GEOID < "60") %>%
  filter(GEOID != "02") %>%
  filter(GEOID != "15") 
  #shift_geometry(preserve_area = TRUE,
  #               position = "outside")
ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1)

nf_surv1yr <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = forest_surv_df, aes(fill = SURV_1YR), size = 0.05) + 
  scale_fill_gradient(low = "white", high = "forestgreen")
ggsave("nf_surv1yr.png", nf_surv1yr, width = 12, height = 12, dpi = 300)
dev.off()

nf_count1yr <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = count_df, aes(fill = greater_1yr_percent), size = 0.05) + 
  scale_fill_gradient(low = "white", high = "forestgreen")
ggsave("nf_count1yr.png", nf_count1yr, width = 12, height = 12, dpi = 300)

nf_count2yr <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = count_df, aes(fill = greater_2yr_percent), size = 0.05) + 
  scale_fill_gradient(low = "white", high = "forestgreen")
ggsave("nf_count2yr.png", nf_count2yr, width = 12, height = 12, dpi = 300)

nf_surv2yr <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = forest_df, aes(fill = SURV_2YR), size = 0.05)+ 
  scale_fill_gradient(low = "white", high = "forestgreen")
ggsave("nf_surv2yr.png", nf_surv2yr, width = 12, height = 12, dpi = 300)
dev.off()

reg_surv1yr <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = region_df, aes(fill = SURV_1YR), size = 0.05) + 
  scale_fill_gradient(low = "white", high = "forestgreen")
ggsave("reg_surv1yr.png", reg_surv1yr, width = 12, height = 12, dpi = 300)
dev.off()

reg_surv2yr <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = region_df, aes(fill = SURV_2YR), size = 0.05) + 
  scale_fill_gradient(low = "white", high = "forestgreen")
ggsave("reg_surv2yr.png", reg_surv2yr, width = 12, height = 12, dpi = 300)
dev.off()