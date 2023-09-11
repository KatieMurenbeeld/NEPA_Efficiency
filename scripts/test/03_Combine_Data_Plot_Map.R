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
forest_boundary <- st_read('data/original/S_USA.AdministrativeForest.shp')
forest_boundary <- select(forest_boundary, -c(ADMINFORES, REGION, FORESTNUMB))
region_boundary <- st_read("data/original/S_USA.AdministrativeRegion.shp")
region_boundary <- select(region_boundary, -c(ADMINREGIO))

forest_df <- left_join(forest_boundary, forest_surv,
                       by = c("FORESTORGC" = "FOREST_ID"))

forest_df <- left_join(forest_boundary, forest_surv,
                       by = c("FORESTORGC" = "FOREST_ID"))


#st_write(joined_df, "data/processed/surv_prob.shp") # I think this file will be too big for github. 
# May have to have this script combine the data and make a map. 


us_states <- states(cb = TRUE) %>%
  filter(GEOID < "60") 
  #filter(GEOID != "02") %>%
  #filter(GEOID != "15") 
  #shift_geometry(preserve_area = TRUE,
  #               position = "outside")
ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1)

nf_surv1yr <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = joined_df, aes(fill = SURV_1YR), size = 0.05) + 
  scale_fill_gradient(low = "white", high = "forestgreen")
ggsave("nf_surv1yr.png", nf_surv1yr, width = 12, height = 12, dpi = 300)
dev.off()

nf_surv2yr <- ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  geom_sf(data = joined_df, aes(fill = SURV_2YR), size = 0.05)+ 
  scale_fill_gradient(low = "white", high = "forestgreen")
ggsave("nf_surv2yr.png", nf_surv2yr, width = 12, height = 12, dpi = 300)
dev.off()

