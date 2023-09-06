## Combine the survival probability by forest with the USFS Nation Forest Admin boundaries 

library(sf)
library(ggplot2)
library(ggmap)
library(tigris)

# Load the files to combine
forest_surv <- read_csv("data/processed/forest_surv.csv")
forest_boundary <- st_read('data/original/S_USA.AdministrativeForest.shp')
forest_boundary <- select(forest_boundary, -c(ADMINFORES, REGION, FORESTNUMB))

joined_df <- left_join(forest_boundary, forest_surv,
                       by = c("FORESTORGC" = "FOREST_ID"))


#st_write(joined_df, "data/processed/surv_prob.shp") # I think this file will be too big for github. 
# May have to have this script combine the data and make a map.

us_states <- states(cb = TRUE) %>%
  shift_geometry()

ggplot() +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1)

ggplot() +
  geom_sf(data = joined_df, aes(fill = SURV_1YR))
