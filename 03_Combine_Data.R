## Combine the survival probability by forest with the USFS Nation Forest Admin boundaries 

library(sf)

# Load the files to combine
forest_surv <- read_csv("data/processed/forest_surv.csv")
forest_boundary <- st_read('data/original/S_USA.AdministrativeForest.shp')
forest_boundary <- select(forest_boundary, -c(ADMINFORES, REGION, FORESTNUMB))

joined_df <- left_join(forest_boundary, forest_surv,
                       by = c("FORESTORGC" = "FOREST_ID"))
