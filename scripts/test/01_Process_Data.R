## Process and save the PALS data for use in a survival analysis

library(tidyverse) # used to manipulate data frames

## Read in the PALS data and forests shapefile
pals <- read.csv("data/original/pals_ongoing_projects_11-2022.csv", sep = ";")

# PALS: select desired columns/variables
pals_df <- select(pals, c(REGION_ID, FOREST_ID, PROJECT.NUMBER, PROJECT.NAME, PROJECT.STATUS,
                          PROJECT.CREATED, INITIATION.DATE, DECISION.SIGNED, DECISION.TYPE,
                          ELAPSED.DAYS, DECISION_LEVEL, ongoing))

# PALS: create a binary column to serve as the Event Occurring for the survival analysis. 
# Status of Completed = 1, every other status = 0. This will make In Progress and Cancelled projects = 0.
pals_df$EVENT <- ifelse(pals_df$PROJECT.STATUS == "Complete", 1, 0)

# PALS: create two more binary columns to denote which projects exceeded 1 year or 2 years
# Projects with NEPA assessments > 365 days = 1, <= 365 days = 0.
# Projects with NEPA assessments > 730 days = 1, <= 730 days = 0.
pals_df$LONGER_1YR <- ifelse(pals_df$ELAPSED.DAYS > 360, 1, 0)
pals_df$LONGER_2YR <- ifelse(pals_df$ELAPSED.DAYS > 730, 1, 0)

# PALS: replace NA with 0 for ongoing
pals_df$ongoing[is.na(pals_df$ongoing)] <- 0

# PALS: filter out experimental forests (FOREST_ID > 2400)
pals_df <-pals_df %>%
  filter(FOREST_ID < 2400)

# PALS: make REGION_ID and FOREST_ID a factor or character
pals_df$FOREST_ID <- as.character(pals_df$FOREST_ID)
pals_df$REGION_ID <- as.character(pals_df$REGION_ID)

# PALS: filter by year initiated >2009-01-01 and status = Complete
## First need to convert character strings to datetime or calendar time objects
pals_df$PROJECT.CREATED <- as.Date(pals_df$PROJECT.CREATED, format = "%m/%d/%Y")
pals_df$INITIATION.DATE <- as.Date(pals_df$INITIATION.DATE, format = "%m/%d/%Y")
pals_df$DECISION.SIGNED <- as.Date(pals_df$DECISION.SIGNED, format = "%m/%d/%Y")

## Then filter by year NEPA was initiated and (&) status = complete
## Also remove Test Project TEST and project 35233
pals2009_df <- pals_df %>%
  filter(INITIATION.DATE > "2009-01-01" & PROJECT.STATUS == "Complete") %>%
  filter(PROJECT.NAME != "Test Project TEST") %>%
  filter(PROJECT.NUMBER != 35233) %>%
  filter(ongoing == 0)

## save pals2009_df as a .csv file to "data/processed/"
write_csv(pals2009_df, "data/processed/pals_2009.csv")
