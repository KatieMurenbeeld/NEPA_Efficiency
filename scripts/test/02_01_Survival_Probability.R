

library(tidyverse)
library(broom)
library(survminer)
library(survival)
library(rms)

df <- read_csv("data/processed/pals_2009.csv")
# Note to self: may need to save REGION_ID and FOREST_ID as characters not numeric in 01_*.R

# Fit a kaplan-meier survival curve to the entire data set
fit_assess_all <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ 1, data = df)

# print out the summary
summary(fit_assess_all, time = c(365, 730))$surv

# Do I want to have ALL, 0.19019984, 0.06784094 as a row in the data frame?

# Fit a kaplan-meier survival curve to each forest
fit_assess_forest <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ FOREST_ID, data = df)

# Find the survival probability for each forest at time (t) duration of 1 year and 2 years 
forest_id_1yr <- data.frame(summary(fit_assess_forest, time = 365)$strata)
time_1yr <- data.frame(summary(fit_assess_forest, time = 365)$time)
surv_prob_1yr <- data.frame(summary(fit_assess_forest, time = 365)$surv)

forest_id_2yr <- data.frame(summary(fit_assess_forest, time = 730)$strata)
time_2yr <- data.frame(summary(fit_assess_forest, time = 730)$time)
surv_prob_2yr <- data.frame(summary(fit_assess_forest, time = 730)$surv)

# Combine into a data frame

forest_surv <- data.frame(forest_id_1yr, surv_prob_1yr, surv_prob_2yr) 

# Rename the columns
forest_surv <- forest_surv %>%
  rename(FOREST_ID = summary.fit_assess_forest..time...365..strata,
         SURV_1YR = summary.fit_assess_forest..time...365..surv,
         SURV_2YR = summary.fit_assess_forest..time...730..surv)

# Strip the "FOREST_ID=" string before the Forest ID and pad with 0
forest_surv$FOREST_ID <- str_remove(forest_surv$FOREST_ID, "FOREST_ID=")
forest_surv$FOREST_ID <- paste0("0", forest_surv$FOREST_ID)

## save forest_surv as a .csv file to "data/processed/"
write_csv(forest_surv, "data/processed/forest_surv.csv")

# Repeat at Region level
# Fit a kaplan-meier survival curve to each forest
fit_assess_region <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ REGION_ID, data = df)

# Find the survival probability for each forest at time (t) duration of 1 year and 2 years 
region_id_1yr <- data.frame(summary(fit_assess_region, time = 365)$strata)
time_1yr <- data.frame(summary(fit_assess_region, time = 365)$time)
surv_prob_1yr <- data.frame(summary(fit_assess_region, time = 365)$surv)

region_id_2yr <- data.frame(summary(fit_assess_region, time = 730)$strata)
time_2yr <- data.frame(summary(fit_assess_region, time = 730)$time)
surv_prob_2yr <- data.frame(summary(fit_assess_region, time = 730)$surv)

# Combine into a data frame

region_surv <- data.frame(region_id_1yr, surv_prob_1yr, surv_prob_2yr) 

# Rename the columns
region_surv <- region_surv %>%
  rename(REGION_ID = summary.fit_assess_region..time...365..strata,
         SURV_1YR = summary.fit_assess_region..time...365..surv,
         SURV_2YR = summary.fit_assess_region..time...730..surv)

# Strip the "REGION_ID=" string before the Region ID and pad with 0
region_surv$REGION_ID <- str_remove(region_surv$REGION_ID, "REGION_ID=")
region_surv$REGION_ID <- paste0("0", region_surv$REGION_ID)

## save forest_surv as a .csv file to "data/processed/"
write_csv(region_surv, "data/processed/region_surv.csv")

