

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

# Strip the "FOREST_ID=" string before the Forest ID and pad with a 0
forest_surv$FOREST_ID <- str_remove(forest_surv$FOREST_ID, "FOREST_ID=")
forest_surv$FOREST_ID <- str_pad(forest_surv$FOREST_ID, 2, pad = "0")

## save forest_surv as a .csv file to "data/processed/"
write_csv(forest_surv, "data/processed/forest_surv.csv")


