

library(tidyverse)
library(broom)
library(survminer)
library(survival)
library(rms)

df <- read_csv("data/processed/pals_2009.csv")
# Note to self: may need to save REGION_ID and FOREST_ID as characters not numeric in 01_*.R

fit_assess_all <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ 1, data = df)
fit_assess_summary <- fit_assess_all %>% tidy()

fit_assess_forest <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ FOREST_ID, data = df)
fit_forest_summary <- fit_assess_forest %>% tidy()

summary(fit_assess_forest, time = c(365, 730))$time

forest_id_1yr <- data.frame(summary(fit_assess_forest, time = 365)$strata)
time_1yr <- data.frame(summary(fit_assess_forest, time = 365)$time)
surv_prob_1yr <- data.frame(summary(fit_assess_forest, time = 365)$surv)

forest_id_2yr <- data.frame(summary(fit_assess_forest, time = 730)$strata)
time_2yr <- data.frame(summary(fit_assess_forest, time = 730)$time)
surv_prob_2yr <- data.frame(summary(fit_assess_forest, time = 730)$surv)

# Create new data frames for the survival probabilities by forest at 1 and 2 years
# Maybe just 1 data frame

forest_surv <- data.frame(forest_id_1yr, time_1yr, surv_prob_1yr, time_2yr, surv_prob_2yr) 

forests_1yr <- data.frame(forest_id_1yr, surv_prob_1yr) %>%
  rename(FOREST_ID = summary.fit_assess_forest..time...365..strata, 
         SURV = summary.fit_assess_forest..time...365..surv)
forests_2yr <- data.frame(FOREST_ID = forest_id_2yr, SURV = surv_prob_2yr) %>%
  rename(FOREST_ID = summary.fit_assess_forest..time...730..strata, 
         SURV = summary.fit_assess_forest..time...730..surv)

# n
forests_1yr$FOREST_ID <- str_remove(forests_1yr$FOREST_ID, "FOREST_ID=")
forests_2yr$FOREST_ID <- str_remove(forests_2yr$FOREST_ID, "FOREST_ID=")


#forests_2yr$FOREST_ID <- str_pad(probs_df_dn$REGION, 2, pad = "0")


