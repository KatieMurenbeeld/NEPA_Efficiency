

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
time_1yr <- summary(fit_assess_forest, time = 365)$time
surv_prob_1yr <- summary(fit_assess_forest, time = 365)$surv

forest_id_2yr <- data.frame(summary(fit_assess_forest, time = 730)$strata)
time_2yr <- summary(fit_assess_forest, time = 730)$time
surv_prob_2yr <- summary(fit_assess_forest, time = 730)$surv

