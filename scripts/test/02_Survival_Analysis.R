

library(tidyverse)
library(broom)
library(survminer)
library(survival)

df <- read_csv("data/processed/pals_2009.csv")
# Note to self: may need to save REGION_ID and FOREST_ID as characters not numeric in 01_*.R

fit_assess_all <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ 1, data = df)
fit_assess_summary <- fit_assess_all %>% tidy()

fit_assess_forest <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ FOREST_ID, data = df)
fit_forest_summary <- fit_assess_forest %>% tidy()

