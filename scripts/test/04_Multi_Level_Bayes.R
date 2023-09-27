library(rstanarm)
library(rstan)
library(tidybayes)
library(tidyr)

df <- read.csv('data/processed/long_nepa_count.csv')
df$FOREST_ID <- as.factor(df$FOREST_ID)
df$region <- as.factor(df$region) 

df <- df %>% 
  mutate(less1_count = replace_na(less1_count, 0))

mod_1yr <- stan_glmer(cbind(longer1_count, less1_count)~FOREST_ID + (1|region), 
                   data=df,
                   family="binomial")
